(in-package :cl-posixre)

;; /* --------------------------------------------
;;    Extended Regular Expression
;;    --------------------------------------------
;; */
;; extended_reg_exp   :                      ERE_branch
;;                    | extended_reg_exp '|' ERE_branch
;;                    ;
;; ERE_branch         :            ERE_expression
;;                    | ERE_branch ERE_expression
;;                    ;
;; ERE_expression     : one_char_or_coll_elem_ERE
;;                    | '^'
;;                    | '$'
;;                    | '(' extended_reg_exp ')'
;;                    | ERE_expression ERE_dupl_symbol
;;                    ;
;; one_char_or_coll_elem_ERE  : ORD_CHAR
;;                    | QUOTED_CHAR
;;                    | '.'
;;                    | bracket_expression
;;                    ;
;; ERE_dupl_symbol    : '*'
;;                    | '+'
;;                    | '?'
;;                    | '{' DUP_COUNT               '}'
;;                    | '{' DUP_COUNT ','           '}'
;;                    | '{' DUP_COUNT ',' DUP_COUNT '}'
;;                    ;

;; /* --------------------------------------------
;;    Bracket Expression
;;    -------------------------------------------
;; */
;; bracket_expression : '[' matching_list ']'
;;                | '[' nonmatching_list ']'
;;                ;
;; matching_list  : bracket_list
;;                ;
;; nonmatching_list : '^' bracket_list
;;                ;
;; bracket_list   : follow_list
;;                | follow_list '-'
;;                ;
;; follow_list    :             expression_term
;;                | follow_list expression_term
;;                ;
;; expression_term : single_expression
;;                | range_expression
;;                ;
;; single_expression : end_range
;;                | character_class
;;                | equivalence_class
;;                ;
;; range_expression : start_range end_range
;;                | start_range '-'
;;                ;
;; start_range    : end_range '-'
;;                ;
;; end_range      : COLL_ELEM_SINGLE
;;                | collating_symbol
;;                ;
;; collating_symbol : Open_dot COLL_ELEM_SINGLE Dot_close
;;                | Open_dot COLL_ELEM_MULTI Dot_close
;;                | Open_dot META_CHAR Dot_close
;;                ;
;; equivalence_class : Open_equal COLL_ELEM_SINGLE Equal_close
;;                | Open_equal COLL_ELEM_MULTI Equal_close
;;                ;
;; character_class : Open_colon class_name Colon_close

(defstruct (lexer (:constructor make-lexer-aux))
  (str-exp "" :type string :read-only t)
  (str-len 0 :type fixnum :read-only t)
  (cur-tok #\Nul :type character :read-only t)
  (next-pos 0 :type fixnum :read-only t))

(defun make-lexer (str-exp)
  ;; advance the freshly minted lexer once to 'start' it
  ;; cur-token will be ready to be consumed once done
  (lexer-adv (make-lexer-aux :str-exp str-exp :str-len (length str-exp))))

;; the cur-token represents the next token to be consumed
;; parse functions assume that cur-token is the next to be
;; 'analyzed' token
(defun lexer-adv (lex)
  (with-slots (str-exp str-len cur-tok next-pos) lex
    (make-lexer-aux
     :str-exp str-exp
     :str-len str-len
     :cur-tok (if (>= next-pos str-len)
		  #\Nul
		  (schar str-exp next-pos))
     :next-pos (1+ next-pos))))

(defun eol-p (lex)
  (> (lexer-next-pos lex) (lexer-str-len lex)))

(defun char-num-p (char)
  (let ((tok-val (char-code char)))
    (and (>= tok-val (char-code #\0))
	 (<= tok-val (char-code #\9)))))

(defun char-to-int (char)
  (if (char-num-p char)
      (- (char-code char) (char-code #\0))
      nil))

(defun parse-top-level (lex)
  "parse a top level expression, handles alternation"
  (flet ((exp-end-p (lex)
	   ;; end of expression is either eol or ')'
	   (or (eol-p lex)
	       (char= (lexer-cur-tok lex) #\)))))

    (if (exp-end-p lex)
	(values nil lex)
	;; parse the first expression
	(multiple-value-bind (left-exp lex*) (parse-branch lex)
	  (cond
	    ;; check for end of expression
	    ((exp-end-p lex*)
	     (values left-exp lex*))

	    ;; otherwise we should encounter an alternation
	    ((char= (lexer-cur-tok lex*) #\|)
	     (multiple-value-bind (right-exp lex**)
		 (parse-top-level (lexer-adv lex*))
	       (values (make-instance 'exp-alt :left left-exp :right right-exp)
		       lex**)))

	    (t (throw-parse-error lex*)))))))

(defun parse-branch (lex)
  "parse a string of concatenated expressions. we stop when we encounter the end
of line or an alternation (|)."
  (flet ((exp-end-p (lex)
	   ;; end of expression is either eol, '|', or ')'
	   (with-slots (cur-tok) lex
	     (or (eol-p lex)
		 (char= cur-tok #\|)
		 (char= cur-tok #\))))))

    (if (exp-end-p lex)
	(values nil lex)

	;; parse the first expression element
	(multiple-value-bind (left-exp lex*) (parse-rep lex)
	  (if (exp-end-p lex*)
	      (values left-exp lex*)

	      ;; parse another if there's more
	      (multiple-value-bind (right-exp lex**) (parse-branch lex*)
		(values (make-instance 'exp-concat
				       :left left-exp :right right-exp)
			lex**)))))))

(defun parse-exp (lex)
  (case (lexer-cur-tok lex)
    ((#\() ; parenthesis sub expression
     (parse-sub-exp lex))
    ((#\[) ; bracket expression
     (parse-bracket lex))
    ((#\\) ; escaped character or backreference
     (parse-escape lex))
    ((#\^) (values '(:anchor :start) (lexer-adv lex)))
    ((#\$) (values '(:anchor :end) (lexer-adv lex)))

    ;; char assumed ordinary if we reach here
    (otherwise
     (parse-char lex))))

;; NOTE: may need to introduce a new sub expression type
(defun parse-sub-exp (lex)
  "parses a sub-expression, an expression within '(' and ')', ie. (abc), also
known as a capture group."
  (multiple-value-bind (sub-exp lex*)
      ;; consume '('
      (parse-top-level (lexer-adv lex))
    (case (lexer-cur-tok lex*)
      ((#\)) (values sub-exp (lexer-adv lex*)))
      ;; we expect to see ')'
      (otherwise (throw-parse-error lex*)))))

(defun parse-escape (lex) (values nil lex))

(defun parse-bracket (lex) (values nil lex))

(defun parse-char (lex)
  "parses a single character expression"
  (values (make-instance 'atom-char :val (lexer-cur-tok lex)) (lexer-adv lex)))

(defun parse-rep (lex)
  (multiple-value-bind (nonrep-exp lex*) (parse-exp lex)
    (multiple-value-bind (range lex**) (parse-rep-symbol lex*)
      (if range
	  (case (lexer-cur-tok lex**)
	    ((#\?)
	     (values (make-instance 'exp-rep
				    :greedy-p nil
				    :range range
				    :sub nonrep-exp)
		     (lexer-adv lex**)))
	    (otherwise
	     (values (make-instance 'exp-rep
				    :greedy-p t
				    :range range
				    :sub nonrep-exp)

		     lex**)))
	  (values nonrep-exp lex**)))))

(defun parse-rep-symbol (lex)
  (case (lexer-cur-tok lex)
    ((#\*) ; 0 or more quantifier
     (values '(0 nil) (lexer-adv lex)))
    ((#\+) ; 1 or more quantifier
     (values '(1 nil) (lexer-adv lex)))
    ((#\?) ; 0 or 1 quantifier
     (values '(0 1) (lexer-adv lex)))
    ((#\{) ; range quantifier
     (parse-range-rep lex))
    (otherwise (values nil lex))))

(defun parse-range-rep (lex)
  (multiple-value-bind (min-rep lex*) (parse-rep-val (lexer-adv lex))
    (case (lexer-cur-tok lex*)
      ;; parse a second value, for the case {m,n}
      ((#\,)
       (multiple-value-bind (max-rep lex**)
	   (parse-rep-val (lexer-adv lex*))
	 ;; we expect to see a '}'
	 (case (lexer-cur-tok lex**)
	   ((#\})
	    (if (> min-rep max-rep)
		(throw-rep-val-invalid-error (lexer-adv lex*) min-rep max-rep)
		(values (list min-rep max-rep) (lexer-adv lex**))))
	   (otherwise (throw-parse-error lex**)))))
      ;; the range is a single value, as in the case {n}
      ((#\}) (values (list min-rep min-rep) (lexer-adv lex*)))
      (otherwise (throw-parse-error lex*)))))

(defun parse-rep-val (lex)
  (labels ((parse-num (acc lex)
	   (with-slots (cur-tok) lex
	     (if (or (eol-p lex)
		     (not (char-num-p cur-tok)))
		 (values acc lex)
		 (parse-num (+ (* 10 acc)
			       (char-to-int cur-tok))
			    (lexer-adv lex))))))
    
    (multiple-value-bind (val lex*) (parse-num 0 lex)
      (if (or (< val +re-dup-min+)
	      (> val +re-dup-max+))
	  ;; report error with the original lex
	  (throw-rep-val-oob-error lex val)
	  (values val lex*)))))

;; generates a parse tree from a string expression
(defun parse (str-exp)
  (parse-top-level (make-lexer str-exp)))
