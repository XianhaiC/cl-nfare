(in-package cl-posixre)

(defun throw-parse-error (lex)
  (with-slots (cur-tok next-pos) lex
    (error
     (format nil "position [~a]: unexpected token ~a"
	     (1- next-pos) cur-tok))))

(defun throw-rep-val-oob-error (lex val)
  (with-slots (next-pos) lex
    (error
     (format
      nil
      "position [~a]: invalid range value ~a, must be within ~a and ~a"
      (1- next-pos) val
      +re-dup-min+ +re-dup-max+))))

(defun throw-rep-val-invalid-error (lex min max)
  (with-slots (next-pos) lex
    (error
     (format
      nil
      "position [~a]: invalid repetition value ~a, cannot be less than ~a"
      (1- next-pos) max min))))
