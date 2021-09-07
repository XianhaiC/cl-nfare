(in-package :cl-posixre)

(defclass exp-atom (exp-base)
  ()
  (:documentation "represents an atomic expression pattern, such as a single
 char, a bracket expression, a character class, or a wildcard"))

(defgeneric match-p (exp char)
  (:documentation "check if the char matches the atom"))

(defclass atom-char (exp-atom)
  ((val :initarg :val
	:accessor val
	:type standard-char
	:documentation "char value from str"))
  (:documentation "an atomic char expression"))

(defmethod match-p ((exp atom-char) char)
  (char= (val exp) char))

(defclass atom-bracket (exp-atom)
  ((negate-p :initarg :negate-p
	     :accessor negate-p
	     :type boolean
	     :documentation "flag for negating match")
   (match-set :initarg :match-set
	      :accessor match-set
	      :type list
	      :documentation "set of chars that match"))
  (:documentation "an atomic bracket expression, ie. '[abc]'"))

(defmethod match-p ((exp atom-bracket) char)
  (with-accessors ((negate-p negate-p)
		   (match-set match-set)) exp
    (if negate-p
	(not (member char match-set))
	(member char match-set))))

(defclass atom-wildcard (exp-atom)
  ()
  (:documentation "an atomic wildcard expression, ie. '.'"))

(defmethod match-p ((exp atom-wildcard) char) t)

(defclass atom-char-class (exp-atom)
  ((char-class :initarg :char-class
	       :accessor char-class
	       :type keyword
	       :documentation "the char class to match in"))
  (:documentation "an atomic character class expression, ie. '\d'"))

;; TODO
(defmethod match-p ((exp atom-char-class) char) t)
