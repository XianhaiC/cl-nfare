(in-package :cl-posixre)

(defclass exp ()
  ()
  (:documentation "an ast node that represents a regular expression"))

(defclass exp-alt (exp)
  ((left :initarg :left
	 :accessor left
	 :type exp
	 :documentation "left subexpression")
   (right :initarg :right
	  :accessor right
	  :type exp
	  :documentation "right subexpression"))
  (:documentation "alternation node, ie. 'a|b'"))

(defclass exp-concat (exp)
  ((left :initarg :left
	 :accessor left
	 :type exp
	 :documentation "left subexpression")
   (right :initarg :right
	  :accessor right
	  :type exp
	  :documentation "right subexpression"))
  (:documentation "concatenation node, ie. 'ab'"))

(defclass exp-rep (exp)
  ((greedy-p :initarg :greedy-p
	     :accessor greedy-p
	     :type boolean
	     :documentation "flag for match greediness")
   (range :initarg :range
	  :accessor range
	  :type list
	  :documentation "2 element list containing the min and max repetitions")
   (sub :initarg :sub
	:accessor sub
	:type exp
	:documentation "subexpression that is repeated"))
  (:documentation "repetition node, ie. 'a*'"))
