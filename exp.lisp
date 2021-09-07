(in-package :cl-posixre)

(defclass exp-base ()
  ()
  (:documentation "an ast node that represents a regular expression"))

(defclass exp-alt (exp-base)
  ((left :initarg :left
	 :accessor left
	 :type exp-base
	 :documentation "left subexpression")
   (right :initarg :right
	  :accessor right
	  :type exp-base
	  :documentation "right subexpression"))
  (:documentation "alternation node, ie. 'a|b'"))

(defclass exp-concat (exp-base)
  ((left :initarg :left
	 :accessor left
	 :type exp-base
	 :documentation "left subexpression")
   (right :initarg :right
	  :accessor right
	  :type exp-base
	  :documentation "right subexpression"))
  (:documentation "concatenation node, ie. 'ab'"))

(defclass exp-rep (exp-base)
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
	:type exp-base
	:documentation "subexpression that is repeated"))
  (:documentation "repetition node, ie. 'a*'"))
