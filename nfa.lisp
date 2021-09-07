(in-package :cl-posixre)

(defclass state ()
  () (:documentation "base NFA state node"))

(defclass state-atom (state)
  ((val :initarg :val :accessor val :type exp-atom
	 :documentation "the atom exp that this state holds")
   (out :initarg :out :accessor out :type state
	:documentation "the next node"))
  (:documentation "atom expression node"))

(defclass state-split (state)
  ((left :initarg :left :accessor left :type state
	 :documentation "the left sub node")
   (right :initarg :right :accessor right :type state
	  :documentation "the right sub node"))
  (:documentation "split node for nondeterministic jumps"))

(defclass state-match (state)
  () (:documentation "terminal node"))

(defgeneric nfa-gen (exp leaf)
  (:documentation "generates an NFA graph for the corresponding expression,
leaf parameter aids in recursive graph construction"))

(defmethod nfa-gen ((exp exp-alt) leaf)
  (make-instance 'state-split
		 :left (nfa-gen (left exp) leaf)
		 :right (nfa-gen (right exp) leaf)))

(defmethod nfa-gen ((exp exp-concat) leaf)
  (let* ((right-root (nfa-gen (right exp) leaf))
	 (left-root (nfa-gen (left exp) right-root)))
    left-root))

(defmethod nfa-gen ((exp exp-rep) leaf))

(defmethod nfa-gen ((exp exp-atom) leaf)
  (make-instance 'state-atom :val exp :out leaf))

(defun nfa (exp)
  "convert an expression AST to its corresponding NFA graph"
  ;; supply a terminal node
  (nfa-gen exp (make-instance 'state-match)))
