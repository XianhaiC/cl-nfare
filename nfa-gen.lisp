(in-package :cl-posixre)

(defstruct (state (:constructor make-state-aux))
  (atom nil :type exp-atom :read-only t)
  (out-a nil :type state :read-only t)
  (out-b nil :type state :read-only t)
  (state-type :atom :type keyword :read-only t))

(defstruct (fragment (:constructor make-fragment-aux))
  (state nil :type state :read-only t))


(defgeneric nfa-gen (ast
(defun nfa-gen (ast))



