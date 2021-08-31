(in-package :cl-posixre)

(defun scan (exp string)
  ;; takes in an expression and a string and performs a match
  ;; this should return the earliest substring that matches
  )

;; TODO: NFA
;; we need to create a graph that represents the NFA from a given expression
;; the following tokens we can expect are
;; - character literal
;; - alternation
;; - concatenation
;; - zero or one
;; - zero or more
;; - one or more
;; in order to parse the expression string properly, we'll need to first convert it to a post-fix notation,
;; where we use a stack to house intermediate expression tokens.
;; alternation and concatenation are binary operators that act on the previous two operands in a stack

(defun convert-to-post-fix (exp))

(defun generate-nfa (exp-post))
