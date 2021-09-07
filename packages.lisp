(in-package :cl-user)

(defpackage :cl-posixre
  (:nicknames :pre)
  (:use :cl)
  (:export :parse
	   :nfa))
