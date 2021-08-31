(in-package cl-posixre)

ldefun throw-parse-error (lex)
  (with-slots (cur-tok next-pos) lex
    (error
     (format nil "parse-top-level: unexpected token ~a at position ~a"
	     cur-tok (1- next-pos)))))
