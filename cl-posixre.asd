(in-package :cl-user)

(defpackage :cl-posixre-asd
  (:use :cl :asdf))

(in-package :cl-posixre-asd)

(defsystem :cl-posixre
    :version "1.0.0"
    :description "POSIX compliant regular expression library"
    :serial t
    :components ((:file "packages")
		 (:file "constants")
		 (:file "errors")
		 (:file "exp")
		 (:file "atom")
		 (:file "parser")))

(defsystem :cl-posixre/test
    :description "POSIX compliant regular expression library tests"
    :depends-on (:cl-posixre)
    :components ((:module "test"
			  :serial t
			  :components ((:file "packages")
				       (:file "test-helper")))))

(defmethod perform ((o test-op) (c (eql (find-system :cl-posixre))))
  (operate 'load-op :cl-posixre/test)
  (funcall (intern (symbol-name :run-all-tests) (find-package :cl-posixre-test))))
