(in-package :cl-posixre-test)

(defvar *this-file* (load-time-value
                     (or #.*compile-file-pathname* *load-pathname*))
  "The location of this source file.")

(defmacro do-tests ((name &optional show-progress-p) &body body)
  "Helper macro which repeatedly executes BODY until the code in body
calls the function DONE.  It is assumed that each invocation of BODY
will be the execution of one test which returns NIL in case of success
and list of string describing errors otherwise.

The macro prints a simple progress indicator \(one dots for ten tests)
to *STANDARD-OUTPUT* unless SHOW-PROGRESS-P is NIL and returns a true
value iff all tests succeeded.  Errors in BODY are caught and reported
\(and counted as failures)."
  `(let ((successp t)
         (testcount 1))
     (block test-block
       (flet ((done ()
                (return-from test-block successp)))
         (format t "~&Test: ~A~%" ,name)
         (loop
          (when (and ,show-progress-p (zerop (mod testcount 10)))
            (format t ".")
            (when (zerop (mod testcount 100))
              (terpri))
            (force-output))
          (let ((errors
                 (handler-case
                     (progn ,@body)
                   (error (msg)
                     (list (format nil "~&got an unexpected error: ~A" msg))))))
            (setq successp (and successp (null errors)))
            (when errors
              (format t "~&~4@A:~{~&   ~A~}~%" testcount errors))
            (incf testcount)))))
     successp))

(defun parser-tests-suite (&key (file-name
                           (make-pathname :name "parser"
                                          :type nil :version nil
                                          :defaults *this-file*))
                          verbose)
  "Loops through all the forms in the file FILE-NAME and executes each
of them using EVAL.  It is assumed that each FORM specifies a test
which returns a true value iff it succeeds.  Prints each test form to
*STANDARD-OUTPUT* if VERBOSE is true and shows a simple progress
indicator otherwise.  EXTERNAL-FORMAT is the FLEXI-STREAMS external
format which is used to read the file.  Returns a true value iff all
tests succeeded."
  (with-open-file (stream file-name)
    (let ((*package* (find-package :cl-posixre-test)))
      (do-tests ((format nil "Simple tests from file ~S" (file-namestring file-name))
                 (not verbose))
        (let ((form (or (read stream nil) (done))))
          (when verbose
            (format t "~&~S" form))
          (cond ((eval form) nil)
                (t (list (format nil "~S returned NIL" form)))))))))

(defun run-all-tests (&key verbose)
  "Runs all tests for CL-PPCRE and returns a true value iff all tests
succeeded.  VERBOSE is interpreted by the individual test suites.
MORE-TESTS can be a list of function designators designating
additional tests to run.  This facility is used by the tests for
CL-PPCRE-UNICODE."
  (let ((successp t))
    (macrolet ((run-test-suite (&body body)
                 `(unless (progn ,@body)
                    (setq successp nil))))
      ;; run the automatically generated Perl tests
      (run-test-suite (parser-tests-suite :verbose verbose)))
    (format t "~2&~:[Some tests failed~;All tests passed~]." successp)
    successp))
