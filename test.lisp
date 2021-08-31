(+ 1 2)
(ql:quickload :cl-ppcre)

(defvar x 3)
(defparameter s (cl-ppcre:create-scanner "abc"))
(+ 1 x)
  
;; scan returns 4 values if a match is found, else nil
;; the first two describe the substring match position [start, end)
;; the third and fourth are two vectors that describe the starts and ends of each
;; capture group. they are always equal in length and empty if no capture groups are used
(cl-ppcre:scan "(a)*b" "xaaabd")
(cl-ppcre:scan "(a)*b" "xaaabd" :start 1)
(cl-ppcre:scan '(:greedy-repetition 0 nil #\b) "bbbc")
(cl-ppcre:scan-to-strings "[^b]*b" "aaabd")
(cl-ppcre:scan-to-strings "([^b])*b" "aaabd")
(cl-ppcre:scan-to-strings "(([^b])*)b" "aaabd")

;; error
(cl-ppcre:scan-to-strings "[3-1]{3}" "aaa324bd")

;; nested anchoring
(cl-ppcre:scan-to-strings "(he(l(l))o)" "hello world")

;; we need to escape the backslash
(cl-ppcre:scan "^(.*) \\1$" "hello hello")

;; 9.3.6 example
(cl-ppcre:scan-to-strings "(ab*)*" "abbab")    ; captures "ab"
(cl-ppcre:scan-to-strings "(ab*)*" "ababb")    ; captures "abb"
(cl-ppcre:scan-to-strings "(ab*)*\\1" "abbab") ; doesn't match

(cl-ppcre:scan-to-strings "a{3,5}" "aaaaaaa")  ; returns the max length match
(cl-ppcre:scan-to-strings "a{0,5}" "b")        ; matches ""
(cl-ppcre:scan-to-strings "a{5,}" "aaaaaa")    ; matches 5 or more a's

(cl-ppcre:scan-to-strings "a*" "b")            ; matches ""

;; alternation
(cl-ppcre:scan-to-strings "a((bc)|d)" "abcd")            ; matches ""

;; anchors
(cl-ppcre:scan-to-strings "^.*$" "abc")

;; escape the middle dollar sign with \\$, can't use it otherwise
(cl-ppcre:scan-to-strings "^\\$$" "$asdjlfaksd")

;; bracket expression for collation
;; the first "]" after the opening bracket has no special meaning
(cl-ppcre:scan-to-strings "[]]" "]") ; matches "]"
(cl-ppcre:scan-to-strings "[^]]" "b") ; matches anything but "]"
(cl-ppcre:scan-to-strings "ab]" "ab]") ; right bracket has no special meaning alone
(cl-ppcre:scan-to-strings "[a]]" "a]") ; second right bracket has not special meaning

;; character classes
(cl-ppcre:scan-to-strings "[:alpha:]" "b") ; doesn't seem to work

;; when the end range point is also a start range point, the interpretation is undefined
(cl-ppcre:scan-to-strings "[0-3-7]" "-") ; the second hyphen is interpreted literally here
(cl-ppcre:scan-to-strings "[%--]" "-") ; hyphen can be used as a range point
(cl-ppcre:scan-to-strings "[--0]" "-") ; hyphen must come first in expression if used as starting point
(cl-ppcre:scan-to-strings "[a--@]" "-") ; invalid range error? how does ppcre define these ranges?
(cl-ppcre:scan-to-strings "hello$" "hello\nworld") ; invalid range error? how does ppcre define these ranges?

(cl-ppcre:scan-to-strings "|" "") ; matches empty string

(cl-ppcre:scan-to-strings "\\)" "hello)")

(cl-ppcre:scan-to-strings "(a*)*" "ab") ; this works
(cl-ppcre:scan-to-strings "(a*)*" "ab") ; but this doesn't?

(cl-ppcre:scan-to-strings "|a" "a")     ; alternation on the left is empty string, so that's what matches
