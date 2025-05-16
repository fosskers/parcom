(defpackage parcom/xml
  (:use :cl)
  (:import-from :parcom #:<*> #:<* #:*> #:<$)
  (:local-nicknames (#:p #:parcom))
  ;; --- Entry --- ;;
  (:export #:parse #:xml))

(in-package :parcom/xml)

(defun parse (input)
  "Attempt to parse a whole XML document."
  (p:parse #'xml input))

;; TODO: Incomplete.
(defun xml (offset)
  "Parser: Parse an entire XML document into a Hash Table."
  (funcall (p:alt #'comment) offset))

(defun comment (offset)
  "Parser: A comment tag."
  (funcall (p:between (p:string "<!-- ")
                      (p:take-until (p:string " -->"))
                      (p:string " -->"))
           offset))

#+nil
(comment (p:in "<!-- hello -->"))
