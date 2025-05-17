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

(defun pair (offset)
  "Parser: Some key-value pair. Tag metadata?"
  (funcall (<*> (p:take-while (lambda (c) (not (eql #\= c))))
                (*> (p:char #\=)
                    (p:between (p:char #\")
                               (p:take-while (lambda (c) (not (eql #\" c))))
                               (p:char #\"))))
           offset))

#+nil
(pair (p:in "version=\"1.0\""))

(defun document-type (offset)
  "Parser: The version, etc., declarations at the top of the document."
  (p:fmap (lambda (pairs)
            (let ((ht (make-hash-table :test #'equal)))
              (dolist (pair pairs)
                (setf (gethash (car pair) ht) (cadr pair)))
              ht))
          (funcall (p:between (p:string "<?xml ")
                              (p:sep-end1 #'skip-space #'pair)
                              (p:string "?>"))
                   offset)))

#+nil
(p:parse #'document-type "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")

(defun skip-space (offset)
  "A faster variant of `space' that just advances over whitespace chars."
  (funcall (p:consume (lambda (c) (or (equal c #\space) (equal c #\tab))))
           offset))
