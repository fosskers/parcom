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

(defun elements (offset)
  "Parser: A linear series of elements parsed into a Hash Table."
  (p:fmap (lambda (els)
            (let ((ht (make-hash-table :test #'equal)))
              (dolist (pair els)
                (setf (gethash (car pair) ht) (cdr pair)))
              ht))
          (funcall (p:sep-end1 #'skip-space-and-comments #'element) offset)))

#+nil
(p:parse #'elements "<greeting>hi!</greeting>
<!-- comment -->
<farewell>bye!</farewell>
<!-- comment -->
")

(defun element (offset)
  "Parser: Some basic element with character contents."
  (multiple-value-bind (res next) (open-tag offset)
    (if (p:failure? res)
        (p:fail next)
        (p:fmap (lambda (s) (cons res s))
                (funcall (<* (*> #'skip-all-space
                                 (p:alt #'elements
                                        (p:take-while (lambda (c) (not (or (eql #\< c) (eql c #\newline)))))))
                             #'skip-all-space
                             (close-tag res))
                         next)))))

#+nil
(p:parse #'element "<greeting>hi!</greeting>")
#+nil
(p:parse #'element "<phrases><greeting>hi!</greeting><farewell>bye!</farewell></phrases>")

(defun open-tag (offset)
  "Parser: The <foo> part of an element."
  (funcall (p:between (p:char #\<)
                      (p:pmap #'simplify-string
                              (p:take-while1 (lambda (c) (not (eql c #\>)))))
                      (p:char #\>))
           offset))

#+nil
(p:parse #'open-tag "<greeting>")

(defun close-tag (label)
  (lambda (offset)
    ;; TODO: 2025-05-17 Make a lambda cache for string.
    (funcall (p:between (p:string "</")
                        (p:string label)
                        (p:char #\>))
             offset)))

#+nil
(p:parse (close-tag "greeting") "</greeting>")

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

(defun simplify-string (s)
  "Convert some general string into a simple-string."
  (let ((new (make-array (length s) :element-type 'character)))
    (replace new s)))

(defun skip-space (offset)
  "A faster variant of `space' that just advances over whitespace chars."
  (funcall (p:consume (lambda (c) (or (equal c #\space) (equal c #\tab))))
           offset))

(declaim (ftype (function (string) simple-string) simplify-string))
(defun skip-all-space (offset)
  "Like `skip-space' but consumes newlines as well."
  (funcall (p:consume #'p:space?) offset))

(defun skip-space-and-comments (offset)
  "Blows past all the stuff we don't care about."
  (funcall (*> #'skip-all-space
               (p:skip (*> #'comment #'skip-all-space)))
           offset))
