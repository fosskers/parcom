(defpackage parcom/xml
  (:use :cl)
  (:import-from :parcom #:<*> #:<* #:*> #:<$)
  (:local-nicknames (#:p #:parcom))
  ;; --- Types --- ;;
  (:export #:element #:element-content #:element-metadata #:content)
  ;; --- Entry --- ;;
  (:export #:parse #:xml))

(in-package :parcom/xml)

;; --- Types --- ;;

(defstruct element
  "The content of an element, alongside any metadata its opening tag may have
carried."
  (content  ""  :type (or string hash-table))
  (metadata nil :type (or null hash-table)))

(defgeneric content (element)
  (:documentation "Fetch the inner `content' of an element."))

(defmethod content ((element element))
  (element-content element))

(defmethod content ((element string))
  element)

(defmethod content ((elements hash-table))
  elements)

;; --- Entry --- ;;

(defun parse (input)
  "Attempt to parse a whole XML document."
  (p:parse #'xml input))

;; TODO: Incomplete.
(defun xml (offset)
  "Parser: Parse an entire XML document into a Hash Table."
  (funcall (*> #'skip-space-and-comments
               (<*> #'document-type
                    #'elements))
           offset))

#+nil
(p:parse #'xml (uiop:read-file-string "tests/data/java.pom"))

;; --- Parsers --- ;;

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
  (funcall (<*> (p:take-while1 (lambda (c) (not (or (eql #\= c) (eql #\> c)))))
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
<farewell hi=\"there\">bye!</farewell>
<!-- comment -->
")

#+nil
(p:parse #'elements "<farewell meta=\"hi\">bye!</farewell>")

(defun element (offset)
  "Parser: Some basic element with character contents."
  (multiple-value-bind (res next) (open-tag offset)
    (if (p:failure? res)
        (p:fail next)
        (multiple-value-bind (name meta)
            (etypecase res
              (simple-string (values res nil))
              (cons (values (car res) (cdr res))))
          (p:fmap (lambda (content)
                    (if (not meta)
                        (cons name content)
                        (cons name (make-element :content content :metadata meta))))
                  (funcall (<* (*> #'skip-space-and-comments
                                   (p:alt #'elements
                                          (p:take-while (lambda (c) (not (or (eql #\< c) (eql c #\newline)))))))
                               #'skip-space-and-comments
                               (close-tag name))
                           next))))))

#+nil
(p:parse #'element "<greeting foo=\"bar\">hi!</greeting>")
#+nil
(p:parse #'element "<greeting>hi!</greeting>")
#+nil
(p:parse #'element "<phrases><greeting>hi!</greeting><farewell>bye!</farewell></phrases>")

(defun open-tag (offset)
  "Parser: The <foo> part of an element."
  (p:fmap (lambda (two)
            (destructuring-bind (name meta) two
              (if (not meta)
                  name
                  (let ((ht (make-hash-table :test #'equal)))
                    (dolist (pair meta)
                      (setf (gethash (car pair) ht) (cadr pair)))
                    (cons name ht)))))
          (funcall (p:between (p:char #\<)
                              (<*> (p:pmap #'simplify-string
                                           (p:take-while1 (lambda (c)
                                                            (not (or (eql c #\>)
                                                                     (eql c #\space))))))
                                   (p:opt (*> (p:char #\space)
                                              #'skip-space
                                              (p:sep-end1 #'skip-space #'pair))))
                              (p:char #\>))
                   offset)))

#+nil
(open-tag (p:in "<greeting>"))
#+nil
(p:parse #'open-tag "<greeting foo=\"bar\" baz=\"zoo\">")

(defun close-tag (label)
  (lambda (offset)
    (declare (optimize (debug 3)))
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
