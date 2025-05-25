(defpackage parcom/xml
  (:use :cl)
  (:import-from :parcom #:<*> #:<* #:*> #:<$)
  (:local-nicknames (#:p #:parcom))
  ;; --- Types --- ;;
  (:export #:element #:element-name #:element-content #:element-metadata #:content)
  ;; --- Entry --- ;;
  (:export #:parse #:xml))

(in-package :parcom/xml)

;; --- Static Parsers --- ;;

(defparameter +comment-open+   (p:string "<!--"))
(defparameter +comment-close+  (p:string "-->"))
(defparameter +tag-close+      (p:string "</"))
(defparameter +tag-start+      (p:char #\<))
(defparameter +tag-end+        (p:char #\>))
(defparameter +slash+          (p:char #\/))
(defparameter +quote+          (p:char #\"))
(defparameter +skip-space+     (p:consume (lambda (c) (or (equal c #\space) (equal c #\tab)))))
(defparameter +skip-all-space+ (p:consume #'p:space?))
(defparameter +skip-comments+  (p:skip (*> #'comment +skip-all-space+)))
(defparameter +skip-junk+      (*> +skip-all-space+ +skip-comments+))
(defparameter +peek-close+     (p:peek +tag-close+))
(defparameter +peek-no-slash+  (p:peek (p:any-but #\/)))
(defparameter +until-close+    (p:take-until +comment-close+))
(defparameter +comment+        (p:between +comment-open+ +until-close+ +comment-close+))

;; --- Types --- ;;

(defstruct document
  "The entire XML document."
  (metadata nil :type (or null hash-table))
  (element  nil :type element))

(defstruct element
  "The content of an element, alongside any metadata its opening tag may have
carried."
  (name     ""  :type p::char-string)
  (metadata nil :type (or null hash-table))
  (content  nil :type (or null string list hash-table)))

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

(declaim (ftype (function (fixnum) (values (or document (member :fail)) fixnum)) xml))
(defun xml (offset)
  "Parser: Parse an entire XML document into a Hash Table."
  (p:fmap (lambda (list)
            (destructuring-bind (metadata element) list
              (make-document :metadata metadata :element element)))
          (funcall (*> +skip-junk+
                       (<*> (p:opt #'document-type)
                            (*> +skip-junk+ #'element)))
                   offset)))

#+nil
(p:parse #'xml (uiop:read-file-string "tests/data/java.pom"))

;; --- Parsers --- ;;

(declaim (ftype (function (fixnum) (values (or cl:string (member :fail)) fixnum)) comment))
(defun comment (offset)
  "Parser: A comment tag."
  (funcall +comment+ offset))

#+nil
(comment (p:in "<!-- hello -->"))
#+nil
(p:parse #'comment "<!--
  Hello!
-->
")

(declaim (ftype (function (fixnum) (values (or list (member :fail)) fixnum)) pair))
(defun pair (offset)
  "Parser: Some key-value pair. Tag metadata?"
  (funcall (<*> (p:take-while1 (lambda (c) (not (or (eql #\= c) (eql #\> c)))))
                (*> (p:char #\=)
                    (p:between +quote+
                               (p:take-while (lambda (c) (not (eql #\" c))))
                               +quote+
                               :id :xml-pair)))
           offset))

#+nil
(pair (p:in "version=\"1.0\""))

(declaim (ftype (function (fixnum) (values (or hash-table (member :fail)) fixnum)) elements))
(defun elements (offset)
  "Parser: A linear series of elements parsed into a Hash Table."
  (p:fmap (lambda (els)
            (let ((ht (make-hash-table :test #'equal)))
              (dolist (el els)
                (let* ((name (element-name el))
                       (got? (gethash name ht)))
                  (cond
                    ((not got?) (setf (gethash name ht) el))
                    ;; Subelements can share the same name, in which case they
                    ;; need to be grouped into a list.
                    ((listp got?) (setf (gethash name ht) (cons el got?)))
                    ;; Similar to the case above, here we found a key collision,
                    ;; but a list hasn't been started yet, so we start one.
                    (t (setf (gethash name ht) (list el got?))))))
              ht))
          (funcall (p:sep-end1 +skip-junk+ #'element) offset)))

#+nil
(p:parse #'elements "<greeting>hi!</greeting>
<!-- comment -->
<farewell hi=\"there\">bye!</farewell>
<!-- comment -->
")

(declaim (ftype (function (fixnum) (values (or element (member :fail)) fixnum)) element))
(defun element (offset)
  "Parser: Some basic element with character contents."
  (multiple-value-bind (res next) (open-tag offset)
    (cond ((p:failure? res) (p:fail next))
          ;; The opening tag was self-closing.
          ((element-p res) (values res next))
          ;; We need to go deeper.
          (t (multiple-value-bind (name meta)
                 (etypecase res
                   (simple-string (values res nil))
                   (cons (values (car res) (cdr res))))
               (p:fmap (lambda (content)
                         (make-element :name name :content content :metadata meta))
                       (funcall (<* (*> +skip-junk+
                                        (p:alt
                                         ;; Having this sneaky case here allows us to
                                         ;; assert `take-while1' below. Look
                                         ;; carefully; `sep-end' and `take-while'
                                         ;; would otherwise form an infinite loop.
                                         (<$ "" +peek-close+)
                                         #'elements
                                         ;; Preemptively unwrap a single-element list
                                         ;; so that it yields just the underlying
                                         ;; string.
                                         (p:pmap (lambda (list) (if (null (cdr list)) (car list) list))
                                                 (p:sep-end1 +skip-junk+
                                                             (p:take-while1 (lambda (c) (not (or (eql #\< c) (eql #\newline c)))))))))
                                    +skip-junk+
                                    (close-tag name))
                                next)))))))

#+nil
(p:parse #'element "<greeting foo=\"bar\">hi!</greeting>")
#+nil
(p:parse #'element "<greeting>hi!</greeting>")
#+nil
(p:parse #'element "<phrases><greeting>hi!</greeting><farewell>bye!</farewell></phrases>")
#+nil
(p:parse #'element "<greeting foo=\"bar\"/>")
#+nil
(p:parse #'element "<greeting/>")

(defparameter +open-tag+
  (p:between (*> +tag-start+ +peek-no-slash+)
             (<*> (p:consume (lambda (c)
                               (not (or (p:space? c)
                                        (eql c #\>)
                                        (eql c #\/)))))
                  (p:opt (*> (p:any-if #'p:space?)
                             +skip-all-space+
                             (p:sep-end1 +skip-all-space+ #'pair)))
                  (p:opt (*> +skip-space+ +slash+)))
             +tag-end+))

#+nil
(p:parse #'open-tag "<project
  xmlns=\"http://maven.apache.org/POM/4.0.0\"
  xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
  xsi:schemaLocation=\"http://maven.apache.org/POM/4.0.0 http://maven.apache.org/maven-v4_0_0.xsd\">
<greeting>hi</greeting>
</project>")

(declaim (ftype (function (fixnum) (values (or element cons p::char-string (member :fail)) fixnum)) open-tag))
(defun open-tag (offset)
  "Parser: The <foo> part of an element. If shaped like <foo/> it is in fact
standalone with no other content, and no closing tag."
  (multiple-value-bind (res next)
      (funcall +open-tag+ offset)
    (if (p:failure? res)
        (p:fail next)
        (destructuring-bind (consumed meta slash) res
          (let ((meta (when meta
                        ;; TODO: 2025-05-20 Abstract this out.
                        (let ((ht (make-hash-table :test #'equal)))
                          (dolist (pair meta)
                            (setf (gethash (car pair) ht) (cadr pair)))
                          ht)))
                (name (p::direct-copy p::*input* (1+ offset) consumed)))
            (cond
              ;; This was a self-closing, standalone tag with no other
              ;; content. We yield a completed `element' as a signal to the
              ;; caller that they shouldn't attempt to parse anything
              ;; deeper.
              (slash (p:ok next (make-element :name name :content nil :metadata meta)))
              ;; There's more to parse within this element, but for now we
              ;; also found some metadata.
              (meta (p:ok next (cons name meta)))
              ;; It was just a simple named tag.
              (t (p:ok next name))))))))

#+nil
(open-tag (p:in "<greeting>"))
#+nil
(p:parse #'open-tag "<greeting foo=\"bar\" baz=\"zoo\">")
#+nil
(p:parse #'open-tag "<greeting foo=\"bar\" baz=\"zoo\"/>")
#+nil
(p:parse #'open-tag "<organization />")

(declaim (ftype (function (p::char-string) (function (fixnum) (values (or p::char-string (member :fail)) fixnum))) close-tag))
(defun close-tag (label)
  (lambda (offset)
    (funcall (p:between +tag-close+
                        (p:string label)
                        +tag-end+)
             offset)))

#+nil
(p:parse (close-tag "greeting") "</greeting>")

(declaim (ftype (function (fixnum) (values (or hash-table (member :fail)) fixnum)) document-type))
(defun document-type (offset)
  "Parser: The version, etc., declarations at the top of the document."
  (p:fmap (lambda (pairs)
            (let ((ht (make-hash-table :test #'equal)))
              (dolist (pair pairs)
                (setf (gethash (car pair) ht) (cadr pair)))
              ht))
          (funcall (p:between (p:string "<?xml ")
                              (p:sep-end1 +skip-space+ #'pair)
                              (p:string "?>")
                              :id :xml-document-type)
                   offset)))

#+nil
(p:parse #'document-type "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
