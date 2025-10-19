(defpackage parcom/xml
  (:use :cl)
  (:import-from :parcom #:<* #:*> #:<$ #:fn #:-> #:maybe)
  (:local-nicknames (#:p #:parcom))
  ;; --- Types --- ;;
  (:export #:element #:element-name #:element-content #:element-metadata
           #:document #:document-metadata #:document-element
           #:content)
  ;; --- Entry --- ;;
  (:export #:parse #:xml))

(in-package :parcom/xml)

;; --- Static Parsers --- ;;

(defparameter +equal+          (p:char #\=))
(defparameter +quote+          (p:char #\"))
(defparameter +slash+          (p:char #\/))
(defparameter +tag-end+        (p:char #\>))
(defparameter +tag-start+      (p:char #\<))

(defparameter +comment-close+  (p:string "-->"))
(defparameter +comment-open+   (p:string "<!--"))
(defparameter +tag-close+      (p:string "</"))
(defparameter +meta-open+      (p:string "<?xml "))
(defparameter +meta-close+     (p:string "?>"))
(defparameter +doctype+        (p:string "<!DOCTYPE"))
(defparameter +system+         (p:string "SYSTEM"))

(defparameter +peek-close+     (p:peek +tag-close+))
(defparameter +peek-no-slash+  (p:peek (p:any-but #\/)))
(defparameter +until-close+    (p:take-until +comment-close+))
(defparameter +comment+        (p:between +comment-open+ +until-close+ +comment-close+))

(defparameter +skip-space+     (p:consume (lambda (c) (or (equal c #\space) (equal c #\tab)))))
(defparameter +skip-all-space+ (p:consume #'p:space?))
(defparameter +skip-comments+  (p:skip (*> +comment+ +skip-all-space+)))
(defparameter +skip-junk+      (*> +skip-all-space+ +skip-comments+))

;; --- Types --- ;;

(defstruct document
  "The entire XML document."
  (metadata nil :type (or null hash-table))
  (doctype  nil :type (or null doctype))
  (element  nil :type element))

(defstruct doctype
  "A `!DOCTYPE' tag."
  (type   nil :type string)
  (system nil :type string))

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

(defmethod content ((document document))
  (element-content (document-element document)))

;; --- Entry --- ;;

(fn parse (-> p::char-string document))
(defun parse (input)
  "Attempt to parse a whole XML document."
  (p:parse (<* #'xml +skip-junk+ #'p:eof)  input))

(fn xml (maybe document))
(defun xml (offset)
  "Parser: Parse an entire XML document into a Hash Table."
  (funcall (*> +skip-junk+
               (p:ap (lambda (metadata doctype element)
                       (make-document :metadata metadata :doctype doctype :element element))
                     (p:opt #'xml-metadata)
                     (*> +skip-junk+ (p:opt #'doctype))
                     (*> +skip-junk+ #'element)))
           offset))

#+nil
(p:parse #'xml (uiop:read-file-string "tests/data/java.pom"))

;; --- Parsers --- ;;

(fn pair (maybe cons))
(defun pair (offset)
  "Parser: Some key-value pair. Tag metadata?"
  (funcall (p:ap #'cons
                 (p:take-while1 (lambda (c) (not (or (char= #\= c) (char= #\> c)))))
                 (*> +equal+
                     (p:between +quote+
                                (p:take-while (lambda (c) (not (char= #\" c))))
                                +quote+)))
           offset))

#+nil
(pair (p:in "version=\"1.0\""))

(fn elements (maybe hash-table))
(defun elements (offset)
  "Parser: A linear series of elements parsed into a Hash Table."
  (funcall (p:ap (lambda (els)
                   (let ((ht (make-hash-table :test #'equal :size 32)))
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
                 (p:sep-end1 +skip-junk+ #'element))
           offset))

#+nil
(p:parse #'elements "<greeting>hi!</greeting>
<!-- comment -->
<farewell hi=\"there\">bye!</farewell>
<!-- comment -->
")

(fn element (maybe element))
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

#+nil
(p:parse #'open-tag "<project
  xmlns=\"http://maven.apache.org/POM/4.0.0\"
  xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
  xsi:schemaLocation=\"http://maven.apache.org/POM/4.0.0 http://maven.apache.org/maven-v4_0_0.xsd\">
<greeting>hi</greeting>
</project>")

(fn open-tag (maybe (or element cons p::char-string)))
(defun open-tag (offset)
  "Parser: The <foo> part of an element. If shaped like <foo/> it is in fact
standalone with no other content, and no closing tag."
  (funcall (p:between
            (*> +tag-start+ +peek-no-slash+)
            (p:ap (lambda (consumed meta slash)
                    (let ((meta (when meta
                                  ;; TODO: 2025-05-20 Abstract this out.
                                  (let ((ht (make-hash-table :test #'equal :size 32)))
                                    (dolist (pair meta)
                                      (setf (gethash (car pair) ht) (cdr pair)))
                                    ht)))
                          (name (p::direct-copy p::*input* (1+ offset) consumed)))
                      (cond
                        ;; This was a self-closing, standalone tag with no other
                        ;; content. We yield a completed `element' as a signal to the
                        ;; caller that they shouldn't attempt to parse anything
                        ;; deeper.
                        (slash (make-element :name name :content nil :metadata meta))
                        ;; There's more to parse within this element, but for now we
                        ;; also found some metadata.
                        (meta (cons name meta))
                        ;; It was just a simple named tag.
                        (t name))))
                  (p:consume (lambda (c)
                               (not (or (p:space? c)
                                        (eql c #\>)
                                        (eql c #\/)))))
                  (p:opt (*> (p:any-if #'p:space?)
                             +skip-all-space+
                             (p:sep-end1 +skip-all-space+ #'pair)))
                  (p:opt (*> +skip-space+ +slash+)))
            +tag-end+)
           offset))

#+nil
(open-tag (p:in "<greeting>"))
#+nil
(p:parse #'open-tag "<greeting foo=\"bar\" baz=\"zoo\">")
#+nil
(p:parse #'open-tag "<greeting foo=\"bar\" baz=\"zoo\"/>")
#+nil
(p:parse #'open-tag "<organization />")

;; FIXME: 2025-10-20 Consider using a cache for the label here to avoid repeated
;; allocations.
(fn close-tag (-> p::char-string (maybe p::char-string)))
(defun close-tag (label)
  (lambda (offset)
    (funcall (p:between +tag-close+
                        (p:string label)
                        +tag-end+)
             offset)))

#+nil
(p:parse (close-tag "greeting") "</greeting>")

(fn xml-metadata (maybe hash-table))
(defun xml-metadata (offset)
  "Parser: The version, etc., declarations at the top of the document."
  (funcall (p:ap (lambda (pairs)
                   (let ((ht (make-hash-table :test #'equal :size 16)))
                     (dolist (pair pairs)
                       (setf (gethash (car pair) ht) (cdr pair)))
                     ht))
                 (p:between +meta-open+
                            (p:sep-end1 +skip-space+ #'pair)
                            +meta-close+))
           offset))

#+nil
(p:parse #'xml-metadata "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")

(fn doctype (maybe doctype))
(defun doctype (offset)
  "Parse a `!DOCTYPE' block."
  (funcall (p:between (*> +doctype+ +skip-space+)
                      (p:ap (lambda (type system)
                              (make-doctype :type type :system system))
                            (p:take-while1 (lambda (c) (not (eql c #\space))))
                            (*> +skip-space+
                                +system+
                                +skip-space+
                                (p:between +quote+
                                           (p:take-while1 (lambda (c) (not (eql c #\"))))
                                           +quote+)))
                      +tag-end+)
           offset))

#+nil
(p:parse #'doctype "<!DOCTYPE supplementalData SYSTEM \"../../common/dtd/ldmlSupplemental.dtd\">")
