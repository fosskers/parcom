(defpackage parcom/xml
  (:use :cl)
  (:import-from :parcom #:<*> #:<* #:*> #:<$)
  (:local-nicknames (#:p #:parcom))
  ;; --- Types --- ;;
  (:export #:element #:element-name #:element-content #:element-metadata #:content)
  ;; --- Entry --- ;;
  (:export #:parse #:xml))

(in-package :parcom/xml)

;; --- Types --- ;;

(defstruct document
  "The entire XML document."
  (metadata nil :type (or null hash-table))
  (element  nil :type element))

(defstruct element
  "The content of an element, alongside any metadata its opening tag may have
carried."
  (name     ""  :type simple-string)
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

(defun xml (offset)
  "Parser: Parse an entire XML document into a Hash Table."
  (p:fmap (lambda (list)
            (destructuring-bind (metadata element) list
              (make-document :metadata metadata :element element)))
          (funcall (*> #'skip-space-and-comments
                       (<*> (p:opt #'document-type)
                            (*> #'skip-space-and-comments
                                #'element)))
                   offset)))

#+nil
(p:parse #'xml (uiop:read-file-string "tests/data/java.pom"))

;; --- Parsers --- ;;

(defun comment (offset)
  "Parser: A comment tag."
  (funcall (p:between (p:string "<!--")
                      (p:take-until (p:string "-->") :id :xml-comment)
                      (p:string "-->")
                      :id :xml-comment)
           offset))

#+nil
(comment (p:in "<!-- hello -->"))
#+nil
(p:parse #'comment "<!--
  Hello!
-->
")

(defun pair (offset)
  "Parser: Some key-value pair. Tag metadata?"
  (funcall (<*> (p:take-while1 (lambda (c) (not (or (eql #\= c) (eql #\> c)))))
                (*> (p:char #\=)
                    (p:between (p:char #\")
                               (p:take-while (lambda (c) (not (eql #\" c))))
                               (p:char #\")
                               :id :xml-pair)))
           offset))

#+nil
(pair (p:in "version=\"1.0\""))

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
          (funcall (p:sep-end1 #'skip-space-and-comments #'element) offset)))

#+nil
(p:parse #'elements "<greeting>hi!</greeting>
<!-- comment -->
<farewell hi=\"there\">bye!</farewell>
<!-- comment -->
")

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
                       (funcall (<* (*> #'skip-space-and-comments
                                        (p:alt
                                         ;; Having this sneaky case here allows us to
                                         ;; assert `take-while1' below. Look
                                         ;; carefully; `sep-end' and `take-while'
                                         ;; would otherwise form an infinite loop.
                                         (<$ "" (p:peek (p:string "</")))
                                         #'elements
                                         ;; Preemptively unwrap a single-element list
                                         ;; so that it yields just the underlying
                                         ;; string.
                                         (p:pmap (lambda (list) (if (null (cdr list)) (car list) list))
                                                 (p:sep-end1 #'skip-space-and-comments
                                                             (p:take-while1 (lambda (c) (not (or (eql #\< c) (eql #\newline c)))))))))
                                    #'skip-space-and-comments
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

(defun open-tag (offset)
  "Parser: The <foo> part of an element. If shaped like <foo/> it is in fact
standalone with no other content, and no closing tag."
  (p:fmap (lambda (two)
            (destructuring-bind (name meta slash) two
              (let ((meta (when meta
                            ;; TODO: 2025-05-20 Abstract this out.
                            (let ((ht (make-hash-table :test #'equal)))
                              (dolist (pair meta)
                                (setf (gethash (car pair) ht) (cadr pair)))
                              ht))))
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
                  (t name)))))
          (funcall (p:between (*> (p:char #\<)
                                  (p:peek (p:any-but #\/)))
                              (<*> (p:pmap #'simplify-string
                                           (p:take-while1 (lambda (c)
                                                            (not (or (eql c #\>)
                                                                     (eql c #\space)
                                                                     (eql c #\/))))))
                                   (p:opt (*> (p:char #\space)
                                              #'skip-space
                                              (p:sep-end1 #'skip-space #'pair)))
                                   (p:opt (p:char #\/)))
                              (p:char #\>)
                              :id :xml-open-tag)
                   offset)))

#+nil
(open-tag (p:in "<greeting>"))
#+nil
(p:parse #'open-tag "<greeting foo=\"bar\" baz=\"zoo\">")
#+nil
(p:parse #'open-tag "<greeting foo=\"bar\" baz=\"zoo\"/>")

(defun close-tag (label)
  (lambda (offset)
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
                              (p:string "?>")
                              :id :xml-document-type)
                   offset)))

#+nil
(p:parse #'document-type "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")

(defun simplify-string (s)
  "Convert some general string into a simple-string."
  (let ((new (make-array (length s) :element-type 'character)))
    (replace new s)))

(defun skip-space (offset)
  "A faster variant of `space' that just advances over whitespace chars."
  (funcall (p:consume (lambda (c) (or (equal c #\space) (equal c #\tab)))
                      :id :xml-skip-space)
           offset))

(declaim (ftype (function (string) simple-string) simplify-string))
(defun skip-all-space (offset)
  "Like `skip-space' but consumes newlines as well."
  (funcall (p:consume #'p:space? :id :xml-skip-all-space) offset))

(defun skip-space-and-comments (offset)
  "Blows past all the stuff we don't care about."
  (funcall (*> #'skip-all-space
               (p:skip (*> #'comment #'skip-all-space)
                 :id :xml-skip-space-and-comments))
           offset))
