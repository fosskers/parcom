;;; JSON parsing via Parser Combinators.
;;;
;;; # Performance
;;;
;;; Two tricks are used to enable fast, memory-efficient parsing. The first is
;;; _pre-cached parser lambas_, as can be seen in the `defparameter'
;;; declarations below. By preallocating these closures, subsequent usage avoids
;;; closure allocation, but also the Hash Table lookups otherwise associated
;;; with the Lambda Caches established in the main `parcom' library.
;;;
;;; The second trick is for ABCL, and involves definining some of the larger
;;; composed parsers within macros, such that under ABCL they are inlined
;;; directly at their call sites, and on non-ABCL they are stored in a
;;; `defparameter' as usual. Benchmarking and disassembly showed that ABCL is
;;; able to highly optimize the macro-based manual inlining within its internal
;;; class definitions, using "static final" values and avoiding intermediate
;;; method calls. Overall it results in a 5x speedup and 100x reduction in
;;; allocation.

(defpackage parcom/json
  (:use :cl)
  (:shadow #:array #:string #:boolean #:null #:number)
  (:import-from :parcom #:<*> #:<* #:*> #:<$)
  (:local-nicknames (#:p #:parcom))
  ;; --- Entry Points --- ;;
  (:export #:parse)
  ;; --- Parsers --- ;;
  (:export #:json
           #:collection #:array #:object
           #:primitive #:string #:boolean #:number #:null))

(in-package :parcom/json)

;; --- Static Parsers --- ;;

(defparameter +open-bracket+  (p:char #\[))
(defparameter +close-bracket+ (p:char #\]))
(defparameter +open-brace+    (p:char #\{))
(defparameter +close-brace+   (p:char #\}))
(defparameter +comma+         (p:char #\,))
(defparameter +colon+         (p:char #\:))
(defparameter +quotes+        (p:char #\"))
(defparameter +period+        (p:char #\.))
(defparameter +small-e+       (p:char #\e))
(defparameter +big-e+         (p:char #\E))
(defparameter +plus+          (p:char #\+))
(defparameter +minus+         (p:char #\-))
(defparameter +zero+          (p:char #\0))
(defparameter +true+          (p:string "true"))
(defparameter +false+         (p:string "false"))
(defparameter +null+          (p:string "null"))
(defparameter +consume-space+ (p:consume #'p:space?))
(defparameter +digits+        (p:consume #'p:digit?))
(defparameter +any-digit+     (p:any-if #'p:digit?))

;; --- Entry --- ;;

(defparameter *open-slash* nil
  "A marker for detecting backslashes in string parsing.")
(defparameter *slash-seen* nil
  "Was a backslash seen at all during this pass?")

(defun parse (input)
  "Attempt to parse any JSON value."
  (p:parse #'json input))

#+nil
(parse "{\"xy\": 1, \"yz\": 2, \"za\": [1, {\"a\":true}]}")
#+nil
(parse "{\"x\": 1, \"y\": 2, \"z\": [1, {\"a\" true}]}")
#+nil
(json (p:in "{\"x\": 1, \"y\": 2, \"z\": [1, {\"a\":true}]}"))

;; --- Parsers --- ;;

(declaim (ftype (function (fixnum) (values (or t (member :fail)) fixnum)) json))
(defun json (offset)
  "Parser: Parse any JSON value."
  (funcall (p:alt #'collection #'primitive) offset))

#+nil
(json (p:in "{\"x\": 1, \"y\": 2, \"z\": [1, {\"a\":true}]}"))

#+nil
(json (p:in "[1,true,3,\"hi\",[4]]"))

(declaim (ftype (function (fixnum) (values (or vector hash-table (member :fail)) fixnum)) collection))
(defun collection (offset)
  "Parser: Parse either an Object or an Array."
  (funcall (p:alt #'object #'array) offset))

(defmacro array-parser ()
  "A trick to enable efficient JVM optimizations."
  `(p:between
    (*> +open-bracket+ +consume-space+)
    (p:sep (*> +comma+ +consume-space+)
           (<* #'json +consume-space+))
    (*> +consume-space+ +close-bracket+)
    :id :array))

#-abcl
(defparameter +array+ (array-parser))

(declaim (ftype (function (fixnum) (values (or vector (member :fail)) fixnum)) array))
(defun array (offset)
  "Parser: Parse a JSON Array as a Lisp vector."
  (p:fmap (lambda (list) (coerce list 'vector))
          #-abcl
          (funcall +array+ offset)
          #+abcl
          (funcall (array-parser) offset)))

#+nil
(array (p:in "[]"))
#+nil
(array (p:in "[ 1,true,3,\"hi\",[4] ]"))
#+nil
(array (p:in "[ 1 , 2 ]"))
#+nil
(p:parse #'array "[1 2]")

(defmacro object-parser ()
  "A trick to enable efficient JVM optimizations."
  `(p:between
    (*> +open-brace+ +consume-space+)
    (p:sep (*> +comma+ +consume-space+)
           (<*> #'string (*> +consume-space+
                             +colon+
                             +consume-space+
                             (<* #'json +consume-space+))))
    (*> +consume-space+ +close-brace+)
    :id :object))

#-abcl
(defparameter +object+ (object-parser))

(declaim (ftype (function (fixnum) (values (or hash-table (member :fail)) fixnum)) object))
(defun object (offset)
  "Parser: Parse a JSON Object as a Hash Table."
  (p:fmap (lambda (pairs)
            (let ((ht (make-hash-table :size 8 :test #'equal)))
              (dolist (pair pairs)
                (setf (gethash (car pair) ht) (cadr pair)))
              ht))
          #-abcl
          (funcall +object+ offset)
          #+abcl
          (funcall (object-parser) offset)))

#+nil
(object (p:in "{\"x\": 1, \"y\": 2}"))
#+nil
(object (p:in "{ \"x\" : 1 , \"y\":2 } "))
#+nil
(p:parse #'object "{ \"x\" 1 , \"y\":2 } ")
#+nil
(p:parse #'collection "[{}, { \"x\": 1 , \"y\" 2 }]")

(declaim (ftype (function (fixnum) (values (or p::char-string fixnum double-float cl:boolean (member :null :fail)) fixnum)) primitive))
(defun primitive (offset)
  "Parser: Parse a string, number, boolean, or null."
  (funcall (p:alt #'string #'number #'boolean #'null) offset))

(declaim (ftype (function (character) (or character cl:null)) escaped-variant))
(defun escaped-variant (c)
  "Quick one-to-one mappings of known escape characters."
  (case c
    (#\n #\newline)
    (#\t #\tab)
    (#\r #\return)
    (#\b #\backspace)
    (#\f #\page)))

(declaim (ftype (function (p::char-string fixnum fixnum) p::char-string) escaped))
(defun escaped (s from to)
  "Escape a string."
  (declare (optimize (speed 3) (safety 0)))
  (let* ((len  (- to from))
         (work (make-array len :element-type 'character))
         (i 0)
         (j from))
    (declare (dynamic-extent work))
    ;; --- Escape the original characters --- ;;
    (loop :while (< j to)
          :do (progn (let ((curr (schar s j)))
                       (cond ((eql #\\ curr)
                              (let* ((next (schar s (1+ j)))
                                     (escp (escaped-variant next)))
                                (cond (escp
                                       (setf (schar work i) escp)
                                       (incf j 2))
                                      ((or (eql next #\u)
                                           (eql next #\U))
                                       ;; Near the end of the string, if the
                                       ;; string claims it wants to escape but
                                       ;; there can't possibly be enough code
                                       ;; points remaining, we skip escaping
                                       ;; entirely.
                                       (if (> j (- to 6))
                                           (progn (setf (schar work i) curr)
                                                  (incf j))
                                           (let ((ex (code-char (+ (* 4096 (digit-char-p (schar s (+ 2 j)) 16))
                                                                   (*  256 (digit-char-p (schar s (+ 3 j)) 16))
                                                                   (*   16 (digit-char-p (schar s (+ 4 j)) 16))
                                                                   (digit-char-p (schar s (+ 5 j)) 16)))))
                                             (setf (schar work i) ex)
                                             (incf j 6))))
                                      (t (setf (schar work i) next)
                                         (incf j 2)))))
                             (t (setf (schar work i) curr)
                                (incf j))))
                     (incf i)))
    ;; --- Copy the final elements over --- ;;
    (let ((final (make-array i :element-type 'character)))
      (loop :for k fixnum :from 0 :below i
            :do (setf (schar final k) (schar work k)))
      final)))

#+nil
(escaped "hello there" 1 3)
#+nil
(escaped "\\u03" 0 4)

(defmacro string-parser ()
  "A trick to enable efficient JVM optimizations."
  `(p:between
    +quotes+
    ;; NOTE: 2025-05-04 This was originally a call to (many #'compound-char),
    ;; which is conceptually much simpler, but it was discovered to allocate too
    ;; many intermediate lists. Further, using `take-while' still allocates
    ;; displaced arrays whose lookups as slow during escaping, so I realized
    ;; that `consume' allows us to scream across the source string and retain
    ;; fast lookups.
    (p:consume (lambda (c)
                 (cond (*open-slash*
                        (setf *open-slash* nil)
                        t)
                       ((eql c #\\)
                        (setf *open-slash* t)
                        (setf *slash-seen* t))
                       ((eql c #\") nil)
                       (t t))))
    +quotes+
    :id :string))

#-abcl
(defparameter +string+ (string-parser))

(declaim (ftype (function (fixnum) (values (or p::char-string (member :fail)) fixnum)) string))
(defun string (offset)
  "Parser: Parse any string."
  (setf *open-slash* nil)
  (setf *slash-seen* nil)
  (multiple-value-bind (res next)
      #-abcl (funcall +string+ offset)
    #+abcl (funcall (string-parser) offset)
    (cond ((p:failure? res) (p:fail next))
          ((not *slash-seen*) (values (p::direct-copy p::*input* (1+ offset) (1- next)) next))
          (t (values (escaped p::*input* (1+ offset) (1- next)) next)))))

#+nil
(string (p:in "\"hello\""))
#+nil
(string (p:in "\"\""))
#+nil
(string (p:in "\"Hello\"   "))
#+nil
(string (p:in "\"Hel\\tlo\""))
#+nil
(string (p:in "\"\\\"\""))
#+nil
(string (p:in "\"Hi \\u03B1\""))

(declaim (ftype (function (fixnum) (values (or t cl:null (member :fail)) fixnum)) boolean))
(defun boolean (offset)
  "Parser: Parse `true' as T and `false' as NIL."
  (funcall (p:alt (<$ t +true+)
                  (<$ nil +false+))
           offset))

#+nil
(boolean "true")

(declaim (ftype (function (fixnum) (values (or fixnum double-float (member :fail)) fixnum)) number))
(defun number (offset)
  "Parser: Parse a JSON number, either a `fixnum' or something in scientific
notation. Optimized to yield a fixnum early if no futher float-like characters
could be parsed."
  (multiple-value-bind (int off0) (funcall #'p:integer offset)
    (if (p:failure? int)
        (p:fail off0)
        (multiple-value-bind (_ off1)
            (funcall (*> (p:opt (*> +period+ +any-digit+ +digits+))
                         (p:opt (*> (p:alt +small-e+ +big-e+)
                                    (p:opt (p:alt +plus+ +minus+))
                                    (p:skip +zero+)
                                    ;; BUG: 2025-07-12 If someone attempts to
                                    ;; parse just `1e' a condition will be
                                    ;; raised by the compiler's float parser.
                                    ;; The `opt' was initial added here to avoid
                                    ;; the bug caused by `1e00'.
                                    (p:opt #'p:unsigned))))
                     off0)
          (declare (ignore _))
          ;; If the second parser moved _at all_ we have to consider it a
          ;; "success" in terms of float parsing, since the two `opt' calls can
          ;; otherwise not fail.
          (if (= off0 off1)
              (values int off0)
              (let ((s (make-array (- off1 offset)
                                   :element-type 'character
                                   :displaced-to p::*input*
                                   :displaced-index-offset offset))
                    (*read-default-float-format* 'double-float))
                (values (cl:float (read-from-string s) 1.0d0)
                        off1)))))))

#+nil
(p:parse #'number "123456789!")
#+nil
(p:parse #'number "123.456!")
#+nil
(number (p:in "123.a"))
#+nil
(p:parse #'number "123E10!")
#+nil
(number (p:in "-123.456E10!"))
#+nil
(p:parse #'number "1e00")

#+nil
(let ((*read-default-float-format* 'double-float))
  (read-from-string "1.23e4"))

(declaim (ftype (function (fixnum) (values (member :null :fail) fixnum)) null))
(defun null (offset)
  "Parser: Parse `null' as :null."
  (funcall (<$ :null +null+) offset))

#+nil
(p:parse #'null "null")

#+nil
(p:parse #'array (uiop:read-file-string "tests/data/pass2.json"))
