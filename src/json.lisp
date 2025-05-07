(defpackage parcom/json
  (:use :cl)
  (:shadow #:array #:string #:boolean #:null)
  (:import-from :parcom #:<*> #:<* #:*> #:<$)
  (:local-nicknames (#:p #:parcom))
  ;; --- Entry Points --- ;;
  (:export #:parse)
  ;; --- Parsers --- ;;
  (:export #:json
           #:collection #:array #:object
           #:primitive #:string #:boolean #:scientific #:null))

(in-package :parcom/json)

(defparameter +open-slash+ nil
  "A marker for detecting backslashes in string parsing.")
(defparameter +slash-seen+ nil
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

(declaim (ftype (function (fixnum) (values (or vector (member :fail)) fixnum)) array))
(defun array (offset)
  "Parser: Parse a JSON Array as a Lisp vector."
  (p:fmap (lambda (list) (coerce list 'vector))
          (funcall (p:between (*> (p:char #\[) #'skip-space)
                              (p:sep (*> (p:char #\,) #'skip-space)
                                     (<* #'json #'skip-space)
                                     :id :json-array)
                              (*> #'skip-space (p:char #\]))
                              :id :json-array)
                   offset)))

#+nil
(array (p:in "[]"))
#+nil
(array (p:in "[ 1,true,3,\"hi\",[4] ]"))
#+nil
(array (p:in "[ 1 , 2 ]"))
#+nil
(p:parse #'array "[1 2]")

(declaim (ftype (function (fixnum) (values (or hash-table (member :fail)) fixnum)) object))
(defun object (offset)
  "Parser: Parse a JSON Object as a Hash Table."
  (p:fmap (lambda (pairs)
            (let ((ht (make-hash-table :size 8 :test #'equal)))
              (dolist (pair pairs)
                (setf (gethash (car pair) ht) (cadr pair)))
              ht))
          (funcall (p:between (*> (p:char #\{) #'skip-space)
                              (p:sep (*> (p:char #\,) #'skip-space)
                                     (<*> #'string (*> #'skip-space
                                                       (p:char #\:)
                                                       #'skip-space
                                                       (<* #'json #'skip-space)))
                                     :id :json-object)
                              (*> #'skip-space (p:char #\}))
                              :id :json-object)
                   offset)))

#+nil
(object (p:in "{\"x\": 1, \"y\": 2}"))
#+nil
(object (p:in "{ \"x\" : 1 , \"y\":2 } "))
#+nil
(p:parse #'object "{ \"x\" 1 , \"y\":2 } ")
#+nil
(p:parse #'collection "[{}, { \"x\": 1 , \"y\" 2 }]")

(declaim (ftype (function (fixnum) (values (or p::char-string double-float t (member :fail)) fixnum)) primitive))
(defun primitive (offset)
  "Parser: Parse a string, number, or boolean."
  (funcall (p:alt #'string #'scientific #'boolean #'null) offset))

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
                                       (let ((ex (code-char (+ (* 4096 (digit-char-p (schar s (+ 2 j)) 16))
                                                               (*  256 (digit-char-p (schar s (+ 3 j)) 16))
                                                               (*   16 (digit-char-p (schar s (+ 4 j)) 16))
                                                               (digit-char-p (schar s (+ 5 j)) 16)))))
                                         (setf (schar work i) ex)
                                         (incf j 6)))
                                      (t (setf (schar work i) next)
                                         (incf j 2)))))
                             (t (setf (schar work i) curr)
                                (incf j))))
                     (incf i)))
    ;; --- Copy the final elements over --- ;;
    (let ((final (make-array i :element-type 'character)))
      (loop :for k :from 0 :below i
            :do (setf (schar final k) (schar work k)))
      final)))

#+nil
(escaped "hello there" 1 3)

(declaim (ftype (function (p::char-string fixnum fixnum) p::char-string) naive-copy))
(defun naive-copy (s from to)
  "We know no escaping needs to occur, so we can just copy the characters over as-is."
  (declare (optimize (speed 3) (safety 0)))
  (let* ((len  (- to from))
         (work (make-array len :element-type 'character)))
    (loop :for i :from 0 :below len
          :do (setf (schar work i) (schar s (+ i from))))
    work))

#+nil
(naive-copy "hello there" 1 3)

(declaim (ftype (function (fixnum) (values (or p::char-string (member :fail)) fixnum)) string))
(defun string (offset)
  "Parser: Parse any string."
  (setf +open-slash+ nil)
  (setf +slash-seen+ nil)
  (multiple-value-bind (res next)
      (funcall (p:between (p:char #\")
                          ;; NOTE: 2025-05-04 This was originally a call to
                          ;; (many #'compound-char), which is conceptually
                          ;; much simpler, but it was discovered to allocate
                          ;; too many intermediate lists. Further, using
                          ;; `take-while' still allocates displaced arrays
                          ;; whose lookups as slow during escaping, so I
                          ;; realized that `consume' allows us to scream
                          ;; across the source string and retain fast lookups.
                          (p:consume (lambda (c)
                                       (cond (+open-slash+
                                              (setf +open-slash+ nil)
                                              t)
                                             ((eql c #\\)
                                              (setf +open-slash+ t)
                                              (setf +slash-seen+ t))
                                             ((eql c #\") nil)
                                             (t t)))
                                     :id :json-string)
                          (p:char #\")
                          :id :json-string)
               offset)
    (cond ((p:failure? res) (p:fail next))
          ((not +slash-seen+) (values (naive-copy p::+input+ (1+ offset) (1- next)) next))
          (t (values (escaped p::+input+ (1+ offset) (1- next)) next)))))

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
  (funcall (p:alt (<$ t (p:string "true"))
                  (<$ nil (p:string "false")))
           offset))

#+nil
(boolean "true")

(declaim (ftype (function (fixnum) (values (or double-float (member :fail)) fixnum)) scientific))
(defun scientific (offset)
  "Parser: Parse a JSON number in scientific notation."
  (p:fmap (lambda (s)
            (let ((*read-default-float-format* 'double-float))
              (cl:float (read-from-string s) 1.0d0)))
          (funcall (p:recognize (*> #'p:float
                                    (p:opt (*> (p:alt (p:char #\E) (p:char #\e))
                                               (p:opt (p:alt (p:char #\+) (p:char #\-)))
                                               (*> (p:skip (p:char #\0)) (p:opt #'p:unsigned))))))
                   offset)))

#+nil
(scientific "23456789012E66   ")

#+nil
(let ((*read-default-float-format* 'double-float))
  (read-from-string "1.23e4"))

(declaim (ftype (function (fixnum) (values (member :null :fail) fixnum)) null))
(defun null (offset)
  "Parser: Parse `null' as :null."
  (funcall (<$ :null (p:string "null")) offset))

#+nil
(null "null")

#+nil
(array (p:in (uiop:read-file-string "tests/data/pass2.json")))

(declaim (ftype (function (fixnum) (values t fixnum)) skip-space))
(defun skip-space (offset)
  "A faster variant of `multispace' that just advances over the space chars."
  (funcall (p:consume #'p:space? :id :json-skip-space) offset))

#+nil
(funcall #'skip-space (p:in "   abc"))
