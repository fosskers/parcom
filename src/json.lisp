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

(defun parse (input)
  "Attempt to parse any JSON value."
  (multiple-value-bind (res next) (json (p:in input))
    (declare (ignore next))
    (if (p:ok? res)
        res
        (error "Oh no!")
        #+nil
        (error "Parsing json failed. Expected: ~a, but got: ~a"
               (p:failure-expected res)
               (p:failure-actual res)))))

#+nil
(parse "{\"x\": 1, \"y\": 2, \"z\": [1, {\"a\":true}]}")
#+nil
(funcall #'json (p:in "{\"x\": 1, \"y\": 2, \"z\": [1, {\"a\":true}]}"))

(declaim (ftype (function (fixnum) (values (or t (member :fail)) &optional fixnum)) json))
(defun json (input)
  "Parser: Parse any JSON value."
  (funcall (p:alt #'collection #'primitive) input))

#+nil
(json "{\"x\": 1, \"y\": 2, \"z\": [1, {\"a\":true}]}")
#+nil
(json "[1,true,3,\"hi\",[4]]")

(declaim (ftype (function (fixnum) (values (or vector hash-table (member :fail)) &optional fixnum)) collection))
(defun collection (input)
  "Parser: Parse either an Object or an Array."
  (funcall (p:alt #'object #'array) input))

(declaim (ftype (function (fixnum) (values (or vector (member :fail)) &optional fixnum)) array))
(defun array (input)
  "Parser: Parse a JSON Array as a Lisp vector."
  (p:fmap (lambda (list) (coerce list 'vector))
          (funcall (p:between (*> (p:char #\[) #'skip-space)
                              (p:sep (*> (p:char #\,) #'skip-space)
                                     (<* #'json #'skip-space))
                              (*> #'skip-space (p:char #\])))
                   input)))

#+nil
(array "[]")
#+nil
(array "[ 1,true,3,\"hi\",[4] ]")
#+nil
(array "[ 1 , 2 ]")

(declaim (ftype (function (fixnum) (values (or hash-table (member :fail)) &optional fixnum)) object))
(defun object (input)
  "Parser: Parse a JSON Object as a Hash Table."
  (p:fmap (lambda (pairs)
            (let ((ht (make-hash-table :size 8 :test #'equal)))
              (dolist (pair pairs)
                (setf (gethash (car pair) ht) (cdr pair)))
              ht))
          (funcall (p:between (*> (p:char #\{) #'skip-space)
                              (p:sep (*> (p:char #\,) #'skip-space)
                                     (p:pair #'string (*> #'skip-space
                                                          (p:char #\:)
                                                          #'skip-space
                                                          (<* #'json #'skip-space))))
                              (*> #'skip-space (p:char #\})))
                   input)))

#+nil
(object "{\"x\": 1, \"y\": 2}")
#+nil
(object "{ \"x\" : 1 , \"y\":2 } ")

(declaim (ftype (function (fixnum) (values (or simple-string double-float t (member :fail)) &optional fixnum)) primitive))
(defun primitive (input)
  "Parser: Parse a string, number, or boolean."
  (funcall (p:alt #'string #'scientific #'boolean #'null) input))

(declaim (ftype (function (fixnum) (values (or character (member :fail)) &optional fixnum)) compound-char))
(defun compound-char (input)
  "Parser: Parse a char while being wary of escaping."
  (funcall (p:alt #'escaped-char (p:any-but #\")) input))

(declaim (ftype (function (fixnum) (values (or character (member :fail)) &optional fixnum)) escaped-char))
(defun escaped-char (input)
  (funcall (*> (p:sneak #\\)
               (p:alt #'special-char #'p:control-char #'p:unicode))
           input))

(declaim (ftype (function (fixnum) (values (or character (member :fail)) &optional fixnum)) special-char))
(defun special-char (input)
  "Parser: Backslashes and quotes."
  (funcall (*> (p:char #\\)
               (p:alt (p:char #\\) (p:char #\") (p:char #\')))
           input))

#+nil
(special-char "\\\\")
#+nil
(special-char "\\\"")
#+nil
(special-char "\\'")

(declaim (ftype (function (fixnum) (values (or simple-string (member :fail)) &optional fixnum)) string))
(defun string (input)
  "Parser: Parse any string."
  (p:fmap (lambda (chars) (concatenate 'cl:simple-string chars))
          (funcall (p:between (p:char #\")
                              (p:many #'compound-char)
                              (p:char #\"))
                   input)))

#+nil
(string (p:in "\"Hel\\tlo\""))
#+nil
(string (p:in "\"\\\"\""))
#+nil
(string (p:in "\"Hi \\u03B1\""))

(declaim (ftype (function (fixnum) (values (or t nil (member :fail)) &optional fixnum)) boolean))
(defun boolean (input)
  "Parser: Parse `true' as T and `false' as NIL."
  (funcall (p:alt (<$ t (p:string "true"))
                  (<$ nil (p:string "false")))
           input))

#+nil
(boolean "true")

(declaim (ftype (function (fixnum) (values (or double-float (member :fail)) &optional fixnum)) scientific))
(defun scientific (input)
  "Parser: Parse a JSON number in scientific notation."
  (p:fmap (lambda (s)
            (let ((*read-default-float-format* 'double-float))
              (cl:float (read-from-string s) 1.0d0)))
          (funcall (p:recognize (*> #'p:float
                                    (p:opt (*> (p:alt (p:char #\E) (p:char #\e))
                                               (p:opt (p:alt (p:char #\+) (p:char #\-)))
                                               (*> (p:skip (p:char #\0)) (p:opt #'p:unsigned))))))
                   input)))

#+nil
(scientific "23456789012E66   ")

#+nil
(let ((*read-default-float-format* 'double-float))
  (read-from-string "1.23e4"))

(declaim (ftype (function (fixnum) (values (member :null :fail) &optional fixnum)) null))
(defun null (input)
  "Parser: Parse `null' as :null."
  (funcall (<$ :null (p:string "null")) input))

#+nil
(null "null")

#+nil
(array (uiop:read-file-string "tests/data/pass2.json"))

(declaim (ftype (function (fixnum) (values t fixnum)) skip-space))
(defun skip-space (input)
  "A faster variant of `multispace' that just advances over the space chars."
  (funcall (p:consume #'p:space?) input))

#+nil
(funcall #'skip-space (p:in "   abc"))
