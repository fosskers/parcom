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
(json (p:in "{\"x\": 1, \"y\": 2, \"z\": [1, {\"a\":true}]}"))

(declaim (ftype (function (fixnum) (values (or t (member :fail)) &optional fixnum)) json))
(defun json (offset)
  "Parser: Parse any JSON value."
  (funcall (p:alt #'collection #'primitive) offset))

#+nil
(json (p:in "{\"x\": 1, \"y\": 2, \"z\": [1, {\"a\":true}]}"))

#+nil
(json (p:in "[1,true,3,\"hi\",[4]]"))

(declaim (ftype (function (fixnum) (values (or vector hash-table (member :fail)) &optional fixnum)) collection))
(defun collection (offset)
  "Parser: Parse either an Object or an Array."
  (funcall (p:alt #'object #'array) offset))

(declaim (ftype (function (fixnum) (values (or vector (member :fail)) &optional fixnum)) array))
(defun array (offset)
  "Parser: Parse a JSON Array as a Lisp vector."
  (p:fmap (lambda (list) (coerce list 'vector))
          (funcall (p:between (*> (p:char #\[) #'skip-space)
                              (p:sep (*> (p:char #\,) #'skip-space)
                                     (<* #'json #'skip-space))
                              (*> #'skip-space (p:char #\])))
                   offset)))

#+nil
(array (p:in "[]"))
#+nil
(array (p:in "[ 1,true,3,\"hi\",[4] ]"))
#+nil
(array (p:in "[ 1 , 2 ]"))

(declaim (ftype (function (fixnum) (values (or hash-table (member :fail)) &optional fixnum)) object))
(defun object (offset)
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
                   offset)))

#+nil
(object (p:in "{\"x\": 1, \"y\": 2}"))
#+nil
(object (p:in "{ \"x\" : 1 , \"y\":2 } "))

(declaim (ftype (function (fixnum) (values (or simple-string double-float t (member :fail)) &optional fixnum)) primitive))
(defun primitive (offset)
  "Parser: Parse a string, number, or boolean."
  (funcall (p:alt #'string #'scientific #'boolean #'null) offset))

(declaim (ftype (function (fixnum) (values (or character (member :fail)) &optional fixnum)) compound-char))
(defun compound-char (offset)
  "Parser: Parse a char while being wary of escaping."
  (funcall (p:alt #'escaped-char (p:any-but #\")) offset))

(declaim (ftype (function (fixnum) (values (or character (member :fail)) &optional fixnum)) escaped-char))
(defun escaped-char (offset)
  (funcall (*> (p:sneak #\\)
               (p:alt #'special-char #'p:control-char #'p:unicode))
           offset))

(declaim (ftype (function (fixnum) (values (or character (member :fail)) &optional fixnum)) special-char))
(defun special-char (offset)
  "Parser: Backslashes and quotes."
  (funcall (*> (p:char #\\)
               (p:alt (p:char #\\) (p:char #\") (p:char #\')))
           offset))

#+nil
(special-char "\\\\")
#+nil
(special-char "\\\"")
#+nil
(special-char "\\'")

(declaim (ftype (function (character) (or character cl:null)) escaped-variant))
(defun escaped-variant (c)
  "Quick one-to-one mappings of known escape characters."
  (case c
    (#\n #\newline)
    (#\t #\tab)
    (#\r #\return)
    (#\b #\backspace)
    (#\f #\page)))

(declaim (ftype (function (cl:string) simple-string) escaped))
(defun escaped (s)
  "Escape a string."
  (declare (optimize (speed 3) (safety 0)))
  (let* ((len  (length s))
         (work (make-array len :element-type 'character))
         (i 0)
         (j 0))
    (declare (dynamic-extent work))
    ;; --- Escape the original characters --- ;;
    (loop :while (< j len)
          :do (progn (let ((curr (char s j)))
                       (cond ((eql #\\ curr)
                              (let* ((next (char s (1+ j)))
                                     (escp (escaped-variant next)))
                                (cond (escp
                                       (setf (aref work i) escp)
                                       (incf j 2))
                                      ((or (eql next #\u)
                                           (eql next #\U))
                                       (let ((ex (code-char (+ (* 4096 (digit-char-p (char s (+ 2 j)) 16))
                                                               (*  256 (digit-char-p (char s (+ 3 j)) 16))
                                                               (*   16 (digit-char-p (char s (+ 4 j)) 16))
                                                               (digit-char-p (char s (+ 5 j)) 16)))))
                                         (setf (aref work i) ex)
                                         (incf j 6)))
                                      (t (setf (aref work i) next)
                                         (incf j 2)))))
                             (t (setf (aref work i) curr)
                                (incf j))))
                     (incf i)))
    ;; --- Copy the final elements over --- ;;
    (let ((final (make-array i :element-type 'character)))
      (loop :for k :from 0 :below i
            :do (setf (aref final k) (schar work k)))
      final)))

#+nil
(escaped "abcd!")
#+nil
(escaped "ab\\u0022!")
#+nil
(escaped "ab\\ncd!")

;; TODO: Avoid even the `take-while' call! All I need to know is how far the
;; offset got, then I can pass that to `escaped' to regain the ability to call
;; `schar'.
(declaim (ftype (function (fixnum) (values (or simple-string (member :fail)) &optional fixnum)) string))
(defun string (offset)
  "Parser: Parse any string."
  (let ((open-slash nil))
    (p:fmap #'escaped
            (funcall (p:between (p:char #\")
                                ;; NOTE: 2025-05-04 This was originally a call
                                ;; to (many #'compound-char), which is
                                ;; conceptually much simpler, but it was
                                ;; discovered to allocate too many intermediate
                                ;; lists. Using `take-while' at first allows us
                                ;; to scream across the source string.
                                (p:take-while (lambda (c)
                                                (cond (open-slash
                                                       (setf open-slash nil)
                                                       t)
                                                      ((eql c #\\) (setf open-slash t))
                                                      ((eql c #\") nil)
                                                      (t t))))
                                (p:char #\"))
                     offset))))

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

(declaim (ftype (function (fixnum) (values (or t nil (member :fail)) &optional fixnum)) boolean))
(defun boolean (offset)
  "Parser: Parse `true' as T and `false' as NIL."
  (funcall (p:alt (<$ t (p:string "true"))
                  (<$ nil (p:string "false")))
           offset))

#+nil
(boolean "true")

(declaim (ftype (function (fixnum) (values (or double-float (member :fail)) &optional fixnum)) scientific))
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

(declaim (ftype (function (fixnum) (values (member :null :fail) &optional fixnum)) null))
(defun null (offset)
  "Parser: Parse `null' as :null."
  (funcall (<$ :null (p:string "null")) offset))

#+nil
(null "null")

#+nil
(array (uiop:read-file-string "tests/data/pass2.json"))

(declaim (ftype (function (fixnum) (values t fixnum)) skip-space))
(defun skip-space (offset)
  "A faster variant of `multispace' that just advances over the space chars."
  (funcall (p:consume #'p:space?) offset))

#+nil
(funcall #'skip-space (p:in "   abc"))
