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
  (let ((res (json input)))
    (etypecase res
      (p:parser (p:parser-value res))
      (p:failure (error "Parsing json failed. Expected: ~a, but got: ~a"
                        (p:failure-expected res)
                        (p:failure-actual res))))))

#+nil
(parse "{\"x\": 1, \"y\": 2, \"z\": [1, {\"a\":true}]}")

(defun json (input)
  "Parser: Parse any JSON value."
  (funcall (p:alt #'collection #'primitive) input))

#+nil
(json "{\"x\": 1, \"y\": 2, \"z\": [1, {\"a\":true}]}")
#+nil
(json "[1,true,3,\"hi\",[4]]")

(defun collection (input)
  "Parser: Parse either an Object or an Array."
  (funcall (p:alt #'object #'array) input))

(defun array (input)
  "Parser: Parse a JSON Array as a Lisp vector."
  (p:fmap (lambda (list) (coerce list 'vector))
          (funcall (p:between (*> (p:char #\[) #'p:multispace)
                              (p:sep (*> (p:char #\,) #'p:multispace)
                                     (<* #'json #'p:multispace))
                              (*> #'p:multispace (p:char #\])))
                   input)))

#+nil
(array "[]")
#+nil
(array "[ 1,true,3,\"hi\",[4] ]")
#+nil
(array "[ 1 , 2 ]")

(defun object (input)
  "Parser: Parse a JSON Object as a Hash Table."
  (p:fmap (lambda (pairs)
            (let ((ht (make-hash-table :test #'equal)))
              (dolist (pair pairs)
                (setf (gethash (car pair) ht) (cdr pair)))
              ht))
          (funcall (p:between (*> (p:char #\{) #'p:multispace)
                              (p:sep (*> (p:char #\,) #'p:multispace)
                                     (p:pair #'string (*> #'p:multispace
                                                          (p:char #\:)
                                                          #'p:multispace
                                                          (<* #'json #'p:multispace))))
                              (*> #'p:multispace (p:char #\})))
                   input)))

#+nil
(object "{\"x\": 1, \"y\": 2}")
#+nil
(object "{ \"x\" : 1 , \"y\":2 } ")

(defun primitive (input)
  "Parser: Parse a string, number, or boolean."
  (funcall (p:alt #'string #'scientific #'boolean #'null) input))

(defun compound-char (input)
  "Parser: Parse a char while being wary of escaping."
  (funcall (p:alt #'special-char #'control-char #'unicode (p:anybut #\")) input))

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

(defun control-char (input)
  "Parser: Newlines and whatnot."
  (funcall (*> (p:char #\\)
               (p:alt (<$ #\newline (p:char #\n))
                      (<$ #\tab (p:char #\t))
                      (<$ #\return (p:char #\r))
                      (<$ #\backspace (p:char #\b))
                      (<$ #\formfeed (p:char #\f))))
           input))

#+nil
(control-char "\\n")

(defun unicode (input)
  "Parser: Parse a unicode char of 4 hex values."
  (p:fmap (lambda (chars)
            (destructuring-bind (a b c d) chars
              (code-char (+ (* 4096 (digit-char-p a 16))
                            (* 256 (digit-char-p b 16))
                            (* 16 (digit-char-p c 16))
                            (digit-char-p d 16)))))
          (funcall (*> (p:char #\\)
                       (p:alt (p:char #\u) (p:char #\U))
                       (p:count 4 #'p:hex))
                   input)))

#+nil
(unicode "\\u0022")
#+nil
(unicode "\\U0022")

(defun string (input)
  "Parser: Parse any string."
  (p:fmap (lambda (chars) (concatenate 'cl:string chars))
          (funcall (p:between (p:char #\")
                              (p:many #'compound-char)
                              (p:char #\"))
                   input)))

#+nil
(string "\"Hel\\tlo\"")
#+nil
(string "\"\\\"\"")
#+nil
(string "\"Hi \\u03B1\"")

(defun boolean (input)
  "Parser: Parse `true' as T and `false' as NIL."
  (funcall (p:alt (<$ t (p:string "true"))
                  (<$ nil (p:string "false")))
           input))

#+nil
(boolean "true")

(defun scientific (input)
  "Parser: Parse a JSON number in scientific notation."
  (p:fmap (lambda (s)
            (let ((*read-default-float-format* 'double-float))
              (read-from-string s)))
          (funcall (p:recognize (p:pair #'p:float
                                        (p:opt (*> (p:alt (p:char #\E) (p:char #\e))
                                                   (p:opt (p:alt (p:char #\+) (p:char #\-)))
                                                   #'p:unsigned))))
                   input)))

#+nil
(scientific "23456789012E66   ")

(let ((*read-default-float-format* 'double-float))
  (read-from-string "1.23e4"))

(defun null (input)
  "Parser: Parse `null' as :null."
  (funcall (<$ :null (p:string "null")) input))

#+nil
(null "null")

#+nil
(parse (uiop:read-file-string "tests/data/pass0.json"))
