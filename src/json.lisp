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
  (let ((res (json (p:in input))))
    (if (p:ok? res)
        (p:parser-value res)
        (error "Oh no!")
        #+nil
        (error "Parsing json failed. Expected: ~a, but got: ~a"
               (p:failure-expected res)
               (p:failure-actual res)))))

#+nil
(parse "{\"x\": 1, \"y\": 2, \"z\": [1, {\"a\":true}]}")
#+nil
(funcall #'json (p:in "{\"x\": 1, \"y\": 2, \"z\": [1, {\"a\":true}]}"))

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

(defun object (input)
  "Parser: Parse a JSON Object as a Hash Table."
  (p:fmap (lambda (pairs)
            (let ((ht (make-hash-table :test #'equal)))
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

(defun primitive (input)
  "Parser: Parse a string, number, or boolean."
  (funcall (p:alt #'string #'scientific #'boolean #'null) input))

(defun compound-char (input)
  "Parser: Parse a char while being wary of escaping."
  (funcall (p:alt #'escaped-char (p:any-but #\")) input))

(defun escaped-char (input)
  (funcall (*> (p:peek (p:char #\\))
               (p:alt #'special-char #'p:control-char #'p:unicode))
           input))

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

(defun string (input)
  "Parser: Parse any string."
  (p:fmap (lambda (chars) (concatenate 'cl:string chars))
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

(defun null (input)
  "Parser: Parse `null' as :null."
  (funcall (<$ :null (p:string "null")) input))

#+nil
(null "null")

#+nil
(array (uiop:read-file-string "tests/data/pass2.json"))

(defun skip-space (input)
  "A faster variant of `multispace' that just advances over the space chars."
  (funcall (p:consume #'p:space?) input))

#+nil
(funcall #'skip-space (p:in "   abc"))
