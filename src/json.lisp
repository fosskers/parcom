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
           #:primitive #:string #:boolean #:null))

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
  (funcall (p:alt #'string #'p:float #'boolean #'null) input))

(defun string (input)
  "Parser: Parse any string."
  (funcall (p:between (p:char #\")
                      (p:take-while (lambda (c) (not (equal #\" c))))
                      (p:char #\"))
           input))

#+nil
(string "\"Hello\"")

(defun boolean (input)
  "Parser: Parse `true' as T and `false' as NIL."
  (funcall (p:alt (<$ t (p:string "true"))
                  (<$ nil (p:string "false")))
           input))

#+nil
(boolean "true")

(defun null (input)
  "Parser: Parse `null' as :null."
  (funcall (<$ :null (p:string "null")) input))

#+nil
(null "null")

#+nil
(object (uiop:read-file-string "tests/test/pass0.json"))

#+nil
(json "\"\b\f\n\r\t\"")

#+nil
(json "1.234567890E+34")

#+nil
(json "\"\u0123\u4567\u89AB\uCDEF\uabcd\uef4A\"")
