(defpackage parcom
  (:use :cl)
  (:shadow #:char #:string #:integer #:float #:count #:rest #:space)
  ;; --- Types --- ;;
  (:export #:parser #:parser-input #:parser-value
           #:failure #:failure-expected #:failure-actual
           #:ok #:fail #:parse
           #:empty? #:digit?)
  ;; --- Functional Programming --- ;;
  (:export #:fmap #:const
           #:all #:right #:left #:instead
           #:*> #:<* #:<*> #:<$ #:alt)
  ;; --- Parsers --- ;;
  (:export #:any #:eof
           #:char #:string
           #:unsigned #:integer #:float
           #:newline #:space #:space1 #:multispace #:multispace1
           #:take #:take-while #:take-while1 #:rest)
  ;; --- Combinators --- ;;
  (:export #:opt #:between
           #:many #:many1 #:sep #:sep1 #:sep-end #:sep-end1
           #:skip #:peek #:count #:recognize)
  (:documentation "A simple parser combinator library."))

(in-package :parcom)

;; --- Types --- ;;

(deftype maybe-parse ()
  "A parser that might fail."
  '(function (cl:string) (or parser failure)))

(deftype always-parse ()
  "A parser that always succeeds."
  '(function (cl:string) parser))

(defstruct parser
  "The result of some successful parsing. Tracks the remaining input."
  (input nil :type cl:string)
  value)

(defun ok (input value)
  "Some successful parsing!"
  (make-parser :input input :value value))

(defstruct failure
  "The result of some failed parsing."
  (expected nil :type cl:string)
  (actual   nil :type cl:string))

(defun fail (exp act)
  (make-failure :expected exp :actual act))

(defun parse (parser input)
  "Run a parser and attempt to extract its final value."
  (let ((res (funcall parser input)))
    (etypecase res
      (parser  (parser-value res))
      (failure (error "Parsing failed. Expected: ~a, but got: ~a"
                      (failure-expected res)
                      (failure-actual res))))))

;; --- Utilities --- ;;

(declaim (ftype (function (cl:string) boolean) empty?))
(defun empty? (string)
  "Is a given string empty?"
  (zerop (length string)))

#++
(empty? "")

(declaim (ftype (function (character) boolean) digit?))
(defun digit? (char)
  "Is a given character a number from 0 to 9?"
  (char<= #\0 char #\9))

#+nil
(digit? #\7)
