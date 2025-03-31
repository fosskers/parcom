(defpackage parcom
  (:use :cl)
  (:shadow #:char #:string)
  ;; --- Types --- ;;
  (:export #:parser #:parser-input #:parser-value
           #:failure #:failure-expected #:failure-actual
           #:ok #:fail #:parse
           #:empty?)
  ;; --- Functional Programming --- ;;
  (:export #:fmap #:const #:comp
           #:*> #:<* #:<$ #:alt)
  ;; --- Parsers --- ;;
  (:export #:any #:eof
           #:char #:string
           #:newline
           #:take #:take-while)
  ;; --- Combinators --- ;;
  (:export #:opt #:delimited
           #:many0 #:many1 #:sep0 #:sep1
           #:skip #:peek)
  (:documentation "A simple parser combinator library."))

(in-package :parcom)

;; --- Types --- ;;

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
