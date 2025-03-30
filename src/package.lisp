(defpackage parcom
  (:use :cl)
  (:shadow #:char #:string)
  ;; --- Types --- ;;
  (:export #:parser #:parser-input #:parser-value
           #:failure #:failure-expected #:failure-actual
           #:ok #:fail
           #:empty?)
  ;; --- Functional Programming --- ;;
  (:export #:fmap #:const #:comp
           #:*> #:<* #:<$)
  ;; --- Combinators --- ;;
  (:export #:any #:eof
           #:char #:string)
  (:documentation "A simple parser combinator library."))

(in-package :parcom)

;; --- Types --- ;;

(defstruct parser
  "The result of some successful parsing. Tracks the remaining input."
  (input nil :type string)
  value)

(defun ok (input value)
  "Some successful parsing!"
  (make-parser :input input :value value))

(defstruct failure
  "The result of some failed parsing."
  (expected nil :type string)
  (actual   nil :type string))

(defun fail (exp act)
  (make-failure :expected exp :actual act))

;; --- Utilities --- ;;

(declaim (ftype (function (string) boolean) empty?))
(defun empty? (string)
  "Is a given string empty?"
  (zerop (length string)))

#++
(empty? "")
