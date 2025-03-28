(defpackage parcom
  (:use :cl)
  (:export)
  (:documentation "A simple parser combinator library."))

(in-package :parcom)

(defstruct right
  "Some successful result."
  val)

(defun right (val)
  (make-right :val val))

(defstruct left
  "Some failure result."
  val)

(defun left (val)
  (make-left :val val))

(defstruct perror
  (expected nil :type string)
  (actual   nil :type string))

(defun perror (exp act)
  (make-perror :expected exp :actual act))

(declaim (ftype (function (string) boolean) empty?))
(defun empty? (string)
  "Is a given string empty?"
  (zerop (length string)))

#++
(empty? "")

(defun any (input)
  (if (empty? input)
      (left (perror "any char" "end of input"))
      (right (cons (subseq input 1)
                   (aref input 0)))))

#++
(any "hello")
#++
(any "")

;; Basically, do it just like nom. Input is always a string, or perhaps a
;; wrapper around a string that records a cursor of where we are in the original
;; source. We might not be able to do zero-copy substrings in CL.

(defun eof (input)
  (if (empty? input)
      (right (cons input t))
      (left (perror "the end of the input" input))))

#++
(eof "hi")
#++
(eof "")
