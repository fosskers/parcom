(defpackage parcom
  (:use :cl)
  (:export)
  (:documentation "A simple parser combinator library."))

(in-package :parcom)

;; --- Types --- ;;

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

;; --- Utilities --- ;;

(defmacro comp (function &rest functions)
  "Function composition."
  (let ((args (gensym "COMP-ARGS-"))
        (reversed (reverse (cons function functions))))
    `(lambda (&rest ,args)
       ,(reduce (lambda (data fn)
                  `(funcall ,fn ,data))
                (cdr reversed)
                :initial-value `(apply ,(car reversed) ,args)))))

#++
(funcall (comp #'1+ #'length) '(1 2 3))

(declaim (ftype (function (string) boolean) empty?))
(defun empty? (string)
  "Is a given string empty?"
  (zerop (length string)))

#++
(empty? "")

(defgeneric flat-map (f thing)
  (:documentation "Try to apply a new function to the inner contents of some `thing'."))

(defmethod flat-map ((f function) (right right))
  "Dive into the `right' via some `f'."
  (funcall f (right-val right)))

(defmethod flat-map ((f function) (left left))
  "Pass through the `left' value without manipulating it."
  left)

#++
(flat-map (comp #'right #'1+)
          (flat-map (comp #'right #'1+) (right 1)))
#++
(flat-map (comp #'right #'1+)
          (flat-map (comp #'right #'1+) (left "no!")))

;; --- Combinators --- ;;

;; --- Parsers --- ;;

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

#++
(let* ((s "Hello")
       (q (make-array (1- (length s))
                      :element-type 'character
                      :displaced-to s
                      :displaced-index-offset 1)))
  (setf (aref s 2) #\G)
  (cons s q))
