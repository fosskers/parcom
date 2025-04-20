(defpackage parcom
  (:use :cl)
  (:shadow #:char #:string #:integer #:float #:count #:rest #:space)
  ;; --- Types --- ;;
  (:export #:parser #:parser-input #:parser-value
           #:failure #:failure-expected #:failure-actual
           #:input #:in
           #:ok #:fail #:parse
           #:empty? #:digit?)
  ;; --- Functional Programming --- ;;
  (:export #:fmap #:const
           #:all #:right #:left #:instead
           #:*> #:<* #:<*> #:<$ #:alt)
  ;; --- Parsers --- ;;
  (:export #:any #:anybut #:hex #:eof
           #:char #:string
           #:unsigned #:integer #:float
           #:newline #:space #:space1 #:multispace #:multispace1
           #:take #:take-while #:take-while1 #:rest)
  ;; --- Combinators --- ;;
  (:export #:opt #:between #:pair
           #:many #:many1 #:sep #:sep1 #:sep-end #:sep-end1
           #:skip #:peek #:count #:recognize)
  ;; --- Conditions --- ;;
  (:export #:parse-failure)
  (:documentation "A simple parser combinator library."))

(in-package :parcom)

;; --- Conditions --- ;;

(define-condition parse-failure (error)
  ((expected :initarg expected :reader parse-failure-expected)
   (actual   :initarg actual   :reader parse-failure-actual))
  (:documentation "Some parsing failed, so we render why.")
  (:report (lambda (c stream)
             (format stream "Expected:~%  ~a~%Actual:~%  ~a"
                     (parse-failure-expected c)
                     (parse-failure-actual c)))))

;; --- Types --- ;;

(defstruct input
  "The remaining parser input with a cached reference to its first character."
  (head nil :type character)
  (str  ""  :type cl:string))

(declaim (ftype (function (cl:string) input) in))
(defun in (input)
  "Smart constructor for some parser input."
  (declare (optimize (speed 3) (safety 0)))
  (make-input :head (cl:char input 0) :str input))

#+nil
(in "hello")

(deftype maybe-parse ()
  "A parser that might fail."
  '(function (input) (or parser failure)))

(deftype always-parse ()
  "A parser that always succeeds."
  '(function (input) parser))

(defstruct parser
  "The result of some successful parsing. Tracks the remaining input."
  (input nil :type input)
  value)

(declaim (ftype (function (cl:string t) parser) ok))
(defun ok (input value)
  "Some successful parsing!"
  (make-parser :input (in input) :value value))

(defstruct failure
  "The result of some failed parsing."
  expected
  (actual nil :type input))

(defun fail (exp act)
  "It's assumed that you pass back the entire remaining input as the 'actual' value.
Error reporting code will check the length of this and truncate it if necessary."
  (make-failure :expected exp :actual act))

(defun parse (parser input)
  "Run a parser and attempt to extract its final value."
  (let ((res (funcall parser (in input))))
    (etypecase res
      (parser  (parser-value res))
      (failure (error 'parse-failure
                      :expected (failure-expected res)
                      :actual (if (< (length (input-str (failure-actual res))) 16)
                                  (failure-actual res)
                                  (format nil "~a ... (truncated)"
                                          (make-array 16
                                                      :element-type 'character
                                                      :displaced-to (input-str (failure-actual res))))))))))

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

(declaim (ftype (function (character) boolean) hex?))
(defun hex? (char)
  "Is a given character a hex digit?"
  (or (digit? char)
      (char<= #\a char #\f)
      (char<= #\A char #\F)))

#+nil
(hex? #\7)
#+nil
(hex? #\J)
