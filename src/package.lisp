(defpackage parcom
  (:use :cl)
  (:shadow #:char #:string #:integer #:float #:count #:rest #:space)
  ;; --- Types --- ;;
  (:export #:parse #:in
           #:failure? #:fail
           #:ok #:ok?
           #:empty?
           #:digit? #:hex? #:octal? #:binary?
           #:ascii-letter? #:space?)
  ;; --- Functional Programming --- ;;
  (:export #:fmap #:pmap #:const
           #:all #:right #:left #:instead
           #:*> #:<* #:<*> #:<$ #:alt)
  ;; --- Parsers --- ;;
  (:export #:any #:any-but #:any-if #:hex #:unicode #:control-char #:eof
           #:char #:string
           #:unsigned #:integer #:float
           #:newline #:space #:space1 #:multispace #:multispace1
           #:take #:take-while #:take-while1 #:rest
           #:pure)
  ;; --- Combinators --- ;;
  (:export #:opt #:between #:pair
           #:many #:many1 #:sep #:sep1 #:sep-end #:sep-end1 #:take-until
           #:consume #:skip #:peek #:sneak #:count #:recognize)
  ;; --- Conditions --- ;;
  (:export #:parse-failure)
  (:documentation "A simple parser combinator library."))

(in-package :parcom)

;; --- Types --- ;;

(deftype maybe-parse ()
  "A parser that might fail."
  '(function (fixnum) (values (or t (member :fail)) fixnum)))

(deftype char-string ()
  '(simple-array character (*)))

;; --- Top-level pointer to the input --- ;;

(declaim (type char-string *input*))
(defparameter *input* ""
  "A global pointer to the current input string.")
(declaim (type fixnum *input-length*))
(defparameter *input-length* 0
  "The length of the current global input.")

;; --- Lambda Caches --- ;;

(defparameter *char-cache*    (make-hash-table :size 64 :test #'eql))
(defparameter *any-but-cache* (make-hash-table :size 32 :test #'eql))
(defparameter *sneak-cache*   (make-hash-table :size 32 :test #'eql))
(defparameter *consume-cache* (make-hash-table :size 16 :test #'eq))
(defparameter *between-cache* (make-hash-table :size 16 :test #'eq))
(defparameter *sep-cache*     (make-hash-table :size 16 :test #'eq))
(defparameter *skip-cache*    (make-hash-table :size 16 :test #'eq))
(defparameter *string-cache*  (make-hash-table :size 64 :test #'equal))
(defparameter *take-until-cache* (make-hash-table :size 16 :test #'eq))

;; --- Conditions --- ;;

(define-condition parse-failure (error)
  ((offset  :initarg :offset  :reader parse-failure-offset)
   (context :initarg :context :reader parse-failure-context))
  (:documentation "Some parsing failed, so we render why.")
  (:report (lambda (c stream)
             (format stream "Parsing failed at location ~a. Context:~%↓~%~a"
                     (parse-failure-offset c)
                     (parse-failure-context c)))))

;; --- Short-hands --- ;;

(declaim (ftype (function (char-string) fixnum) in))
(defun in (input)
  "Set the global input and yield the initial parser offset."
  (setf *input* input)
  (setf *input-length* (length *input*))
  0)

#+nil
(in "hello")

(declaim (ftype (function (fixnum fixnum) fixnum) off))
(defun off (offset curr)
  "Advance the input by some offset."
  (declare (optimize (speed 3)))
  (+ offset curr))

#+nil
(off 4 10)

(defmacro ok (offset value)
  "Parsing was successful."
  `(values ,value ,offset))

(defmacro ok? (x)
  "Did parsing succeed?"
  `(not (failure? ,x)))

(defmacro fail (offset)
  "Fail a parse while recording while recording how far it got."
  `(values :fail ,offset))

#+nil
(fail 1)

(defmacro failure? (x)
  "Did parsing fail?"
  `(eq :fail ,x))

(defun parse (parser input)
  "Run a parser and attempt to extract its final value."
  (multiple-value-bind (res next) (funcall parser (in input))
    (if (ok? res)
        res
        (let ((diff (- *input-length* next)))
          (error 'parse-failure
                 :offset next
                 :context (if (< diff 32)
                              (make-array diff
                                          :element-type 'character
                                          :displaced-to *input*
                                          :displaced-index-offset next)
                              (format nil "~a ... (truncated)"
                                      (make-array 32
                                                  :element-type 'character
                                                  :displaced-to *input*
                                                  :displaced-index-offset next))))))))

#+nil
(parse (*> (char #\a) (char #\b)) "acb")

;; --- Utilities --- ;;

(declaim (ftype (function (cl:string) boolean) empty?))
(defun empty? (string)
  "Is a given string empty?"
  (zerop (length string)))

#++
(empty? "")

(declaim (ftype (function (character) boolean) ascii-letter?))
(defun ascii-letter? (char)
  "A-Za-z"
  (or (char<= #\a char #\z)
      (char<= #\A char #\Z)))

#+nil
(ascii-letter? #\h)
#+nil
(ascii-letter? #\1)

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

(declaim (ftype (function (character) boolean) octal?))
(defun octal? (char)
  "Is a given character an octal digit?"
  (char<= #\0 char #\7))

(declaim (ftype (function (character) boolean) binary?))
(defun binary? (char)
  "Is a given character a binary digit?"
  (char<= #\0 char #\1))

(declaim (ftype (function (character) boolean) space?))
(defun space? (char)
  "Is a given character some sort of whitespace?"
  (or (eql char #\space)
      (eql char #\newline)
      (eql char #\tab)
      (eql char #\return)))

(declaim (ftype (function (char-string fixnum fixnum) char-string) direct-copy))
(defun direct-copy (s from to)
  "Direct, low-level string copying."
  (declare (optimize (speed 3) (safety 0)))
  (let* ((len  (- to from))
         (work (make-array len :element-type 'character)))
    #+abcl
    (progn (loop :for i fixnum :from 0 :below len
                 :do (setf (schar work i) (schar s (+ i from))))
           work)
    #-abcl
    (replace work s :start2 from :end2 to)))

#+nil
(direct-copy "hello there" 1 3)
