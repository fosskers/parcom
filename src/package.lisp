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
  (:export #:fmap #:const
           #:all #:right #:left #:instead
           #:*> #:<* #:<*> #:<$ #:alt)
  ;; --- Parsers --- ;;
  (:export #:any #:any-but #:any-if #:hex #:unicode #:control-char #:eof
           #:char #:string
           #:unsigned #:integer #:float
           #:newline #:space #:space1 #:multispace #:multispace1
           #:take #:take-while #:take-while1 #:rest)
  ;; --- Combinators --- ;;
  (:export #:opt #:between #:pair
           #:many #:many1 #:sep #:sep1 #:sep-end #:sep-end1 #:take-until
           #:consume #:skip #:peek #:sneak #:count #:recognize)
  ;; --- Conditions --- ;;
  (:export #:parse-failure)
  (:documentation "A simple parser combinator library."))

(in-package :parcom)

;; --- Top-level pointer to the input --- ;;

(defparameter +input+ nil
  "A global pointer to the current input string.")
(defparameter +input-length+ 0
  "The length of the current global input.")

;; --- Lambda Caches --- ;;

(defparameter +char-cache+    (make-hash-table :size 64 :test #'eql))
(defparameter +any-but-cache+ (make-hash-table :size 32 :test #'eql))
(defparameter +sneak-cache+   (make-hash-table :size 32 :test #'eql))
(defparameter +consume-cache+ (make-hash-table :size 16 :test #'eq))
(defparameter +between-cache+ (make-hash-table :size 16 :test #'eq))
(defparameter +sep-cache+     (make-hash-table :size 16 :test #'eq))

;; --- Conditions --- ;;

(define-condition parse-failure (error)
  ((offset  :initarg :offset  :reader parse-failure-offset)
   (context :initarg :context :reader parse-failure-context))
  (:documentation "Some parsing failed, so we render why.")
  (:report (lambda (c stream)
             (format stream "Parsing failed at location ~a~%Context:~%  ~a"
                     (parse-failure-offset c)
                     (parse-failure-context c)))))

;; --- Types --- ;;

(deftype maybe-parse ()
  "A parser that might fail."
  '(function (fixnum) (values (or t (member :fail)) fixnum)))

(deftype char-string ()
  '(simple-array character (*)))

(declaim (ftype (function (char-string) fixnum) in))
(defun in (input)
  "Set the global input and yield the initial parser offset."
  (setf +input+ input)
  (setf +input-length+ (length +input+))
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
        (let ((diff (- +input-length+ next)))
          (error 'parse-failure
                 :offset next
                 :context (if (< diff 16)
                              (make-array diff
                                          :element-type 'character
                                          :displaced-to +input+
                                          :displaced-index-offset next)
                              (format nil "~a ... (truncated)"
                                      (make-array 16
                                                  :element-type 'character
                                                  :displaced-to +input+
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
  (or (equal char #\space)
      (equal char #\newline)
      (equal char #\tab)
      (equal char #\return)))
