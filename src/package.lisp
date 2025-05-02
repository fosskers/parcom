(defpackage parcom
  (:use :cl)
  (:shadow #:char #:string #:integer #:float #:count #:rest #:space)
  ;; --- Types --- ;;
  (:export #:parser #:parser? #:parser-input #:parser-value
           #:failure #:failure? #:failure-expected #:failure-actual
           #:input #:in
           #:ok #:fail #:parse
           #:empty?
           #:digit? #:hex? #:octal? #:binary?
           #:ascii-letter? #:space?)
  ;; --- Functional Programming --- ;;
  (:export #:fmap #:const
           #:all #:right #:left #:instead
           #:*> #:<* #:<*> #:<$ #:alt)
  ;; --- Parsers --- ;;
  (:export #:any #:any-but #:anybut #:any-if #:hex #:unicode #:control-char #:eof
           #:char #:string
           #:unsigned #:integer #:float
           #:newline #:space #:space1 #:multispace #:multispace1
           #:take #:take-while #:take-while1 #:rest)
  ;; --- Combinators --- ;;
  (:export #:opt #:between #:pair
           #:many #:many1 #:sep #:sep1 #:sep-end #:sep-end1 #:take-until
           #:consume #:skip #:peek #:count #:recognize)
  ;; --- Conditions --- ;;
  (:export #:parse-failure)
  (:documentation "A simple parser combinator library."))

(in-package :parcom)

;; --- Conditions --- ;;

(define-condition parse-failure (error)
  ((expected :initarg :expected :reader parse-failure-expected)
   (actual   :initarg :actual   :reader parse-failure-actual))
  (:documentation "Some parsing failed, so we render why.")
  (:report (lambda (c stream)
             (format stream "Expected:~%  ~a~%Actual:~%  ~a~%"
                     (parse-failure-expected c)
                     (parse-failure-actual c)))))

;; --- Types --- ;;

(defstruct input
  "The remaining parser input with a cached reference to its first character."
  (curr 0   :type fixnum)
  (str  nil :type simple-string))

(declaim (ftype (function (simple-string) input) in))
(defun in (input)
  "Smart constructor for some parser input."
  (declare (optimize (speed 3) (safety 0)))
  (make-input :curr 0 :str input))

#+nil
(in "hello")

(declaim (ftype (function (fixnum input) input) off))
(defun off (offset input)
  "Advance the input by some offset."
  (declare (optimize (speed 3) (safety 0)))
  (make-input :curr (+ offset (input-curr input))
              :str  (input-str input)))

#+nil
(off 4 (in "hello there!"))

(deftype maybe-parse ()
  "A parser that might fail."
  '(function (input) (or parser cons)))

(deftype always-parse ()
  "A parser that always succeeds."
  '(function (input) parser))

(defstruct (parser (:predicate parser?))
  "The result of some successful parsing. Tracks the remaining input."
  (input nil :type input)
  value)

(declaim (ftype (function (input t) parser) ok))
(defun ok (input value)
  "Some successful parsing!"
  (make-parser :input input :value value))

(defmacro fail (loc act)
  "It's assumed that you pass back the entire remaining input as the 'actual' value.
Error reporting code will check the length of this and truncate it if necessary."
  `(cons ,loc ,act))

(defun failure? (x)
  (consp x))

(defmacro failure-actual (x)
  `(cdr ,x))

(defmacro failure-expected (x)
  `(car ,x))

(defun parse (parser input)
  "Run a parser and attempt to extract its final value."
  (let ((res (funcall parser (in input))))
    (etypecase res
      (parser  (parser-value res))
      (cons (let* ((rem (failure-actual res))
                   (diff (- (length (input-str rem)) (input-curr rem))))
              (error 'parse-failure
                     :expected (failure-expected res)
                     :actual (if (< diff 16)
                                 (make-array diff
                                             :element-type 'character
                                             :displaced-to (input-str rem)
                                             :displaced-index-offset (input-curr rem))
                                 (format nil "~a ... (truncated)"
                                         (make-array 16
                                                     :element-type 'character
                                                     :displaced-to (input-str rem)
                                                     :displaced-index-offset (input-curr rem))))))))))

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
