;;; Fundamental parsers.

(in-package :parcom)

(defparameter +empty-string+ "")

(declaim (ftype maybe-parse any))
(defun any (input)
  "Accept any character."
  (declare (optimize (speed 3) (safety 0)))
  (let ((s (input-str input)))
    (if (>= (input-curr input) (length s))
        (fail "any char" input)
        (ok (off 1 input) (schar s (input-curr input))))))

#++
(any (in "hello"))
#++
(any (in ""))

(defmacro anybut (char)
  "Deprecated: Use `any-but'."
  (warn "`anybut' is deprecated; use `any-but' instead.")
  `(any-but ,char))

(declaim (ftype (function (character) maybe-parse) any-but))
(defun any-but (char)
  "Parser: Any character except the given one."
  (lambda (input)
    (let ((res (any input)))
      (cond ((failure? res) res)
            ((eql char (parser-value res)) (fail char input))
            (t res)))))

#+nil
(funcall (any-but #\") (in "hi"))
#+nil
(funcall (any-but #\") (in "\"hi"))

(defun any-if (pred)
  "Parser: Any character, as long as it passes the predicate."
  (lambda (input)
    (let ((res (any input)))
      (cond ((failure? res) res)
            ((funcall pred (parser-value res)) res)
            (t (fail "any-if: should have passed the predicate" input))))))

#+nil
(funcall (any-if #'digit?) (in "8a"))

(declaim (ftype maybe-parse hex))
(defun hex (input)
  "Parser: A hex character of any case."
  (let ((res (any input)))
    (cond ((failure? res) res)
          ((hex? (parser-value res)) res)
          (t (fail "hex: 0-9 or A-F" input)))))

#+nil
(funcall (many #'hex) (in "abcdefgh"))

(defun unicode (input)
  "Parser: Parse a unicode char of 4 hex values."
  (fmap (lambda (chars)
          (destructuring-bind (a b c d) chars
            (code-char (+ (* 4096 (digit-char-p a 16))
                          (* 256 (digit-char-p b 16))
                          (* 16 (digit-char-p c 16))
                          (digit-char-p d 16)))))
        (funcall (*> (char #\\)
                     (alt (char #\u) (char #\U))
                     (count 4 #'hex))
                 input)))

#+nil
(unicode (in "\\u0022"))
#+nil
(unicode (in "\\U0022"))

(defun control-char (input)
  "Parser: Newlines and whatnot."
  (funcall (*> (char #\\)
               (alt (<$ #\newline (char #\n))
                    (<$ #\tab (char #\t))
                    (<$ #\return (char #\r))
                    (<$ #\backspace (char #\b))
                    (<$ #\page (char #\f))))
           input))

#+nil
(control-char (in "\\n"))

(declaim (ftype maybe-parse eof))
(defun eof (input)
  "Parser: Recognize the end of the input."
  (if (= (input-curr input) (length (input-str input)))
      (ok input t)
      (fail "the end of the input" input)))

#++
(eof (in "hi"))
#++
(eof (in ""))
#+nil
(parse (*> (string "Mālum") #'eof) "Mālum")
#+nil
(parse (*> (string "Mālum") (char #\,)) "Mālum")
#+nil
(funcall (*> (string "Mālum") (char #\,)) (in "Mālum"))

(declaim (ftype (function (character) maybe-parse) char))
(defun char (c)
  "Parse a given character."
  (lambda (input)
    (declare (optimize (speed 3) (safety 0))
             (type input input))
    (if (>= (input-curr input) (length (input-str input)))
        (fail c input)
        (let ((head (schar (input-str input) (input-curr input))))
          (if (equal c head)
              (ok (off 1 input) head)
              (fail c input))))))

#++
(funcall (char #\H) (in ""))
#++
(funcall (char #\H) (in "Hello"))
#++
(funcall (char #\H) (in "ello"))
#++
(funcall (*> (char #\H) (char #\e)) (in "Hello"))

(declaim (ftype (function (simple-string) maybe-parse) string))
(defun string (s)
  "Parser: Parse a given string. Yields the original string itself if parsing was
successful, in order to save on memory."
  (lambda (input)
    (declare (optimize (speed 3) (safety 0)))
    (let* ((off (input-curr input))
           (ins (input-str input))
           (i (if (>= off (length ins))
                  0
                  (loop :for i :from 0 :below (length s)
                        :while (equal (schar s i) (schar ins (+ i off)))
                        :finally (return i)))))
      (if (= i (length s))
          (ok (off (length s) input) s)
          (fail s input)))))

#+nil
(funcall (string "Pāstor") (in ""))
#++
(funcall (string "") (in "a"))
#++
(funcall (string "Hēllo") (in "Hēllo yes"))
#++
(funcall (string "HellO") (in "Hello yes"))

(declaim (ftype (function (fixnum) maybe-parse) take))
(defun take (n)
  "Take `n' characters from the input. Lenient, in that if `n' is larger than the
remaining amount of characters, only the remaining ones will be yielded."
  (lambda (input)
    (let ((s (input-str input)))
      (cond ((< n 0) (error "~a must be a positive number" n))
            ((zerop n) (ok input +empty-string+))
            (t (let ((m (min n (- (length s) (input-curr input)))))
                 (ok (off m input)
                     (make-array m
                                 :element-type 'character
                                 :displaced-to s
                                 :displaced-index-offset (input-curr input)))))))))

#+nil
(funcall (take -5) (in "Arbor"))
#+nil
(funcall (take 0) (in "Arbor"))
#+nil
(funcall (take 3) (in "Arbor"))
#+nil
(funcall (take 100) (in "Arbor"))
#+nil
(funcall (*> (take 3) (take 2)) (in "Arbor"))

(declaim (ftype (function ((function (character) boolean)) always-parse) consume))
(defun consume (p)
  "Skip characters according to a given predicate, advancing the parser to a
further point. Yields T, not the characters that were parsed. A faster variant
of `take-while' when you don't actually need the parsed characters, and `skip'
when you don't need to parse something complex."
  (lambda (input)
    (declare (optimize (speed 3) (safety 0))
             (type input input))
    (let* ((s    (input-str input))
           (len  (length s))
           (keep (loop :for i :from (input-curr input) :below len
                       :while (funcall p (schar s i))
                       :finally (return (- i (input-curr input))))))
      (ok (off keep input) t))))

#+nil
(funcall (consume (lambda (c) (eql c #\a))) (in "aaabcd!"))

(declaim (ftype (function ((function (character) boolean)) always-parse) take-while))
(defun take-while (p)
  "Parser: Take characters while some predicate holds."
  (lambda (input)
    (declare (optimize (speed 3) (safety 0))
             (type input input))
    (let* ((s    (input-str input))
           (len  (length s))
           (keep (loop :for i :from (input-curr input) :below len
                       :while (funcall p (schar s i))
                       :finally (return (- i (input-curr input))))))
      (ok (off keep input)
          (if (zerop keep)
              +empty-string+
              (make-array keep
                          :element-type 'character
                          :displaced-to s
                          :displaced-index-offset (input-curr input)))))))

#+nil
(funcall (take-while (lambda (c) (equal #\a c))) (in "bbb"))
#+nil
(funcall (take-while (lambda (c) (equal #\a c))) (in "aaabcd"))
#+nil
(funcall (*> (take-while (lambda (c) (equal #\a c)))
             (take-while (lambda (c)
                           (or (equal #\b c)
                               (equal #\c c)
                               (equal #\d c)))))
         (in "aaabcd!"))

(declaim (ftype (function ((function (character) boolean)) maybe-parse) take-while1))
(defun take-while1 (p)
  "Parser: Take characters while some predicate holds. Must succeed at least once."
  (lambda (input)
    (let ((res (funcall (take-while p) input)))
      (cond ((failure? res) res)
            ((empty? (parser-value res)) (fail "take-while1: at least one success" input))
            (t res)))))

#+nil
(funcall (take-while1 #'digit?) "bob!")
#+nil
(funcall (take-while1 #'digit?) "123!")

(declaim (ftype maybe-parse newline))
(defun newline (input)
  "Parser: Matches a single newline character."
  (funcall (char #\newline) input))

#+nil
(newline "Hello")
#+nil
(let ((s "!Hello"))
  (setf (aref s 0) #\newline)
  (newline s))

(declaim (ftype always-parse space))
(defun space (input)
  "Parse 0 or more ASCII whitespace and tab characters."
  (funcall (take-while (lambda (c) (or (equal c #\space) (equal c #\tab)))) input))

#+nil
(funcall #'space "   hi")

(declaim (ftype maybe-parse space1))
(defun space1 (input)
  "Parse 1 or more ASCII whitespace and tab characters."
  (let ((res (funcall #'space input)))
    (cond ((failure? res) res)
          ((empty? (parser-value res)) (fail "space1: at least one whitespace" input))
          (t res))))

#+nil
(funcall #'space1 "abc")
#+nil
(funcall #'space1 "   abc")

(declaim (ftype always-parse multispace))
(defun multispace (input)
  "Parse 0 or more ASCII whitespace, tabs, newlines, and carriage returns."
  (funcall (take-while (lambda (c)
                         (or (equal c #\space)
                             (equal c #\tab)
                             (equal c #\newline)
                             (equal c #\return))))
           input))

#+nil
(funcall #'multispace (in (concatenate 'cl:string '(#\tab #\tab #\tab))))

(declaim (ftype maybe-parse multispace1))
(defun multispace1 (input)
  "Parse 1 or more ASCII whitespace, tabs, newlines, and carriage returns."
  (let ((res (funcall #'multispace input)))
    (cond ((failure? res) res)
          ((empty? (parser-value res)) (fail "multispace1: at least one space-like character" input))
          (t res))))

#+nil
(funcall #'multispace1 (concatenate 'cl:string '(#\tab #\tab #\tab)))

(declaim (ftype maybe-parse unsigned))
(defun unsigned (input)
  "Parser: A positive integer."
  (declare (optimize (speed 3) (safety 0)))
  (let ((res (funcall (take-while1 #'digit?) input)))
    (cond ((failure? res) res)
          ((and (char-equal #\0 (cl:char (parser-value res) 0))
                (> (length (parser-value res)) 1))
           (fail "unsigned: an integer not starting with 0" input))
          (t (fmap #'parse-integer res)))))

#+nil
(unsigned (in "0!"))
#+nil
(unsigned (in "0123!"))
#+nil
(unsigned (in "123!"))

(declaim (ftype maybe-parse integer))
(defun integer (input)
  "Parser: A positive or negative integer."
  (fmap (lambda (pair) (if (null (car pair)) (cdr pair) (- (cdr pair))))
        (funcall (pair (opt (char #\-)) #'unsigned) input)))

#+nil
(integer "123!")
#+nil
(integer "-123!")

(declaim (ftype maybe-parse float))
(defun float (input)
  "Parser: A positive or negative floating point number."
  (fmap (lambda (s)
          (let ((*read-default-float-format* 'double-float))
            (read-from-string s)))
        (funcall (recognize (*> #'integer (opt (*> (char #\.) (take-while1 #'digit?))))) input)))

#+nil
(funcall #'float (in "-123.0456!"))
#+nil
(funcall #'float (in "123.0456!"))
#+nil
(funcall #'float (in "123.0456123123123123!"))
#+nil
(funcall #'float (in "1"))

(declaim (ftype always-parse rest))
(defun rest (input)
  "Parser: Consume the rest of the input. Always succeeds."
  (let ((len (- (length (input-str input)) (input-curr input))))
    (ok (off len input)
        (make-array len
                    :element-type 'character
                    :displaced-to (input-str input)
                    :displaced-index-offset (input-curr input)))))

#+nil
(rest (in "hello"))
#+nil
(funcall (<*> (string "hi") (*> #'space #'rest)) (in "hi there"))
