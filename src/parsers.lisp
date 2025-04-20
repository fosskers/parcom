;;; Fundamental parsers.

(in-package :parcom)

(declaim (ftype maybe-parse any))
(defun any (input)
  "Accept any character."
  (declare (optimize (speed 3) (safety 0)))
  (let ((s (input-str input)))
    (if (empty? s)
        (fail "any char" input)
        (ok (make-array (1- (length s))
                        :element-type 'character
                        :displaced-to s
                        :displaced-index-offset 1)
            (input-head input)))))

#++
(any "hello")
#++
(any "")

(declaim (ftype (function (character) maybe-parse) anybut))
(defun anybut (char)
  "Parser: Any character except the given one."
  (lambda (input)
    (let ((res (any input)))
      (cond ((failure-p res) res)
            ((eql char (parser-value res)) (fail char input))
            (t res)))))

#+nil
(funcall (anybut #\") (in "hi"))
#+nil
(funcall (anybut #\") (in "\"hi"))

(declaim (ftype maybe-parse hex))
(defun hex (input)
  "Parser: A hex character of any case."
  (let ((res (any input)))
    (cond ((failure-p res) res)
          ((hex? (parser-value res)) res)
          (t (fail "hex: 0-9 or A-F" input)))))

#+nil
(funcall (many #'hex) "abcdefgh")

(declaim (ftype maybe-parse eof))
(defun eof (input)
  "Recognize the end of the input."
  (if (empty? (input-str input))
      (ok (input-str input) t)
      (fail "the end of the input" input)))

#++
(eof "hi")
#++
(eof "")

(declaim (ftype (function (character) maybe-parse) char))
(defun char (c)
  "Parse a given character."
  (lambda (input)
    (declare (optimize (speed 3) (safety 0))
             (type input input))
    (if (empty? (input-str input))
        (fail c input)
        (let ((head (input-head input)))
          (if (equal c head)
              (ok (make-array (1- (length (input-str input)))
                              :element-type 'character
                              :displaced-to (input-str input)
                              :displaced-index-offset 1)
                  head)
              (fail c input))))))

#++
(funcall (char #\H) "Hello")
#++
(funcall (char #\H) "ello")
#++
(funcall (*> (char #\H) (char #\e)) "Hello")

(declaim (ftype (function (cl:string) maybe-parse) string))
(defun string (s)
  "Parse the given string."
  (lambda (input)
    (let ((lens (length s))
          (leni (length (input-str input))))
      (if (> lens leni)
          (fail s input)
          (let ((subs (make-array lens
                                  :element-type 'character
                                  :displaced-to (input-str input))))
            (if (equal s subs)
                (ok (make-array (- leni lens)
                                :element-type 'character
                                :displaced-to (input-str input)
                                :displaced-index-offset lens)
                    subs)
                (fail s input)))))))

#++
(funcall (string "") "a")
#++
(funcall (string "Hello") "Hello yes")
#++
(funcall (string "HellO") "Hello yes")

(declaim (ftype (function (fixnum) maybe-parse) take))
(defun take (n)
  "Take `n' characters from the input."
  (lambda (input)
    (let ((s (input-str input)))
      (cond ((< n 0) (error "~a must be a positive number" n))
            ((< (length s) n) (fail "multiple characters" input))
            (t (ok (make-array (- (length s) n)
                               :element-type 'character
                               :displaced-to s
                               :displaced-index-offset n)
                   (make-array n
                               :element-type 'character
                               :displaced-to s)))))))

#+nil
(funcall (take -5) (in "Arbor"))
#+nil
(funcall (take 0) (in "Arbor"))
#+nil
(funcall (take 3) (in "Arbor"))

(declaim (ftype (function ((function (character) boolean)) always-parse) take-while))
(defun take-while (p)
  "Take characters while some predicate holds."
  (lambda (input)
    (declare (optimize (speed 3) (safety 0)))
    (let* ((s   (input-str input))
           (len (length s))
           (keep (loop :for i :from 0 :below len
                       :while (funcall p (cl:char s i))
                       :finally (return i))))
      (ok (make-array (- len keep)
                      :element-type 'character
                      :displaced-to s
                      :displaced-index-offset keep)
          (make-array keep
                      :element-type 'character
                      :displaced-to s)))))

#+nil
(funcall (take-while (lambda (c) (equal #\a c))) "bbb")
#+nil
(funcall (take-while (lambda (c) (equal #\a c))) "aaabbb")

(declaim (ftype (function ((function (character) boolean)) maybe-parse) take-while1))
(defun take-while1 (p)
  "Take characters while some predicate holds. Must succeed at least once."
  (lambda (input)
    (let ((res (funcall (take-while p) input)))
      (cond ((failure-p res) res)
            ((empty? (parser-value res)) (fail "take-while1: at least one success" input))
            (t res)))))

#+nil
(funcall (take-while1 #'digit?) "bob!")
#+nil
(funcall (take-while1 #'digit?) "123!")

(declaim (ftype maybe-parse newline))
(defun newline (input)
  "Matches a single newline character."
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
    (cond ((failure-p res) res)
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
(funcall #'multispace (concatenate 'cl:string '(#\tab #\tab #\tab)))

(declaim (ftype maybe-parse multispace1))
(defun multispace1 (input)
  "Parse 1 or more ASCII whitespace, tabs, newlines, and carriage returns."
  (let ((res (funcall #'multispace input)))
    (cond ((failure-p res) res)
          ((empty? (parser-value res)) (fail "multispace1: at least one space-like character" input))
          (t res))))

#+nil
(funcall #'multispace1 (concatenate 'cl:string '(#\tab #\tab #\tab)))

(declaim (ftype maybe-parse unsigned))
(defun unsigned (input)
  "Parse a positive integer."
  (declare (optimize (speed 3) (safety 0)))
  (let ((res (funcall (take-while1 #'digit?) input)))
    (cond ((failure-p res) res)
          ((and (char-equal #\0 (cl:char (parser-value res) 0))
                (> (length (parser-value res)) 1))
           (fail "unsigned: an integer not starting with 0" input))
          (t (fmap #'parse-integer res)))))

#+nil
(unsigned "0!")
#+nil
(unsigned "0123!")
#+nil
(unsigned "123!")

(declaim (ftype maybe-parse integer))
(defun integer (input)
  "Parse a positive or negative integer."
  (fmap (lambda (pair) (if (null (car pair)) (cdr pair) (- (cdr pair))))
        (funcall (pair (opt (char #\-)) #'unsigned) input)))

#+nil
(integer "123!")
#+nil
(integer "-123!")

(declaim (ftype maybe-parse float))
(defun float (input)
  "Parse a positive or negative floating point number."
  (fmap (lambda (s)
          (let ((*read-default-float-format* 'double-float))
            (read-from-string s)))
        (funcall (recognize (*> #'integer (opt (*> (char #\.) (take-while1 #'digit?))))) input)))

#+nil
(funcall #'float "-123.0456!")
#+nil
(funcall #'float "123.0456!")
#+nil
(funcall #'float "123.0456123123123123!")
#+nil
(funcall #'float "1")

(declaim (ftype always-parse rest))
(defun rest (input)
  "Consume the rest of the input. Always succeeds."
  (ok "" (input-str input)))

#+nil
(funcall (<*> (string "hi") (*> #'space #'rest)) (in "hi there"))
