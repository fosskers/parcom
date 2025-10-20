;;; Fundamental parsers.

(in-package :parcom)

(defparameter +empty-string+ "")

(fn pure (-> t (always t)))
(defun pure (item)
  "Parser: Parse nothing and just yield the given value."
  (lambda (offset)
    (values item offset)))

#+nil
(parse (pure :pāx) "hello")

(fn any (maybe character))
(defun any (offset)
  "Accept any character."
  (declare (optimize (speed 3) (safety 0)))
  (if (>= offset *input-length*)
      (fail offset)
      (values (schar *input* offset) (off 1 offset))))

#++
(any (in "hello"))
#++
(any (in ""))

(fn any-but (-> character (maybe character)))
(defun any-but (c)
  "Parser: Any character except the given one."
  (lambda (offset)
    (multiple-value-bind (res next) (any offset)
      (cond ((failure? res) (fail next))
            ((eql c res) (fail offset))
            (t (values res next))))))

#+nil
(funcall (any-but #\") (in "hi"))
#+nil
(funcall (any-but #\") (in "\"hi"))

(fn any-if (-> (-> character boolean) (maybe character)))
(defun any-if (pred)
  "Parser: Any character, as long as it passes the predicate."
  (lambda (offset)
    (if (>= offset *input-length*)
        (fail offset)
        (let ((c (schar *input* offset)))
          (if (funcall pred c)
              (values c (off 1 offset))
              (fail offset))))))

#+nil
(funcall (any-if #'digit?) (in "8a"))

(fn hex (maybe character))
(defun hex (offset)
  "Parser: A hex character of any case."
  (multiple-value-bind (res next) (any offset)
    (cond ((failure? res) (fail next))
          ((hex? res) (values res next))
          (t (fail next)))))

#+nil
(funcall #'hex (in "abcdefgh"))
#+nil
(funcall (many #'hex) (in "abcdefgh"))

(fn unicode (maybe character))
(defun unicode (offset)
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
                 offset)))

#+nil
(unicode (in "\\u0022"))
#+nil
(unicode (in "\\U0022"))

(fn control-char (maybe character))
(defun control-char (offset)
  "Parser: Newlines and whatnot."
  (funcall (*> (char #\\)
               (alt (<$ #\newline (char #\n))
                    (<$ #\tab (char #\t))
                    (<$ #\return (char #\r))
                    (<$ #\backspace (char #\b))
                    (<$ #\page (char #\f))))
           offset))

#+nil
(control-char (in "\\n"))

(fn eof (maybe t))
(defun eof (offset)
  "Parser: Recognize the end of the input."
  (if (= offset *input-length*)
      (values t offset)
      (fail offset)))

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

(fn char (-> character (maybe character)))
(defun char (c)
  "Parse a given character."
  (lambda (offset)
    (declare (optimize (speed 3) (safety 0)))
    (declare (type fixnum offset))
    (if (>= offset *input-length*)
        (fail offset)
        (let ((head (schar *input* offset)))
          (if (equal c head)
              (ok (off 1 offset) head)
              (fail offset))))))

#++
(funcall (char #\H) (in ""))
#++
(funcall (char #\H) (in "Hello"))
#++
(funcall (char #\H) (in "ello"))
#++
(funcall (*> (char #\H) (char #\e)) (in "Hello"))

(fn string (-> cl:string (maybe cl:string)))
(defun string (s)
  "Parser: Parse a given string. Yields the original string itself if parsing was
successful, in order to save on memory."
  (lambda (offset)
    (declare (optimize (speed 3) (safety 0)))
    (declare (type fixnum offset))
    (let ((i (if (>= offset *input-length*)
                 0
                 (loop :for i fixnum :from 0 :below (length s)
                       :while (char= (schar s i) (schar *input* (+ i offset)))
                       :finally (return i)))))
      (if (= i (length s))
          (ok (off (length s) offset) s)
          (fail offset)))))

#+nil
(funcall (string "Pāstor") (in ""))
#++
(funcall (string "") (in "a"))
#++
(funcall (string "Hēllo") (in "Hēllo yes"))
#++
(funcall (string "HellO") (in "Hello yes"))

(fn take (-> fixnum (always cl:string)))
(defun take (n)
  "Take `n' characters from the input. Lenient, in that if `n' is larger than the
remaining amount of characters, only the remaining ones will be yielded."
  (lambda (offset)
    (cond ((< n 0) (error "~a must be a positive number" n))
          ((zerop n) (ok offset +empty-string+))
          (t (let ((m (min n (- *input-length* offset))))
               (ok (off m offset)
                   (make-array m
                               :element-type 'character
                               :displaced-to *input*
                               :displaced-index-offset offset)))))))

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

(declaim (ftype (function ((function (character) boolean) &key (:id (or keyword null))) (function (fixnum) (values fixnum fixnum))) consume))
(defun consume (p &key (id nil))
  "Skip characters according to a given predicate, advancing the parser to a
further point. Yields the new offset (i.e. how far it got), not the characters
that were parsed. A faster variant of `take-while' when you don't actually need
the parsed characters, and `skip' when you don't need to parse something
complex."
  (declare (ignore id))
  (lambda (offset)
    (declare (optimize (speed 3) (safety 0)))
    (let* ((keep (loop :for i fixnum :from offset :below *input-length*
                       :while (funcall p (schar *input* i))
                       :finally (return (- i offset))))
           (next (off keep offset)))
      (ok next next))))

#+nil
(funcall (consume (lambda (c) (eql c #\a))) (in "aaabcd!"))

(fn consume1 (-> (-> character boolean) (maybe fixnum)))
(defun consume1 (p)
  "Like `consume', but guarantees that at least one check of the predicate must succeed."
  (lambda (offset)
    (declare (optimize (speed 3) (safety 0)))
    (let* ((keep (loop :for i fixnum :from offset :below *input-length*
                       :while (funcall p (schar *input* i))
                       :finally (return (- i offset))))
           (next (off keep offset)))
      (if (= offset next)
          (fail offset)
          (ok next next)))))

#+nil
(funcall (consume1 (lambda (c) (eql c #\!))) (in "aaabcd!"))

(fn take-while (-> (-> character boolean) (always cl:string)))
(defun take-while (p)
  "Parser: Take characters while some predicate holds."
  (lambda (offset)
    (declare (optimize (speed 3) (safety 0)))
    (let ((keep (loop :for i fixnum :from offset :below *input-length*
                      :while (funcall p (schar *input* i))
                      :finally (return (- i offset)))))
      (ok (off keep offset)
          (if (zerop keep)
              +empty-string+
              (make-array keep
                          :element-type 'character
                          :displaced-to *input*
                          :displaced-index-offset offset))))))

#+nil
(funcall (take-while (lambda (c) (eql #\a c))) (in "bbb"))
#+nil
(funcall (take-while (lambda (c) (eql #\a c))) (in "aaabcd"))
#+nil
(funcall (*> (take-while (lambda (c) (eql #\a c)))
             (take-while (lambda (c)
                           (or (eql #\b c)
                               (eql #\c c)
                               (eql #\d c)))))
         (in "aaabcd!"))

(fn take-while1 (-> (-> character boolean) (maybe cl:string)))
(defun take-while1 (p)
  "Parser: Take characters while some predicate holds. Must succeed at least once."
  (lambda (offset)
    (declare (optimize (speed 3) (safety 0)))
    (let ((keep (loop :for i fixnum :from offset :below *input-length*
                      :while (funcall p (schar *input* i))
                      :finally (return (- i offset)))))
      (ok (off keep offset)
          (if (zerop keep)
              (fail offset)
              (make-array keep
                          :element-type 'character
                          :displaced-to *input*
                          :displaced-index-offset offset))))))

#+nil
(funcall (take-while1 #'digit?) (in "bob!"))
#+nil
(funcall (take-while1 #'digit?) (in "123!"))

(fn newline (maybe character))
(defun newline (offset)
  "Parser: Matches a single newline character."
  (funcall (char #\newline) offset))

#+nil
(newline (in "Hello"))

(fn space (always cl:string))
(defun space (offset)
  "Parse 0 or more ASCII whitespace and tab characters."
  (funcall (take-while (lambda (c) (or (eql c #\space) (eql c #\tab)))) offset))

#+nil
(funcall #'space (in "   hi"))

(fn space1 (maybe cl:string))
(defun space1 (offset)
  "Parse 1 or more ASCII whitespace and tab characters."
  (multiple-value-bind (res next) (space offset)
    (cond ((failure? res) (fail offset))
          ((empty? res) (fail offset))
          (t (values res next)))))

#+nil
(funcall #'space1 (in "abc"))
#+nil
(funcall #'space1 (in "   abc"))

(fn multispace (always cl:string))
(defun multispace (offset)
  "Parse 0 or more ASCII whitespace, tabs, newlines, and carriage returns."
  (funcall (take-while (lambda (c)
                         (or (eql c #\space)
                             (eql c #\tab)
                             (eql c #\newline)
                             (eql c #\return))))
           offset))

#+nil
(funcall #'multispace (in (concatenate 'cl:string '(#\tab #\tab #\tab))))

(fn multispace1 (maybe cl:string))
(defun multispace1 (offset)
  "Parse 1 or more ASCII whitespace, tabs, newlines, and carriage returns."
  (multiple-value-bind (res next) (multispace offset)
    (cond ((failure? res) (fail offset))
          ((empty? res) (fail offset))
          (t (values res next)))))

#+nil
(funcall #'multispace1 (in (concatenate 'cl:string '(#\tab #\tab #\tab))))

(fn unsigned (maybe fixnum))
(defun unsigned (offset)
  "Parser: A positive integer."
  (declare (optimize (speed 3) (safety 0)))
  (multiple-value-bind (res next) (funcall (take-while1 #'digit?) offset)
    (cond ((failure? res) (fail offset))
          ((and (char-equal #\0 (cl:char res 0))
                (> (length res) 1))
           (fail offset))
          (t (values (parse-integer res) next)))))

#+nil
(unsigned (in "0!"))
#+nil
(unsigned (in "0123!"))
#+nil
(unsigned (in "123!"))

(defparameter +minus+ (char #\-))

(defmacro integer-parser ()
  "A trick to enable efficient JVM optimizations."
  `(maybe #'- +minus+ #'unsigned))

#-abcl
(defparameter +integer+ (integer-parser))

(fn integer (maybe fixnum))
(defun integer (offset)
  "Parser: A positive or negative integer."
  #-abcl
  (funcall +integer+ offset)
  #+abcl
  (funcall (integer-parser) offset))

(fn float (maybe double-float))
(defun float (offset)
  "Parser: A positive or negative floating point number."
  (fmap (lambda (s)
          (let ((*read-default-float-format* 'double-float))
            (cl:float (read-from-string s) 1.0d0)))
        (funcall (recognize (*> #'integer (opt (*> (char #\.) (take-while1 #'digit?))))) offset)))

#+nil
(funcall #'float (in "-123.0456!"))
#+nil
(funcall #'float (in "123.0456!"))
#+nil
(funcall #'float (in "123.0456123123123123!"))
#+nil
(funcall #'float (in "1"))

(fn rest (always cl:string))
(defun rest (offset)
  "Parser: Consume the rest of the input. Always succeeds."
  (let ((len (- *input-length* offset)))
    (ok (off len offset)
        (make-array len
                    :element-type 'character
                    :displaced-to *input*
                    :displaced-index-offset offset))))

#+nil
(rest (in "hello"))
#+nil
(funcall (<*> (string "hi") (*> #'space #'rest)) (in "hi there"))
