;;; Fundamental parsers.

(in-package :parcom)

(defparameter +empty-string+ "")

(declaim (ftype (function (fixnum) (values (or character (member :fail)) fixnum)) any))
(defun any (offset)
  "Accept any character."
  (declare (optimize (speed 3) (safety 0)))
  (if (>= offset +input-length+)
      (fail offset)
      (values (schar +input+ offset) (off 1 offset))))

#++
(any (in "hello"))
#++
(any (in ""))

(declaim (ftype (function (character) (function (fixnum) (values (or character (member :fail)) fixnum))) any-but))
(defun any-but (c)
  "Parser: Any character except the given one."
  (or (gethash c +any-but-cache+)
      (let ((f (lambda (offset)
                 (multiple-value-bind (res next) (any offset)
                   (cond ((failure? res) res)
                         ((eql c res) (fail next))
                         (t (values res next)))))))
        (setf (gethash c +any-but-cache+) f)
        f)))

#+nil
(funcall (any-but #\") (in "hi"))
#+nil
(funcall (any-but #\") (in "\"hi"))

(declaim (ftype (function ((function (character) boolean)) (function (fixnum) (values (or character (member :fail)) fixnum))) any-if))
(defun any-if (pred)
  "Parser: Any character, as long as it passes the predicate."
  (lambda (offset)
    (multiple-value-bind (res next) (any offset)
      (cond ((failure? res) res)
            ((funcall pred res) (values res next))
            (t (fail next))))))

#+nil
(funcall (any-if #'digit?) (in "8a"))

(declaim (ftype (function (fixnum) (values (or character (member :fail)) fixnum)) hex))
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

(declaim (ftype (function (fixnum) (values (or character (member :fail)) fixnum)) unicode))
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

(declaim (ftype (function (fixnum) (values (or character (member :fail)) fixnum)) control-char))
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

(declaim (ftype (function (fixnum) (values (or t (member :fail)) fixnum)) eof))
(defun eof (offset)
  "Parser: Recognize the end of the input."
  (if (= offset +input-length+)
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

(declaim (ftype (function (character) (function (fixnum) (values (or character (member :fail)) fixnum))) char))
(defun char (c)
  "Parse a given character."
  (or (gethash c +char-cache+)
      (let ((f (lambda (offset)
                 (declare (optimize (speed 3) (safety 0)))
                 (if (>= offset +input-length+)
                     (fail offset)
                     (let ((head (schar +input+ offset)))
                       (if (equal c head)
                           (ok (off 1 offset) head)
                           (fail offset)))))))
        (setf (gethash c +char-cache+) f)
        f)))

#++
(funcall (char #\H) (in ""))
#++
(funcall (char #\H) (in "Hello"))
#++
(funcall (char #\H) (in "ello"))
#++
(funcall (*> (char #\H) (char #\e)) (in "Hello"))

(declaim (ftype (function (simple-string) (function (fixnum) (values (or simple-string (member :fail)) fixnum))) string))
(defun string (s)
  "Parser: Parse a given string. Yields the original string itself if parsing was
successful, in order to save on memory."
  (lambda (offset)
    (declare (optimize (speed 3) (safety 0)))
    (let ((i (if (>= offset +input-length+)
                 0
                 (loop :for i :from 0 :below (length s)
                       :while (equal (schar s i) (schar +input+ (+ i offset)))
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

(declaim (ftype (function (fixnum) (function (fixnum) (values (or cl:string (member :fail)) fixnum))) take))
(defun take (n)
  "Take `n' characters from the input. Lenient, in that if `n' is larger than the
remaining amount of characters, only the remaining ones will be yielded."
  (lambda (offset)
    (cond ((< n 0) (error "~a must be a positive number" n))
          ((zerop n) (ok offset +empty-string+))
          (t (let ((m (min n (- +input-length+ offset))))
               (ok (off m offset)
                   (make-array m
                               :element-type 'character
                               :displaced-to +input+
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

(declaim (ftype (function ((function (character) boolean) &key (:id (or keyword null))) (function (fixnum) (values t fixnum))) consume))
(defun consume (p &key (id nil))
  "Skip characters according to a given predicate, advancing the parser to a
further point. Yields T, not the characters that were parsed. A faster variant
of `take-while' when you don't actually need the parsed characters, and `skip'
when you don't need to parse something complex."
  (or (gethash id +consume-cache+)
      (let ((f (lambda (offset)
                 (declare (optimize (speed 3) (safety 0)))
                 (let ((keep (loop :for i :from offset :below +input-length+
                                   :while (funcall p (schar +input+ i))
                                   :finally (return (- i offset)))))
                   (ok (off keep offset) t)))))
        (when id
          (setf (gethash id +consume-cache+) f))
        f)))

#+nil
(funcall (consume (lambda (c) (eql c #\a))) (in "aaabcd!"))

(declaim (ftype (function ((function (character) boolean)) (function (fixnum) (values cl:string fixnum))) take-while))
(defun take-while (p)
  "Parser: Take characters while some predicate holds."
  (lambda (offset)
    (declare (optimize (speed 3) (safety 0)))
    (let ((keep (loop :for i :from offset :below +input-length+
                      :while (funcall p (schar +input+ i))
                      :finally (return (- i offset)))))
      (ok (off keep offset)
          (if (zerop keep)
              +empty-string+
              (make-array keep
                          :element-type 'character
                          :displaced-to +input+
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

(declaim (ftype (function ((function (character) boolean)) (function (fixnum) (values (or cl:string (member :fail)) fixnum))) take-while1))
(defun take-while1 (p)
  "Parser: Take characters while some predicate holds. Must succeed at least once."
  (lambda (offset)
    (multiple-value-bind (res next) (funcall (take-while p) offset)
      (cond ((failure? res) (fail offset))
            ((empty? res) (fail offset))
            (t (values res next))))))

#+nil
(funcall (take-while1 #'digit?) (in "bob!"))
#+nil
(funcall (take-while1 #'digit?) (in "123!"))

(declaim (ftype (function (fixnum) (values (or character (member :fail)) fixnum)) newline))
(defun newline (offset)
  "Parser: Matches a single newline character."
  (funcall (char #\newline) offset))

#+nil
(newline (in "Hello"))

(declaim (ftype (function (fixnum) (values cl:string fixnum)) space))
(defun space (offset)
  "Parse 0 or more ASCII whitespace and tab characters."
  (funcall (take-while (lambda (c) (or (eql c #\space) (eql c #\tab)))) offset))

#+nil
(funcall #'space (in "   hi"))

(declaim (ftype (function (fixnum) (values (or cl:string (member :fail)) fixnum)) space1))
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

(declaim (ftype (function (fixnum) (values cl:string fixnum)) multispace))
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

(declaim (ftype (function (fixnum) (values (or cl:string (member :fail)) fixnum)) multispace1))
(defun multispace1 (offset)
  "Parse 1 or more ASCII whitespace, tabs, newlines, and carriage returns."
  (multiple-value-bind (res next) (multispace offset)
    (cond ((failure? res) (fail offset))
          ((empty? res) (fail offset))
          (t (values res next)))))

#+nil
(funcall #'multispace1 (in (concatenate 'cl:string '(#\tab #\tab #\tab))))

(declaim (ftype (function (fixnum) (values (or fixnum (member :fail)) fixnum)) unsigned))
(defun unsigned (offset)
  "Parser: A positive integer."
  (declare (optimize (speed 3) (safety 0)))
  (multiple-value-bind (res next) (funcall (take-while1 #'digit?) offset)
    (cond ((failure? res) (fail offset))
          ((and (char-equal #\0 (cl:char res 0))
                (> (length res) 1))
           (fail offset))
          (t (fmap #'parse-integer (values res next))))))

#+nil
(unsigned (in "0!"))
#+nil
(unsigned (in "0123!"))
#+nil
(unsigned (in "123!"))

(declaim (ftype (function (fixnum) (values (or fixnum (member :fail)) fixnum)) integer))
(defun integer (offset)
  "Parser: A positive or negative integer."
  (fmap (lambda (pair) (if (null (car pair)) (cadr pair) (- (cadr pair))))
        (funcall (<*> (opt (char #\-)) #'unsigned) offset)))

#+nil
(integer (in "123!"))
#+nil
(integer (in "-123!"))

(declaim (ftype (function (fixnum) (values (or double-float (member :fail)) fixnum)) float))
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

(declaim (ftype (function (fixnum) (values cl:string fixnum)) rest))
(defun rest (offset)
  "Parser: Consume the rest of the input. Always succeeds."
  (let ((len (- +input-length+ offset)))
    (ok (off len offset)
        (make-array len
                    :element-type 'character
                    :displaced-to +input+
                    :displaced-index-offset offset))))

#+nil
(rest (in "hello"))
#+nil
(funcall (<*> (string "hi") (*> #'space #'rest)) (in "hi there"))
