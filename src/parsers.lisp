;;; Fundamental parsers.

(in-package :parcom)

(declaim (ftype (function (cl:string) (or parser failure)) any))
(defun any (input)
  (if (empty? input)
      (fail "any char" "end of input")
      (ok (make-array (1- (length input))
                      :element-type 'character
                      :displaced-to input
                      :displaced-index-offset 1)
          (aref input 0))))

#++
(any "hello")
#++
(any "")

(declaim (ftype (function (cl:string) (or parser failure)) eof))
(defun eof (input)
  (if (empty? input)
      (ok input t)
      (fail "the end of the input" input)))

#++
(eof "hi")
#++
(eof "")

(declaim (ftype (function (character) (function (cl:string) (or parser failure))) char))
(defun char (c)
  "Parse a given character."
  (lambda (input)
    (if (empty? input)
        (fail (format nil "character: ~a" c) "end of input")
        (let ((head (aref input 0)))
          (if (equal c head)
              (ok (make-array (1- (length input))
                              :element-type 'character
                              :displaced-to input
                              :displaced-index-offset 1)
                  head)
              (fail (format nil "character: ~a" c)
                  (format nil "character: ~a" head)))))))

#++
(funcall (char #\H) "Hello")
#++
(funcall (char #\H) "ello")
#++
(funcall (*> (char #\H) (char #\e)) "Hello")

(defun string (s)
  "Parse the given string."
  (lambda (input)
    (let ((lens (length s))
          (leni (length input)))
      (if (> lens leni)
          (fail (format nil "string: ~a" s)
              (format nil "string: ~a" input))
          (let ((subs (make-array lens
                                  :element-type 'character
                                  :displaced-to input)))
            (if (equal s subs)
                (ok (make-array (- leni lens)
                                :element-type 'character
                                :displaced-to input
                                :displaced-index-offset lens)
                    subs)
                (fail (format nil "string: ~a" s)
                    (format nil "string: ~a" subs))))))))

#++
(funcall (string "") "a")
#++
(funcall (string "Hello") "Hello yes")
#++
(funcall (string "HellO") "Hello yes")

(defun take (n)
  "Take `n' characters from the input."
  (lambda (input)
    (cond ((< n 0) (error "~a must be a positive number" n))
          ((< (length input) n) (fail (format nil "take: ~a characters" n) input))
          (t (ok (make-array (- (length input) n)
                             :element-type 'character
                             :displaced-to input
                             :displaced-index-offset n)
                 (make-array n
                             :element-type 'character
                             :displaced-to input))))))

#+nil
(funcall (take -5) "Arbor")
#+nil
(funcall (take 0) "Arbor")
#+nil
(funcall (take 3) "Arbor")

(defun take-while (p)
  "Take characters while some predicate holds."
  (lambda (input)
    (let ((len (length input)))
      (labels ((recurse (n)
                 (cond ((>= n len) len)
                       ((funcall p (aref input n)) (recurse (1+ n)))
                       (t n))))
        (let ((keep (recurse 0)))
          (ok (make-array (- len keep)
                          :element-type 'character
                          :displaced-to input
                          :displaced-index-offset keep)
              (make-array keep
                          :element-type 'character
                          :displaced-to input)))))))

#+nil
(funcall (take-while (lambda (c) (equal #\a c))) "bbb")
#+nil
(funcall (take-while (lambda (c) (equal #\a c))) "aaabbb")

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

(defun newline (input)
  "Matches a single newline character."
  (funcall (char #\newline) input))

#+nil
(newline "Hello")
#+nil
(let ((s "!Hello"))
  (setf (aref s 0) #\newline)
  (newline s))

(defun space0 (input)
  "Parse 0 or more ASCII whitespace and tab characters."
  (funcall (take-while (lambda (c) (or (equal c #\space) (equal c #\tab)))) input))

#+nil
(funcall #'space0 "   hi")

(defun space1 (input)
  "Parse 1 or more ASCII whitespace and tab characters."
  (let ((res (funcall #'space0 input)))
    (cond ((failure-p res) res)
          ((empty? (parser-value res)) (fail "space1: at least one whitespace" input))
          (t res))))

#+nil
(funcall #'space1 "abc")
#+nil
(funcall #'space1 "   abc")

(defun multispace0 (input)
  "Parse 0 or more ASCII whitespace, tabs, newlines, and carriage returns."
  (funcall (take-while (lambda (c)
                         (or (equal c #\space)
                             (equal c #\tab)
                             (equal c #\newline)
                             (equal c #\return))))
           input))

#+nil
(funcall #'multispace0 (concatenate 'cl:string '(#\tab #\tab #\tab)))

(defun multispace1 (input)
  "Parse 1 or more ASCII whitespace, tabs, newlines, and carriage returns."
  (let ((res (funcall #'multispace0 input)))
    (cond ((failure-p res) res)
          ((empty? (parser-value res)) (fail "multispace1: at least one space-like character" input))
          (t res))))

#+nil
(funcall #'multispace1 (concatenate 'cl:string '(#\tab #\tab #\tab)))

;; TODO: 2025-03-31 float

(defun unsigned (input)
  "Parse a positive integer."
  (fmap #'read-from-string (funcall (take-while1 #'digit?) input)))

#+nil
(unsigned "123!")

(defun integer (input)
  "Parse a positive or negative integer."
  (fmap (lambda (pair) (if (null (car pair)) (nth 1 pair) (- (nth 1 pair))))
        (funcall (<*> (opt (char #\-)) #'unsigned) input)))

#+nil
(integer "123!")
#+nil
(integer "-123!")
