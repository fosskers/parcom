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
    (if (< (length input) n)
        (fail (format nil "take: ~a characters" n) input)
        (ok (make-array (- (length input) n)
                        :element-type 'character
                        :displaced-to input
                        :displaced-index-offset n)
            (make-array n
                        :element-type 'character
                        :displaced-to input)))))

#+nil
(funcall (take 0) "Arbor")
#+nil
(funcall (take 3) "Arbor")

;; TODO: 2025-03-31 take-while, digits, newline, space0, space1, multispace0, multispace1
