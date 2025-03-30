(in-package :parcom)

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

(defun eof (input)
  (if (empty? input)
      (ok input t)
      (fail "the end of the input" input)))

#++
(eof "hi")
#++
(eof "")
