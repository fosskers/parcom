;;; Functional Programming utilities.

(in-package :parcom)

(defmacro comp (function &rest functions)
  "Function composition."
  (let ((args (gensym "COMP-ARGS-"))
        (reversed (reverse (cons function functions))))
    `(lambda (&rest ,args)
       ,(reduce (lambda (data fn)
                  `(funcall ,fn ,data))
                (cdr reversed)
                :initial-value `(apply ,(car reversed) ,args)))))

#++
(funcall (comp #'1+ #'length) '(1 2 3))

(defgeneric fmap (f thing)
  (:documentation "Apply a pure function to the inner contents of some `thing'."))

(defmethod fmap ((f function) (p parser))
  "Map some `f' over the inner value of a parser."
  (ok (parser-input p)
      (funcall f (parser-value p))))

(defmethod fmap ((f function) (failure failure))
  "Don't map anything, since this was a parse failure."
  failure)

#++
(fmap #'1+ (ok "" 1))
#++
(fmap #'1+ (fail "greatness" "failure"))

(defun const (x)
  "Yield a function that ignores its input and returns some original seed."
  (lambda (foo)
    (declare (ignore foo))
    x))

#++
(funcall (const 1) 5)

(defmacro *> (parser &rest parsers)
  "Rightward combination of parsers."
  (let ((input (gensym "*>-INPUT")))
    `(lambda (,input)
       ,(reduce (lambda (i p)
                  (let ((name (gensym "*>-INNER")))
                    `(let ((,name ,i))
                       (etypecase ,name
                         (parser (funcall ,p (parser-input ,name)))
                         (failure ,name)))))
                parsers
                :initial-value `(funcall ,parser ,input)))))

#++
(funcall (*> #'any #'eof) "H")
#++
(funcall (*> #'any #'any #'eof) "He")

(defmacro <* (parser &rest parsers)
  "Leftward combination of parsers."
  (let ((input (gensym "*>-INPUT")))
    `(lambda (,input)
       ,(reduce (lambda (i p)
                  (let ((name (gensym "*>-INNER")))
                    `(let ((,name ,i))
                       (etypecase ,name
                         (parser (fmap (const (parser-value ,name))
                                       (funcall ,p (parser-input ,name))))
                         (failure ,name)))))
                parsers
                :initial-value `(funcall ,parser ,input)))))

#++
(funcall (<* #'any #'eof) "H")  ; Should get 'H'.
#++
(funcall (<* #'any #'any #'eof) "Ho")  ; Should get 'H'.
#++
(funcall (*> #'any (<* #'any #'eof)) "Ho")  ; Should get 'o'.
