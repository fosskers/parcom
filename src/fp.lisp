;;; Functional Programming utilities.

(in-package :parcom)

#++
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
  (ok (input-str (parser-input p))
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
  "Combination of parsers yielding the result of the rightmost one."
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
(funcall (*> #'any) (in "H"))
#++
(funcall (*> #'any #'eof) (in "H"))
#++
(funcall (*> #'any #'any #'eof) (in "He"))

(defmacro right (parser &rest parsers)
  "Combination of parsers yielding the result of the rightmost one."
  `(*> ,parser ,@parsers))

(defmacro <* (parser &rest parsers)
  "Combination of parsers yielding the result of the leftmost one."
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
(funcall (<* #'any) "H")
#++
(funcall (<* #'any #'eof) "H")  ; Should get 'H'.
#++
(funcall (<* #'any #'any #'eof) "Ho")  ; Should get 'H'.
#++
(funcall (*> #'any (<* #'any #'eof)) "Ho")  ; Should get 'o'.

(defmacro left (parser &rest parsers)
  "Combination of parsers yielding the result of the leftmost one."
  `(<* ,parser ,@parsers))

(defmacro <*> (parser &rest parsers)
  "Combination of parsers yielding all results as a list."
  (let ((input (gensym "<*>-INPUT")))
    `(lambda (,input)
       ,(labels ((recurse (ps i)
                   (if (null ps)
                       `(ok (input-str ,i) nil)
                       (let ((name (gensym "<*>-INNER")))
                         `(let ((,name (funcall ,(car ps) ,i)))
                            (etypecase ,name
                              (failure ,name)
                              (parser  (let ((res ,(recurse (cdr ps) `(parser-input ,name))))
                                         (fmap (lambda (xs) (cons (parser-value ,name) xs)) res)))))))))
          (recurse (cons parser parsers) input)))))

#+nil
(funcall (<*> (string "hi")) (in "hihohum!"))
#+nil
(funcall (<*> (string "hi") (string "ho") (string "hum")) (in "hihohum!"))
#+nil
(funcall (<*> (string "hi") (string "har") (string "hum")) (in "hihohum!"))

(defmacro all (parser &rest parsers)
  "Combination of parsers yielding all results as a list."
  `(<*> ,parser ,@parsers))

#+nil
(all (string "hi") (string "ho") (string "hum"))

(defun <$ (item parser)
  "Run some parser, but substitute its inner value with some `item' if parsing was
successful."
  (lambda (input) (fmap (const item) (funcall parser input))))

#++
(funcall (<$ 1 #'any) "Ho")

(defmacro instead (item parser)
  "Run some parser, but substitute its inner value with some `item' if parsing was
successful."
  `(<$ ,item ,parser))

(defmacro alt (parser &rest parsers)
  "Accept the results of the first parser from a group to succeed."
  (let ((input (gensym "ALT-INPUT")))
    `(lambda (,input)
       ,(labels ((recurse (ps)
                   (if (null ps)
                       `(fail "alt: something to succeed" ,input)
                       `(let ((res (funcall ,(car ps) ,input)))
                          (etypecase res
                            (parser res)
                            (failure ,(recurse (cdr ps))))))))
          (recurse (cons parser parsers))))))

#++
(funcall (alt (char #\H) (char #\h)) "Hello")
#++
(funcall (alt (char #\H) (char #\h)) "hello")
#++
(funcall (alt (char #\H) (char #\h)) "ello")
