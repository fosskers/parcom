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

(defmacro fmap (f thing)
  "Apply a pure function to the inner contents of some parsing operation, if the
parsing itself was successful."
  `(multiple-value-bind (res next) ,thing
     (if (failure? res)
         (fail next)
         (values (funcall ,f res) next))))

#+nil
(fmap #'char-upcase (any (in "hello")))
#++
(fmap #'1+ (ok (in "") 1))

(defmacro pmap (f parser)
  "Similar to `fmap', but this transforms a parser into another one, altering
its inner result if it happened to be successful."
  `(lambda (offset)
     (fmap ,f (funcall ,parser offset))))

#+nil
(parse (pmap #'1+ #'unsigned) "123")
#+nil
(parse (<*> (string "hi") (pmap #'1+ #'unsigned)) "hi123")

(defun const (x)
  "Yield a function that ignores its input and returns some original seed."
  (lambda (foo)
    (declare (ignore foo))
    x))

#++
(funcall (const 1) 5)

(defmacro *> (parser &rest parsers)
  "Combination of parsers yielding the result of the rightmost one."
  (let ((offset (gensym "*>-OFFSET")))
    `(lambda (,offset)
       ,(reduce (lambda (i p)
                  (let ((res  (gensym "*>-RES"))
                        (next (gensym "*>-NEXT")))
                    `(multiple-value-bind (,res ,next) ,i
                       (if (ok? ,res)
                           (funcall ,p ,next)
                           (fail ,next)))))
                parsers
                :initial-value `(funcall ,parser ,offset)))))

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
  (let ((offset (gensym "<*-OFFSET")))
    `(lambda (,offset)
       ,(reduce (lambda (i p)
                  (let ((res0 (gensym "<*-RES"))
                        (next (gensym "<*-NEXT")))
                    `(multiple-value-bind (,res0 ,next) ,i
                       (if (ok? ,res0)
                           (multiple-value-bind (res next) (funcall ,p ,next)
                             (if (ok? res)
                                 (values ,res0 next)
                                 (fail next)))
                           (fail ,next)))))
                parsers
                :initial-value `(funcall ,parser ,offset)))))

#++
(funcall (<* #'any) (in "H"))
#++
(funcall (<* #'any #'eof) (in "H"))  ; Should get 'H'.
#++
(funcall (<* #'any #'any #'eof) (in "Ho"))  ; Should get 'H'.
#++
(funcall (*> #'any (<* #'any #'eof)) (in "Ho"))  ; Should get 'o'.

(defmacro left (parser &rest parsers)
  "Combination of parsers yielding the result of the leftmost one."
  `(<* ,parser ,@parsers))

(defmacro ap (f parser &rest parsers)
  "Run many parsers that must all succeed, and apply their success values to a
given lambda. More memory-efficient than the combination of `<*>' and `fmap'."
  (let ((offset (gensym "ap-OFFSET")))
    `(lambda (,offset)
       ,(labels ((recurse (ps rs i)
                   (if (null ps)
                       (let ((ordered (nreverse rs)))
                         `(values (funcall ,f ,@ordered) ,i))
                       (let ((res  (gensym "ap-RES"))
                             (next (gensym "ap-NEXT")))
                         `(multiple-value-bind (,res ,next) (funcall ,(car ps) ,i)
                            (if (failure? ,res)
                                (fail ,next)
                                ,(recurse (cdr ps) (cons res rs) next)))))))
          (recurse (cons parser parsers) '() offset)))))

#+nil
(parse (ap (lambda (a b c) (list a b c))
           #'unsigned
           (*> (char #\.) #'unsigned)
           (*> (char #\.) #'unsigned))
       "1.2.3")

(defmacro <*> (parser &rest parsers)
  "Combination of parsers yielding all results as a list."
  (let ((offset (gensym "<*>-OFFSET")))
    `(lambda (,offset)
       ,(labels ((recurse (ps i)
                   (if (null ps)
                       `(ok ,i nil)
                       (let ((res  (gensym "<*>-RES"))
                             (next (gensym "<*>-NEXT")))
                         `(multiple-value-bind (,res ,next) (funcall ,(car ps) ,i)
                            (if (failure? ,res)
                                (fail ,next)
                                (fmap (lambda (xs) (cons ,res xs))
                                      ,(recurse (cdr ps) `,next))))))))
          (recurse (cons parser parsers) offset)))))

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

(defmacro <$ (item parser)
  "Run some parser, but substitute its inner value with some `item' if parsing was
  successful."
  `(lambda (offset) (fmap (const ,item) (funcall ,parser offset))))

#++
(funcall (<$ 1 #'any) (in "Ho"))

(defmacro instead (item parser)
  "Run some parser, but substitute its inner value with some `item' if parsing was
  successful."
  `(<$ ,item ,parser))

(defmacro alt (parser &rest parsers)
  "Accept the results of the first parser from a group to succeed."
  (let ((offset (gensym "ALT-OFFSET")))
    `(lambda (,offset)
       ,(labels ((recurse (ps furthest)
                   (if (null ps)
                       `(fail ,furthest)
                       (let ((res  (gensym "ALT-RES"))
                             (next (gensym "ALT-NEXT")))
                         `(multiple-value-bind (,res ,next) (funcall ,(car ps) ,offset)
                            (if (ok? ,res)
                                (values ,res ,next)
                                ,(recurse (cdr ps) `(max ,next ,furthest))))))))
          (recurse (cons parser parsers) offset)))))

#++
(funcall (alt (char #\H) (char #\h)) (in "Hello"))
#++
(funcall (alt (char #\H) (char #\h)) (in "hello"))
#++
(funcall (alt (char #\H) (char #\h)) (in "ello"))
#++
(funcall (*> (char #\e) (alt (char #\M) (char #\m))) (in "ello"))
