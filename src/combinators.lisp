;;; Combinations of other parsers.

(in-package :parcom)

(fn not (-> (maybe t) (maybe t)))
(defun not (parser)
  "Pass if the given parser fails, and don't advance the offset. Fail if it
succeeds."
  (lambda (offset)
    (multiple-value-bind (res next) (funcall parser offset)
      (declare (ignore next))
      (if (ok? res)
          (fail offset)
          (values t offset)))))

#+nil
(funcall (not (char #\a)) (in "bark"))
#+nil
(parse (not (char #\a)) "ark")

(fn opt (-> (maybe t) (maybe t)))
(defun opt (parser)
  "Yield nil if the parser failed, but don't fail the whole process nor consume any
input."
  (lambda (offset)
    (multiple-value-bind (res next) (funcall parser offset)
      (cond ((failure? res) (values nil offset))
            (t (values res next))))))

#+nil
(funcall (opt (string "Ex")) (in "Exercitus"))
#+nil
(funcall (opt (string "Ex")) (in "Facēre"))

(defun between (a parser b &key (id nil))
  "A main parser flanked by two other ones. Only the value of the main parser is
kept. Good for parsing backets, parentheses, etc."
  (declare (ignore id))
  (*> a (<* parser b)))

#+nil
(funcall (between (char #\!) (string "Salvē") (char #\!)) (in "!Salvē!"))

(fn many (-> (maybe t) (always list)))
(defun many (parser)
  "Parse 0 or more occurrences of a `parser'."
  (lambda (offset)
    (declare (optimize (speed 3)))
    (multiple-value-bind (res next) (funcall parser offset)
      (if (failure? res)
          (ok offset '())
          (let* ((inp next)
                 (res res)
                 (final (loop :while (ok? res)
                              :collect res
                              :do (multiple-value-bind (r i) (funcall parser inp)
                                    (setf res r)
                                    (when (ok? r) (setf inp i))))))
            (ok inp final))))))

#+nil
(funcall (many (string "ovēs")) (in "ovis"))
#+nil
(funcall (many (string "ovēs")) (in "ovēsovēsovēs!"))
#+nil
(funcall (many (alt (string "ovēs") (string "avis"))) (in "ovēsovēsavis!"))

(fn many1 (-> (maybe t) (maybe list)))
(defun many1 (parser)
  "Parse 1 or more occurrences of a `parser'."
  (lambda (offset)
    (declare (optimize (speed 3)))
    (multiple-value-bind (res next) (funcall parser offset)
      (if (failure? res)
          (fail offset)
          (let* ((inp next)
                 (res res)
                 (final (loop :while (ok? res)
                              :collect res
                              :do (multiple-value-bind (r i) (funcall parser inp)
                                    (setf res r)
                                    (when (ok? r) (setf inp i))))))
            (ok inp final))))))

#+nil
(funcall (many1 (string "ovēs")) (in "ovis"))
#+nil
(funcall (many1 (string "ovēs")) (in "ovēsovēsovēs!"))

(defun sep (sep parser &key (id nil))
  "Parse 0 or more instances of a `parser' separated by some `sep' parser."
  (declare (ignore id))
  (lambda (offset)
    (labels ((recurse (acc in)
               (multiple-value-bind (sep-res sep-next) (funcall sep in)
                 (if (failure? sep-res)
                     (ok in acc)
                     (multiple-value-bind (res next) (funcall parser sep-next)
                       (if (failure? res)
                           (fail sep-next)
                           (recurse (cons res acc) next)))))))
      (multiple-value-bind (res next) (funcall parser offset)
        (if (failure? res)
            (ok offset '())
            (fmap #'nreverse (recurse (list res) next)))))))

#+nil
(funcall (sep (char #\!) (string "pilum")) (in "."))
#+nil
(funcall (sep (char #\!) (string "pilum")) (in "pilum."))
#+nil
(funcall (sep (char #\!) (string "pilum")) (in "pilum!pilum!pilum."))
#+nil
(funcall (sep (char #\!) (string "pilum")) (in "pilum!pilum!pilum!"))

(fn sep1 (-> (maybe t) (maybe t) (maybe list)))
(defun sep1 (sep parser)
  "Parse 1 or more instances of a `parser' separated by some `sep' parser."
  (lambda (offset)
    (labels ((recurse (acc in)
               (multiple-value-bind (sep-res sep-next) (funcall sep in)
                 (if (failure? sep-res)
                     (ok in acc)
                     (multiple-value-bind (res next) (funcall parser sep-next)
                       (if (failure? res)
                           (fail sep-next)
                           (recurse (cons res acc) next)))))))
      (multiple-value-bind (res next) (funcall parser offset)
        (if (failure? res)
            (fail offset)
            (fmap #'nreverse (recurse (list res) next)))))))

#+nil
(funcall (sep1 (char #\!) (string "pilum")) (in "."))
#+nil
(funcall (sep1 (char #\!) (string "pilum")) (in "pilum."))
#+nil
(funcall (sep1 (char #\!) (string "pilum")) (in "pilum!pilum!pilum."))
#+nil
(funcall (sep1 (char #\!) (string "pilum")) (in "pilum!pilum!pilum!"))

(fn sep-end (-> (maybe t) (maybe t) (maybe list)))
(defun sep-end (sep parser)
  "Parse 0 or more instances of a `parser' separated by some `sep' parser. Parses
the separator eagerly, such that a final instance of it will also be parsed,
even if not followed by an instance of the main parser."
  (lambda (offset)
    (labels ((recurse (acc in)
               (multiple-value-bind (res next) (funcall parser in)
                 (if (failure? res)
                     (ok in acc)
                     (multiple-value-bind (sep-res sep-next) (funcall sep next)
                       (if (failure? sep-res)
                           (ok next (cons res acc))
                           (recurse (cons res acc) sep-next)))))))
      (fmap #'nreverse (recurse '() offset)))))

#+nil
(funcall (sep-end (char #\!) (string "pilum")) (in "."))
#+nil
(funcall (sep-end (char #\!) (string "pilum")) (in "pilum."))
#+nil
(funcall (sep-end (char #\!) (string "pilum")) (in "pilum!pilum!pilum."))
#+nil
(funcall (sep-end (char #\!) (string "pilum")) (in "pilum!pilum!pilum!"))

(fn sep-end1 (-> (maybe t) (maybe t) (maybe list)))
(defun sep-end1 (sep parser)
  "Parse 1 or more instances of a `parser' separated by some `sep' parser. Parses
the separator eagerly, such that a final instance of it will also be parsed,
even if not followed by an instance of the main parser."
  (lambda (offset)
    (multiple-value-bind (res next) (funcall (sep-end sep parser) offset)
      (if (null res)
          (fail offset)
          (values res next)))))

#+nil
(funcall (sep-end1 (char #\!) (string "pilum")) (in "."))
#+nil
(funcall (sep-end1 (char #\!) (string "pilum")) (in "pilum."))
#+nil
(funcall (sep-end1 (char #\!) (string "pilum")) (in "pilum!pilum!pilum."))
#+nil
(funcall (sep-end1 (char #\!) (string "pilum")) (in "pilum!pilum!pilum!"))

(fn consume-sep (-> (maybe t) (maybe t) (maybe fixnum)))
(defun consume-sep (sep parser)
  "Like `sep', but similar to `consume' it ignores all success of the parsers in a
memory-efficient way and simply advances the parsing offset. The main parser
need not succeed even once."
  (lambda (offset)
    (labels ((recurse (in)
               (multiple-value-bind (sep-res sep-next) (funcall sep in)
                 (if (failure? sep-res)
                     (ok in in)
                     (multiple-value-bind (res next) (funcall parser sep-next)
                       (if (failure? res)
                           (fail sep-next)
                           (recurse next)))))))
      (multiple-value-bind (res next) (funcall parser offset)
        (if (failure? res)
            (ok offset offset)
            (recurse next))))))

#+nil
(parse (consume-sep (char #\!) (string "pilum")) "pilum!pilum!pilum.")
#+nil
(parse (consume-sep (char #\!) (string "pilum")) "")

(fn consume-sep1 (-> (maybe t) (maybe t) (maybe fixnum)))
(defun consume-sep1 (sep parser)
  "Like `consume-sep' but expects the main parser to succeed at least once."
  (lambda (offset)
    (labels ((recurse (in)
               (multiple-value-bind (sep-res sep-next) (funcall sep in)
                 (if (failure? sep-res)
                     (ok in in)
                     (multiple-value-bind (res next) (funcall parser sep-next)
                       (if (failure? res)
                           (fail sep-next)
                           (recurse next)))))))
      (multiple-value-bind (res next) (funcall parser offset)
        (if (failure? res)
            (fail offset)
            (recurse next))))))

#+nil
(parse (consume-sep1 (char #\!) (string "pilum")) "pilum!pilum!pilum.")
#+nil
(parse (consume-sep1 (char #\!) (string "pilum")) "")

(defun skip (parser &key (id nil))
  "Parse some `parser' 0 or more times, but throw away all the results."
  (declare (ignore id))
  (lambda (offset)
    (labels ((recurse (in)
               (multiple-value-bind (res next) (funcall parser in)
                 (if (failure? res)
                     (ok in t)
                     (recurse next)))))
      (recurse offset))))

#+nil
(funcall (skip (char #\!)) (in ""))
#+nil
(funcall (skip (char #\!)) (in "a"))
#+nil
(funcall (skip (char #\!)) (in "!!!hi"))

(fn take-until (-> (maybe t) (maybe cl:string)))
(defun take-until (parser)
  "Combinator: Take characters until another parser succeeds. Does not consume the
input of the subparser."
  (lambda (offset)
    (declare (type fixnum offset))
    (let* ((working offset)
           (keep (loop :for i fixnum :from offset :below *input-length*
                       :while (when (failure? (funcall parser working))
                                (incf working))
                       :finally (return (- i offset)))))
      (ok (off keep offset)
          (make-array keep
                      :element-type 'character
                      :displaced-to *input*
                      :displaced-index-offset offset)))))

#+nil
(funcall (*> (string "!!!") (take-until (char #\'))) (in "!!!abcd'"))

(fn peek (-> (maybe t) (maybe t)))
(defun peek (parser)
  "Yield the value of a parser, but don't consume the input."
  (lambda (offset)
    (multiple-value-bind (res next) (funcall parser offset)
      (if (failure? res)
          (fail next)
          (ok offset res)))))

#+nil
(funcall (peek (string "he")) (in "hello"))

(fn sneak (-> character (maybe character)))
(defun sneak (c)
  "Combinator: Like `peek' but specialized for characters and thus more performant."
  (lambda (offset)
    (multiple-value-bind (res next) (funcall (char c) offset)
      (if (failure? res)
          (fail next)
          (ok offset res)))))

#+nil
(funcall (sneak #\a) (in "aaabcd"))

(fn count (-> fixnum (maybe t) (maybe list)))
(defun count (n parser)
  "Apply a `parser' a given number of times."
  (lambda (offset)
    (labels ((recurse (acc m i)
               (if (<= m 0)
                   (ok i (nreverse acc))
                   (multiple-value-bind (res next) (funcall parser i)
                     (if (failure? res)
                         (fail next)
                         (recurse (cons res acc) (1- m) next))))))
      (recurse '() n offset))))

#+nil
(funcall (count 3 (char #\a)) (in "aaaaaa"))
#+nil
(funcall (count 3 (char #\a)) (in "aa"))
#+nil
(funcall (count 0 (char #\a)) (in "aa"))

(fn recognize (-> (maybe t) (maybe cl:string)))
(defun recognize (parser)
  "If the given `parser' was successful, return the consumed input instead."
  (lambda (offset)
    (multiple-value-bind (res next) (funcall parser offset)
      (if (failure? res)
          (fail next)
          (ok next
              (make-array (- next offset)
                          :element-type 'character
                          :displaced-to *input*
                          :displaced-index-offset offset))))))

#+nil
(funcall (recognize (<*> (string "hi") (string "bye"))) (in "hibyethere"))
#+nil
(funcall (recognize (<*> (string "hi") (string "bye"))) (in "hihi"))

(fn pair (-> (maybe t) (maybe t) (maybe cons)))
(defun pair (p0 p1)
  "Combinator: Parse two parsers and yield the results as a cons cell."
  (lambda (offset)
    (funcall (ap #'cons p0 p1) offset)))

#+nil
(funcall (pair #'any #'any) (in "hi"))

(fn maybe (-> function (maybe t) (maybe t) (maybe t)))
(defun maybe (f p0 p1)
  "Combinator: If an initial parser succeeds, apply some `f' to the result of the
second parser. If the first parser doesn't succeed, the second is attempted as
usual but `f' isn't applied."
  (lambda (offset)
    (multiple-value-bind (first next) (funcall p0 offset)
      (if (failure? first)
          (funcall p1 offset)
          (fmap f (funcall p1 next))))))

#+nil
(parse (maybe #'1+ (char #\a) #'integer) "a123")
#+nil
(parse (maybe #'1+ (char #\a) #'integer) "123")
