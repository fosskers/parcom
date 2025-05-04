;;; Combinations of other parsers.

(in-package :parcom)

(defun opt (parser)
  "Yield nil if the parser failed, but don't fail the whole process nor consume any
input."
  (alt parser (lambda (input) (ok input nil)))) ; Clever.

#+nil
(funcall (opt (string "Ex")) (in "Exercitus"))
#+nil
(funcall (opt (string "Ex")) (in "Facēre"))

(defun between (a parser b)
  "A main parser flanked by two other ones. Only the value of the main parser is
kept. Good for parsing backets, parentheses, etc."
  (*> a (<* parser b)))

#+nil
(funcall (between (char #\!) (string "Salvē") (char #\!)) (in "!Salvē!"))

(defun many (parser)
  "Parse 0 or more occurrences of a `parser'."
  (lambda (input)
    (declare (optimize (speed 3)))
    (multiple-value-bind (res next) (funcall parser input)
      (if (failure? res)
          (ok input '())
          (let* ((inp next)
                 (res res)
                 (final (loop :while (ok? res)
                              :collect res
                              :do (multiple-value-bind (r i) (funcall parser inp)
                                    (setf res r)
                                    (when i (setf inp i))))))
            (ok inp final))))))

#+nil
(funcall (many (string "ovēs")) (in "ovis"))
#+nil
(funcall (many (string "ovēs")) (in "ovēsovēsovēs!"))
#+nil
(funcall (many (alt (string "ovēs") (string "avis"))) (in "ovēsovēsavis!"))

(defun many1 (parser)
  "Parse 1 or more occurrences of a `parser'."
  (lambda (input)
    (multiple-value-bind (res next) (funcall (many parser) input)
      (cond ((failure? res) res)
            ((null res) (fail "many1: at least one success" input))
            (t (values res next))))))

#+nil
(funcall (many1 (string "ovēs")) (in "ovis"))
#+nil
(funcall (many1 (string "ovēs")) (in "ovēsovēsovēs!"))

(defun sep (sep parser)
  "Parse 0 or more instances of a `parser' separated by some `sep' parser."
  (lambda (input)
    (labels ((recurse (acc in)
               (multiple-value-bind (sep-res sep-next) (funcall sep in)
                 (if (failure? sep-res)
                     (ok in acc)
                     (multiple-value-bind (res next) (funcall parser sep-next)
                       (if (failure? res)
                           res
                           (recurse (cons res acc) next)))))))
      (multiple-value-bind (res next) (funcall parser input)
        (if (failure? res)
            (ok input '())
            (fmap #'nreverse (recurse (list res) next)))))))

#+nil
(funcall (sep (char #\!) (string "pilum")) (in "."))
#+nil
(funcall (sep (char #\!) (string "pilum")) (in "pilum."))
#+nil
(funcall (sep (char #\!) (string "pilum")) (in "pilum!pilum!pilum."))
#+nil
(funcall (sep (char #\!) (string "pilum")) (in "pilum!pilum!pilum!"))

(defun sep1 (sep parser)
  "Parse 1 or more instances of a `parser' separated by some `sep' parser."
  (lambda (input)
    (multiple-value-bind (res next) (funcall (sep sep parser) input)
      (cond ((failure? res) res)
            ((null res) (fail "sep1: at least one success" input))
            (t (values res next))))))

#+nil
(funcall (sep1 (char #\!) (string "pilum")) (in "."))
#+nil
(funcall (sep1 (char #\!) (string "pilum")) (in "pilum."))
#+nil
(funcall (sep1 (char #\!) (string "pilum")) (in "pilum!pilum!pilum."))
#+nil
(funcall (sep1 (char #\!) (string "pilum")) (in "pilum!pilum!pilum!"))

(defun sep-end (sep parser)
  "Parse 0 or more instances of a `parser' separated by some `sep' parser. Parses
the separator eagerly, such that a final instance of it will also be parsed,
even if not followed by an instance of the main parser."
  (lambda (input)
    (labels ((recurse (acc in)
               (multiple-value-bind (res next) (funcall parser in)
                 (if (failure? res)
                     (ok in acc)
                     (multiple-value-bind (sep-res sep-next) (funcall sep next)
                       (if (failure? sep-res)
                           (ok next (cons res acc))
                           (recurse (cons res acc) sep-next)))))))
      (fmap #'nreverse (recurse '() input)))))

#+nil
(funcall (sep-end (char #\!) (string "pilum")) (in "."))
#+nil
(funcall (sep-end (char #\!) (string "pilum")) (in "pilum."))
#+nil
(funcall (sep-end (char #\!) (string "pilum")) (in "pilum!pilum!pilum."))
#+nil
(funcall (sep-end (char #\!) (string "pilum")) (in "pilum!pilum!pilum!"))

(defun sep-end1 (sep parser)
  "Parse 1 or more instances of a `parser' separated by some `sep' parser. Parses
the separator eagerly, such that a final instance of it will also be parsed,
even if not followed by an instance of the main parser."
  (lambda (input)
    (multiple-value-bind (res next) (funcall (sep-end sep parser) input)
      (cond ((failure? res) res)
            ((null res) (fail "sep-end1: at least one success" input))
            (t (values res next))))))

#+nil
(funcall (sep-end1 (char #\!) (string "pilum")) (in "."))
#+nil
(funcall (sep-end1 (char #\!) (string "pilum")) (in "pilum."))
#+nil
(funcall (sep-end1 (char #\!) (string "pilum")) (in "pilum!pilum!pilum."))
#+nil
(funcall (sep-end1 (char #\!) (string "pilum")) (in "pilum!pilum!pilum!"))

(defun skip (parser)
  "Parse some `parser' 0 or more times, but throw away all the results."
  (lambda (input)
    (labels ((recurse (in)
               (multiple-value-bind (res next) (funcall parser in)
                 (if (failure? res)
                     (ok in t)
                     (recurse next)))))
      (recurse input))))

#+nil
(funcall (skip (char #\!)) (in ""))
#+nil
(funcall (skip (char #\!)) (in "a"))
#+nil
(funcall (skip (char #\!)) (in "!!!hi"))

(declaim (ftype (function (maybe-parse) (function (input) (values cl:string cons))) take-until))
(defun take-until (parser)
  "Combinator: Take characters until another parser succeeds. Does not consume the
input of the subparser."
  (lambda (input)
    (let* ((s (input-str input))
           (len (length s))
           (working (cons (input-curr input) s))
           (keep (loop :for i :from (input-curr input) :below len
                       :while (when (failure? (funcall parser working))
                                (incf (input-curr working)))
                       :finally (return (- i (input-curr input))))))
      (ok (off keep input)
          (make-array keep
                      :element-type 'character
                      :displaced-to s
                      :displaced-index-offset (input-curr input))))))

#+nil
(funcall (*> (string "!!!") (take-until (char #\'))) (in "!!!abcd'"))

(declaim (ftype (function (maybe-parse) maybe-parse) peek))
(defun peek (parser)
  "Yield the value of a parser, but don't consume the input."
  (lambda (input)
    (multiple-value-bind (res next) (funcall parser input)
      (declare (ignore next))
      (if (failure? res)
          res
          (ok input res)))))

#+nil
(funcall (peek (string "he")) (in "hello"))

(declaim (ftype (function (character) (function (input) (values (or character (member :fail)) &optional cons))) sneak))
(defun sneak (c)
  "Combinator: Like `peek' but specialized for characters and thus more performant."
  (or (gethash c +sneak-cache+)
      (let ((f (lambda (input)
                 (multiple-value-bind (res next) (funcall (char c) input)
                   (declare (ignore next))
                   (if (failure? res)
                       :fail
                       (ok input res))))))
        (setf (gethash c +sneak-cache+) f)
        f)))

#+nil
(funcall (sneak #\a) (in "aaabcd"))

(declaim (ftype (function (fixnum maybe-parse) maybe-parse) count))
(defun count (n parser)
  "Apply a `parser' a given number of times."
  (lambda (input)
    (labels ((recurse (acc m i)
               (if (<= m 0)
                   (ok i (nreverse acc))
                   (multiple-value-bind (res next) (funcall parser i)
                     (if (failure? res)
                         res
                         (recurse (cons res acc) (1- m) next))))))
      (recurse '() n input))))

#+nil
(funcall (count 3 (char #\a)) (in "aaaaaa"))
#+nil
(funcall (count 3 (char #\a)) (in "aa"))
#+nil
(funcall (count 0 (char #\a)) (in "aa"))

(declaim (ftype (function (maybe-parse) (function (input) (values (or cl:string (member :fail)) &optional cons))) recognize))
(defun recognize (parser)
  "If the given `parser' was successful, return the consumed input instead."
  (lambda (input)
    (multiple-value-bind (res next) (funcall parser input)
      (if (failure? res)
          res
          (ok next
              (make-array (- (input-curr next)
                             (input-curr input))
                          :element-type 'character
                          :displaced-to (input-str input)
                          :displaced-index-offset (input-curr input)))))))

#+nil
(funcall (recognize (<*> (string "hi") (string "bye"))) (in "hibyethere"))
#+nil
(funcall (recognize (<*> (string "hi") (string "bye"))) (in "hihi"))

(defun pair (p0 p1)
  "Combinator: Parse two parsers and yield the results as a cons cell."
  (lambda (input)
    (fmap (lambda (list) (cons (car list) (cadr list)))
          (funcall (<*> p0 p1) input))))

#+nil
(funcall (pair #'any #'any) (in "hi"))
