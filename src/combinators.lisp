;;; Combinations of other parsers.

(in-package :parcom)

(defun opt (parser)
  "Yield nil if the parser failed, but don't fail the whole process nor consume any
input."
  (alt parser (lambda (offset) (ok offset nil)))) ; Clever.

#+nil
(funcall (opt (string "Ex")) (in "Exercitus"))
#+nil
(funcall (opt (string "Ex")) (in "Facēre"))

(defun between (a parser b &key (id nil))
  "A main parser flanked by two other ones. Only the value of the main parser is
kept. Good for parsing backets, parentheses, etc."
  (or (gethash id *between-cache*)
      (let ((f (*> a (<* parser b))))
        (when id
          (setf (gethash id *between-cache*) f))
        f)))

#+nil
(funcall (between (char #\!) (string "Salvē") (char #\!)) (in "!Salvē!"))

(declaim (ftype (function (maybe-parse) (function (fixnum) (values list fixnum))) many))
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
  (lambda (offset)
    (multiple-value-bind (res next) (funcall (many parser) offset)
      (cond ((failure? res) (fail next))
            ((null res) (fail offset))
            (t (values res next))))))

#+nil
(funcall (many1 (string "ovēs")) (in "ovis"))
#+nil
(funcall (many1 (string "ovēs")) (in "ovēsovēsovēs!"))

(defun sep (sep parser &key (id nil))
  "Parse 0 or more instances of a `parser' separated by some `sep' parser."
  (or (gethash id *sep-cache*)
      (let ((f (lambda (offset)
                 (labels ((recurse (acc in)
                            (multiple-value-bind (sep-res sep-next) (funcall sep in)
                              (if (failure? sep-res)
                                  (ok in acc)
                                  (multiple-value-bind (res next) (funcall parser sep-next)
                                    (if (failure? res)
                                        (fail next)
                                        (recurse (cons res acc) next)))))))
                   (multiple-value-bind (res next) (funcall parser offset)
                     (if (failure? res)
                         (ok offset '())
                         (fmap #'nreverse (recurse (list res) next))))))))
        (when id
          (setf (gethash id *sep-cache*) f))
        f)))

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
  (lambda (offset)
    (multiple-value-bind (res next) (funcall (sep sep parser) offset)
      (cond ((failure? res) (fail next))
            ((null res) (fail offset))
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

(defun sep-end1 (sep parser)
  "Parse 1 or more instances of a `parser' separated by some `sep' parser. Parses
the separator eagerly, such that a final instance of it will also be parsed,
even if not followed by an instance of the main parser."
  (lambda (offset)
    (multiple-value-bind (res next) (funcall (sep-end sep parser) offset)
      (cond ((failure? res) (fail next))
            ((null res) (fail offset))
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

(declaim (ftype (function (maybe-parse) (function (fixnum) (values cl:string fixnum))) take-until))
(defun take-until (parser)
  "Combinator: Take characters until another parser succeeds. Does not consume the
input of the subparser."
  (lambda (offset)
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

(declaim (ftype (function (maybe-parse) maybe-parse) peek))
(defun peek (parser)
  "Yield the value of a parser, but don't consume the input."
  (lambda (offset)
    (multiple-value-bind (res next) (funcall parser offset)
      (if (failure? res)
          (fail next)
          (ok offset res)))))

#+nil
(funcall (peek (string "he")) (in "hello"))

(declaim (ftype (function (character) (function (fixnum) (values (or character (member :fail)) fixnum))) sneak))
(defun sneak (c)
  "Combinator: Like `peek' but specialized for characters and thus more performant."
  (or (gethash c *sneak-cache*)
      (let ((f (lambda (offset)
                 (multiple-value-bind (res next) (funcall (char c) offset)
                   (if (failure? res)
                       (fail next)
                       (ok offset res))))))
        (setf (gethash c *sneak-cache*) f)
        f)))

#+nil
(funcall (sneak #\a) (in "aaabcd"))

(declaim (ftype (function (fixnum maybe-parse) maybe-parse) count))
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

(declaim (ftype (function (maybe-parse) (function (fixnum) (values (or cl:string (member :fail)) fixnum))) recognize))
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

(defun pair (p0 p1)
  "Combinator: Parse two parsers and yield the results as a cons cell."
  (lambda (offset)
    (fmap (lambda (list) (cons (car list) (cadr list)))
          (funcall (<*> p0 p1) offset))))

#+nil
(funcall (pair #'any #'any) (in "hi"))
