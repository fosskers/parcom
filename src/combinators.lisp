;;; Combinations of other parsers.

(in-package :parcom)

(declaim (ftype (function (maybe-parse) always-parse) opt))
(defun opt (parser)
  "Yield nil if the parser failed, but don't fail the whole process nor consume any
input."
  (alt parser (lambda (input) (ok input nil)))) ; Clever.

#+nil
(funcall (opt (string "Ex")) "Exercitus")
#+nil
(funcall (opt (string "Ex")) "Facēre")

(defun between (a parser b)
  "A main parser flanked by two other ones. Only the value of the main parser is
kept. Good for parsing backets, parentheses, etc."
  (*> a (<* parser b)))

#+nil
(funcall (between (char #\!) (string "Salvē") (char #\!)) "!Salvē!")

(declaim (ftype (function (maybe-parse) always-parse) many))
(defun many (parser)
  "Parse 0 or more occurrences of a `parser'."
  (lambda (input)
    (declare (optimize (speed 3)))
    (labels ((recurse (acc in)
               (let ((res (funcall parser in)))
                 (etypecase res
                   (failure (ok in acc))
                   (parser (recurse (cons (parser-value res) acc)
                                    (parser-input res)))))))
      (fmap #'nreverse (recurse '() input)))))

#+nil
(funcall (many (string "ovēs")) "ovis")
#+nil
(funcall (many (string "ovēs")) "ovēsovēsovēs!")
#+nil
(funcall (many (alt (string "ovēs") (string "avis"))) "ovēsovēsavis!")

(declaim (ftype (function (maybe-parse) maybe-parse) many1))
(defun many1 (parser)
  "Parse 1 or more occurrences of a `parser'."
  (lambda (input)
    (let ((res (funcall (many parser) input)))
      (cond ((failure-p res) res)
            ((null (parser-value res)) (fail "many1: at least one success" input))
            (t res)))))

#+nil
(funcall (many1 (string "ovēs")) "ovis")
#+nil
(funcall (many1 (string "ovēs")) "ovēsovēsovēs!")

(declaim (ftype (function (maybe-parse maybe-parse) always-parse) sep))
(defun sep (sep parser)
  "Parse 0 or more instances of a `parser' separated by some `sep' parser."
  (lambda (input)
    (labels ((recurse (acc in)
               (let ((sep-res (funcall sep in)))
                 (etypecase sep-res
                   (failure (ok in acc))
                   (parser  (let ((res (funcall parser (parser-input sep-res))))
                              (etypecase res
                                (failure res)
                                (parser  (recurse (cons (parser-value res) acc)
                                                  (parser-input res))))))))))
      (let ((res (funcall parser input)))
        (etypecase res
          (failure (ok input '()))
          (parser  (fmap #'nreverse (recurse (list (parser-value res))
                                             (parser-input res)))))))))

#+nil
(funcall (sep (char #\!) (string "pilum")) ".")
#+nil
(funcall (sep (char #\!) (string "pilum")) "pilum.")
#+nil
(funcall (sep (char #\!) (string "pilum")) "pilum!pilum!pilum.")
#+nil
(funcall (sep (char #\!) (string "pilum")) "pilum!pilum!pilum!")

(declaim (ftype (function (maybe-parse maybe-parse) maybe-parse) sep1))
(defun sep1 (sep parser)
  "Parse 1 or more instances of a `parser' separated by some `sep' parser."
  (lambda (input)
    (let ((res (funcall (sep sep parser) input)))
      (cond ((failure-p res) res)
            ((null (parser-value res)) (fail "sep1: at least one success" input))
            (t res)))))

#+nil
(funcall (sep1 (char #\!) (string "pilum")) ".")
#+nil
(funcall (sep1 (char #\!) (string "pilum")) "pilum.")
#+nil
(funcall (sep1 (char #\!) (string "pilum")) "pilum!pilum!pilum.")
#+nil
(funcall (sep1 (char #\!) (string "pilum")) "pilum!pilum!pilum!")

(declaim (ftype (function (maybe-parse maybe-parse) always-parse) sep-end))
(defun sep-end (sep parser)
  "Parse 0 or more instances of a `parser' separated by some `sep' parser. Parses
the separator eagerly, such that a final instance of it will also be parsed,
even if not followed by an instance of the main parser."
  (lambda (input)
    (labels ((recurse (acc in)
               (let ((res (funcall parser in)))
                 (etypecase res
                   (failure (ok in acc))
                   (parser (let ((sep-res (funcall sep (parser-input res))))
                             (etypecase sep-res
                               (failure (ok (parser-input res)
                                            (cons (parser-value res) acc)))
                               (parser (recurse (cons (parser-value res) acc)
                                                (parser-input sep-res))))))))))
      (fmap #'nreverse (recurse '() input)))))

#+nil
(funcall (sep-end (char #\!) (string "pilum")) ".")
#+nil
(funcall (sep-end (char #\!) (string "pilum")) "pilum.")
#+nil
(funcall (sep-end (char #\!) (string "pilum")) "pilum!pilum!pilum.")
#+nil
(funcall (sep-end (char #\!) (string "pilum")) "pilum!pilum!pilum!")

(declaim (ftype (function (maybe-parse maybe-parse) maybe-parse) sep-end1))
(defun sep-end1 (sep parser)
  "Parse 1 or more instances of a `parser' separated by some `sep' parser. Parses
the separator eagerly, such that a final instance of it will also be parsed,
even if not followed by an instance of the main parser."
  (lambda (input)
    (let ((res (funcall (sep-end sep parser) input)))
      (cond ((failure-p res) res)
            ((null (parser-value res)) (fail "sep-end1: at least one success" input))
            (t res)))))

#+nil
(funcall (sep-end1 (char #\!) (string "pilum")) ".")
#+nil
(funcall (sep-end1 (char #\!) (string "pilum")) "pilum.")
#+nil
(funcall (sep-end1 (char #\!) (string "pilum")) "pilum!pilum!pilum.")
#+nil
(funcall (sep-end1 (char #\!) (string "pilum")) "pilum!pilum!pilum!")

(declaim (ftype (function (maybe-parse) always-parse) skip))
(defun skip (parser)
  "Parse some `parser' 0 or more times, but throw away all the results."
  (lambda (input)
    (labels ((recurse (in)
               (let ((res (funcall parser in)))
                 (etypecase res
                   (failure (ok in t))
                   (parser  (recurse (parser-input res)))))))
      (recurse input))))

#+nil
(funcall (skip (char #\!)) "")
#+nil
(funcall (skip (char #\!)) "a")
#+nil
(funcall (skip (char #\!)) "!!!hi")

(declaim (ftype (function (maybe-parse) maybe-parse) peek))
(defun peek (parser)
  "Yield the value of a parser, but don't consume the input."
  (lambda (input)
    (let ((res (funcall parser input)))
      (etypecase res
        (failure res)
        (parser  (ok input (parser-value res)))))))

#+nil
(funcall (peek (string "he")) "hello")

(declaim (ftype (function (fixnum maybe-parse) maybe-parse) count))
(defun count (n parser)
  "Apply a `parser' a given number of times."
  (lambda (input)
    (labels ((recurse (acc m i)
               (if (<= m 0)
                   (ok i (nreverse acc))
                   (let ((res (funcall parser i)))
                     (etypecase res
                       (failure res)
                       (parser  (recurse (cons (parser-value res) acc)
                                         (1- m)
                                         (parser-input res))))))))
      (recurse '() n input))))

#+nil
(funcall (count 3 (char #\a)) "aaaaaa")
#+nil
(funcall (count 3 (char #\a)) "aa")
#+nil
(funcall (count 0 (char #\a)) "aa")

(declaim (ftype (function (maybe-parse) maybe-parse) recognize))
(defun recognize (parser)
  "If the given `parser' was successful, return the consumed input instead."
  (lambda (input)
    (let ((res (funcall parser input)))
      (etypecase res
        (failure res)
        (parser  (ok (parser-input res)
                     (make-array (- (length input) (length (parser-input res)))
                                 :element-type 'character
                                 :displaced-to input)))))))

#+nil
(funcall (recognize (<*> (string "hi") (string "bye"))) "hibyethere")
#+nil
(funcall (recognize (<*> (string "hi") (string "bye"))) "hihi")

(defun pair (p0 p1)
  "Combinator: Parse two parsers and yield the results as a cons cell."
  (lambda (input)
    (fmap (lambda (list) (cons (car list) (cadr list)))
          (funcall (<*> p0 p1) input))))

#+nil
(funcall (pair #'any #'any) "hi")
