;;; Combinations of other parsers.

(in-package :parcom)

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

(defun many0 (parser)
  "Parse 0 or more occurrences of a `parser'."
  (lambda (input)
    (labels ((recurse (acc in)
               (let ((res (funcall parser in)))
                 (etypecase res
                   (failure (ok in acc))
                   (parser (recurse (cons (parser-value res) acc)
                                    (parser-input res)))))))
      (fmap #'nreverse (recurse '() input)))))

#+nil
(funcall (many0 (string "ovēs")) "ovis")
#+nil
(funcall (many0 (string "ovēs")) "ovēsovēsovēs!")
#+nil
(funcall (many0 (alt (string "ovēs") (string "avis"))) "ovēsovēsavis!")

(defun many1 (parser)
  "Parse 1 or more occurrences of a `parser'."
  (lambda (input)
    (let ((res (funcall (many0 parser) input)))
      (cond ((failure-p res) res)
            ((null (parser-value res)) (fail "many1: at least one success" input))
            (t res)))))

#+nil
(funcall (many1 (string "ovēs")) "ovis")
#+nil
(funcall (many1 (string "ovēs")) "ovēsovēsovēs!")

(defun sep0 (sep parser)
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
(funcall (sep0 (char #\!) (string "pilum")) ".")
#+nil
(funcall (sep0 (char #\!) (string "pilum")) "pilum.")
#+nil
(funcall (sep0 (char #\!) (string "pilum")) "pilum!pilum!pilum.")
#+nil
(funcall (sep0 (char #\!) (string "pilum")) "pilum!pilum!pilum!")

(defun sep1 (sep parser)
  "Parse 1 or more instances of a `parser' separated by some `sep' parser."
  (lambda (input)
    (let ((res (funcall (sep0 sep parser) input)))
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

(defun sep-end0 (sep parser)
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
(funcall (sep-end0 (char #\!) (string "pilum")) ".")
#+nil
(funcall (sep-end0 (char #\!) (string "pilum")) "pilum.")
#+nil
(funcall (sep-end0 (char #\!) (string "pilum")) "pilum!pilum!pilum.")
#+nil
(funcall (sep-end0 (char #\!) (string "pilum")) "pilum!pilum!pilum!")

(defun sep-end1 (sep parser)
  "Parse 1 or more instances of a `parser' separated by some `sep' parser. Parses
the separator eagerly, such that a final instance of it will also be parsed,
even if not followed by an instance of the main parser."
  (lambda (input)
    (let ((res (funcall (sep-end0 sep parser) input)))
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

(defun peek (parser)
  "Yield the value of a parser, but don't consume the input."
  (lambda (input)
    (let ((res (funcall parser input)))
      (etypecase res
        (failure res)
        (parser  (ok input (parser-value res)))))))

#+nil
(funcall (peek (string "he")) "hello")
