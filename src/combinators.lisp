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

;; TODO: 2025-03-31 sep0, sep1

(defun delimited (a parser b)
  "A main parser flanked by two other ones. Only the value of the main parser is
kept. Good for parsing backets, parentheses, etc."
  (*> a (<* parser b)))

#+nil
(funcall (delimited (char #\!) (string "Salvē") (char #\!)) "!Salvē!")

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
