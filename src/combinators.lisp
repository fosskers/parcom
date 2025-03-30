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

;; TODO: 2025-03-31 sep, many0, many1

(defun delimited (a parser b)
  "A main parser flanked by two other ones. Only the value of the main parser is
kept. Good for parsing backets, parentheses, etc."
  (*> a (<* parser b)))

#+nil
(funcall (delimited (char #\!) (string "Salvē") (char #\!)) "!Salvē!")
