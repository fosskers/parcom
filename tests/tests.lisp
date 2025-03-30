(defpackage parcom/tests
  (:use :cl :parachute)
  (:local-nicknames (#:pc #:parcom)))

(in-package :parcom/tests)

(define-test parsers)

(define-test "char"
  :parent parsers
  (is equal #\H (pc:parse (pc:char #\H) "Hello"))
  (fail (pc:parse (pc:char #\H) "ello")))

(define-test "string"
  :parent parsers
  (is equal "" (pc:parse (pc:string "") "a"))
  (is equal "Hello" (pc:parse (pc:string "Hello") "Hello yes"))
  (fail (pc:parse (pc:string "HellO") "Hello yes")))

(define-test "take"
  :parent parsers
  (is equal "" (pc:parse (pc:take 0) "Arbor"))
  (is equal "Arb" (pc:parse (pc:take 3) "Arbor"))
  (fail (pc:parse (pc:take -5) "Arbor")))

(define-test combinators)

(define-test "opt"
  :parent combinators
  (is equal "Ex" (pc:parse (pc:opt (pc:string "Ex")) "Exercitus"))
  (is equal nil (pc:parse (pc:opt (pc:string "Ex")) "Facēre")))

(define-test "delimited"
  :parent combinators
  (is equal "Salvē" (pc:parse (pc:delimited (pc:char #\!) (pc:string "Salvē") (pc:char #\!)) "!Salvē!")))

(define-test "many0"
  :parent combinators
  (is equal nil (pc:parse (pc:many0 (pc:string "ovēs")) "ovis"))
  (is equal '("ovēs" "ovēs" "ovēs") (pc:parse (pc:many0 (pc:string "ovēs")) "ovēsovēsovēs!"))
  (is equal '("ovēs" "ovēs" "avis") (pc:parse (pc:many0 (pc:alt (pc:string "ovēs") (pc:string "avis"))) "ovēsovēsavis!")))

(define-test "many1"
  :parent combinators
  (fail (pc:parse (pc:many1 (pc:string "ovēs")) "ovis"))
  (is equal '("ovēs" "ovēs" "ovēs") (pc:parse (pc:many1 (pc:string "ovēs")) "ovēsovēsovēs!")))

(define-test "sep0"
  :parent combinators
  (is equal nil (pc:parse (pc:sep0 (pc:char #\!) (pc:string "a")) "."))
  (is equal '("a") (pc:parse (pc:sep0 (pc:char #\!) (pc:string "a")) "a."))
  (is equal '("a" "a" "a") (pc:parse (pc:sep0 (pc:char #\!) (pc:string "a")) "a!a!a."))
  (is equal '("a" "a" "a") (pc:parse (pc:sep0 (pc:char #\!) (pc:string "a")) "a!a!a!")))

(define-test fp)

(define-test "alt"
  :parent fp
  (is equal #\H (pc:parse (pc:alt (pc:char #\H) (pc:char #\h)) "Hello"))
  (is equal #\h (pc:parse (pc:alt (pc:char #\H) (pc:char #\h)) "hello"))
  (fail (pc:parse (pc:alt (pc:char #\H) (pc:char #\h)) "ello")))
