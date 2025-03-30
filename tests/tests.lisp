(defpackage parcom/tests
  (:use :cl :parachute)
  (:local-nicknames (#:pc #:parcom)))

(in-package :parcom/tests)

(define-test combinators)

(define-test "chars"
  :parent combinators
  (is equal #\H (pc:parse (pc:char #\H) "Hello"))
  (fail (pc:parse (pc:char #\H) "ello")))

(define-test "string"
  :parent combinators
  (is equal "" (pc:parse (pc:string "") "a"))
  (is equal "Hello" (pc:parse (pc:string "Hello") "Hello yes"))
  (fail (pc:parse (pc:string "HellO") "Hello yes")))

(define-test "opt"
  :parent combinators
  (is equal "Ex" (pc:parse (pc:opt (pc:string "Ex")) "Exercitus"))
  (is equal nil (pc:parse (pc:opt (pc:string "Ex")) "Facēre")))

(define-test fp)

(define-test "alt"
  :parent fp
  (is equal #\H (pc:parse (pc:alt (pc:char #\H) (pc:char #\h)) "Hello"))
  (is equal #\h (pc:parse (pc:alt (pc:char #\H) (pc:char #\h)) "hello"))
  (fail (pc:parse (pc:alt (pc:char #\H) (pc:char #\h)) "ello")))
