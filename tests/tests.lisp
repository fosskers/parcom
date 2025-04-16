(defpackage parcom/tests
  (:use :cl :parachute)
  (:local-nicknames (#:pc #:parcom)
                    (#:pj #:parcom/json)))

(in-package :parcom/tests)

(define-test types
  #-(or ccl abcl clisp)
  (is eq 'simple-array (car (type-of "How are you")))
  #-(or ccl abcl clisp)
  (is eq 'simple-array (car (type-of "Hōw are yōu")))
  #-(or ccl abcl clisp)
  (is eq 'simple-array (car (type-of "Hōw are う")))
  #-(or sbcl ccl abcl clisp)
  (is eq 'simple-array (car (type-of (format nil "How are you"))))
  #-(or ccl abcl clisp)
  (is eq 'simple-array (car (type-of (format nil "Hōw are yōu"))))
  #-(or ccl abcl clisp)
  (is eq 'simple-array (car (type-of (format nil "Hōw are う"))))
  (is equal #\newline #\linefeed))

(define-test parsers)

(define-test char
  :parent parsers
  (is equal #\H (pc:parse (pc:char #\H) "Hello"))
  (fail (pc:parse (pc:char #\H) "ello")))

(define-test string
  :parent parsers
  (is equal "" (pc:parse (pc:string "") "a"))
  (is equal "Hello" (pc:parse (pc:string "Hello") "Hello yes"))
  (fail (pc:parse (pc:string "HellO") "Hello yes")))

(define-test unsigned
  :parent parsers
  (fail (pc:parse #'pc:unsigned "0123"))
  (is = 0 (pc:parse #'pc:unsigned "0"))
  (is = 123 (pc:parse #'pc:unsigned "123"))
  (is = 1234567890123456789 (pc:parse #'pc:unsigned "1234567890123456789"))
  (is = 123456789012345678901234567890 (pc:parse #'pc:unsigned "123456789012345678901234567890")))

(define-test integer
  :parent parsers
  (is = 123 (pc:parse #'pc:integer "123!"))
  (is = -123 (pc:parse #'pc:integer "-123!")))

(define-test float
  :parent parsers
  (is = 123.0456d0 (pc:parse #'pc:float "123.0456!"))
  (is = -123.0456d0 (pc:parse #'pc:float "-123.0456!"))
  (is = 1.0 (pc:parse #'pc:float "1"))
  (is = 0.0 (pc:parse #'pc:float "0"))
  (is = 2.3456789012d10 (pc:parse #'pc:float "23456789012")))

(define-test take
  :parent parsers
  (is equal "" (pc:parse (pc:take 0) "Arbor"))
  (is equal "Arb" (pc:parse (pc:take 3) "Arbor"))
  (fail (pc:parse (pc:take -5) "Arbor")))

(define-test take-while
  :parent parsers
  (is equal "" (pc:parse (pc:take-while (lambda (c) (equal #\a c))) "bbb"))
  (is equal "aaa" (pc:parse (pc:take-while (lambda (c) (equal #\a c))) "aaabbb")))

(define-test take-while1
  :parent parsers
  (is equal "aaa" (pc:parse (pc:take-while1 (lambda (c) (equal #\a c))) "aaabbb"))
  (fail (pc:parse (pc:take-while1 (lambda (c) (equal #\a c))) "bbb")))

(define-test space
  :parent parsers
  (is equal "" (pc:parse #'pc:space "hi"))
  (is equal "   " (pc:parse #'pc:space "   hi")))

(define-test space1
  :parent parsers
  (is equal "   " (pc:parse #'pc:space1 "   hi"))
  (fail (pc:parse #'pc:space1 "hi")))

(define-test multispace
  :parent parsers
  (let ((chars (concatenate 'string '(#\tab #\newline #\tab))))
    (is equal chars (pc:parse #'pc:multispace chars))))

(define-test multispace1
  :parent parsers
  (let ((chars (concatenate 'string '(#\tab #\newline #\tab))))
    (is equal chars (pc:parse #'pc:multispace1 chars)))
  (fail (pc:parse #'pc:multispace1 "hello")))

(define-test rest
  :parent parsers
  (is equal '("hi" "there") (pc:parse (pc:<*> (pc:string "hi") (pc:*> #'pc:space #'pc:rest)) "hi there")))

(define-test combinators)

(define-test opt
  :parent combinators
  (is equal "Ex" (pc:parse (pc:opt (pc:string "Ex")) "Exercitus"))
  (is equal nil (pc:parse (pc:opt (pc:string "Ex")) "Facēre")))

(define-test between
  :parent combinators
  (is equal "Salvē" (pc:parse (pc:between (pc:char #\!) (pc:string "Salvē") (pc:char #\!)) "!Salvē!")))

(define-test many
  :parent combinators
  (is equal nil (pc:parse (pc:many (pc:string "ovēs")) "ovis"))
  (is equal '("ovēs" "ovēs" "ovēs") (pc:parse (pc:many (pc:string "ovēs")) "ovēsovēsovēs!"))
  (is equal '("ovēs" "ovēs" "avis") (pc:parse (pc:many (pc:alt (pc:string "ovēs") (pc:string "avis"))) "ovēsovēsavis!")))

(define-test many1
  :parent combinators
  (fail (pc:parse (pc:many1 (pc:string "ovēs")) "ovis"))
  (is equal '("ovēs" "ovēs" "ovēs") (pc:parse (pc:many1 (pc:string "ovēs")) "ovēsovēsovēs!")))

(define-test sep
  :parent combinators
  (is equal nil (pc:parse (pc:sep (pc:char #\!) (pc:string "a")) "."))
  (is equal '("a") (pc:parse (pc:sep (pc:char #\!) (pc:string "a")) "a."))
  (is equal '("a" "a" "a") (pc:parse (pc:sep (pc:char #\!) (pc:string "a")) "a!a!a."))
  (fail (pc:parse (pc:sep (pc:char #\!) (pc:string "a")) "a!a!a!")))

(define-test sep1
  :parent combinators
  (fail (pc:parse (pc:sep1 (pc:char #\!) (pc:string "a")) "."))
  (is equal '("a") (pc:parse (pc:sep1 (pc:char #\!) (pc:string "a")) "a."))
  (is equal '("a" "a" "a") (pc:parse (pc:sep1 (pc:char #\!) (pc:string "a")) "a!a!a."))
  (fail (pc:parse (pc:sep1 (pc:char #\!) (pc:string "a")) "a!a!a!")))

(define-test sep-end
  :parent combinators
  (is equal nil (pc:parse (pc:sep-end (pc:char #\!) (pc:string "a")) "."))
  (is equal '("a") (pc:parse (pc:sep-end (pc:char #\!) (pc:string "a")) "a."))
  (is equal '("a" "a" "a") (pc:parse (pc:sep-end (pc:char #\!) (pc:string "a")) "a!a!a."))
  (is equal '("a" "a" "a") (pc:parse (pc:sep-end (pc:char #\!) (pc:string "a")) "a!a!a!")))

(define-test sep-end1
  :parent combinators
  (fail (pc:parse (pc:sep-end1 (pc:char #\!) (pc:string "a")) "."))
  (is equal '("a") (pc:parse (pc:sep-end1 (pc:char #\!) (pc:string "a")) "a."))
  (is equal '("a" "a" "a") (pc:parse (pc:sep-end1 (pc:char #\!) (pc:string "a")) "a!a!a."))
  (is equal '("a" "a" "a") (pc:parse (pc:sep-end1 (pc:char #\!) (pc:string "a")) "a!a!a!")))

(define-test skip
  :parent combinators
  (is equal "hi" (pc:parser-input (funcall (pc:skip (pc:char #\!)) "!!!hi"))))

(define-test peek
  :parent combinators
  (let ((res (funcall (pc:peek (pc:string "he")) "hello")))
    (is equal "he" (pc:parser-value res))
    (is equal "hello" (pc:parser-input res))))

(define-test count
  :parent combinators
  (is equal '() (pc:parse (pc:count (pc:char #\a) 0) "aa"))
  (is equal '(#\a #\a #\a) (pc:parse (pc:count (pc:char #\a) 3) "aaaaaa"))
  (fail (pc:parse (pc:count (pc:char #\a) 3) "aa")))

(define-test recognize
  :parent combinators
  (is equal "hibye" (pc:parse (pc:recognize (pc:<*> (pc:string "hi") (pc:string "bye"))) "hibyethere"))
  (fail (pc:parse (pc:recognize (pc:<*> (pc:string "hi") (pc:string "bye"))) "hihi")))

(define-test fp)

(define-test alt
  :parent fp
  (is equal #\H (pc:parse (pc:alt (pc:char #\H) (pc:char #\h)) "Hello"))
  (is equal #\h (pc:parse (pc:alt (pc:char #\H) (pc:char #\h)) "hello"))
  (fail (pc:parse (pc:alt (pc:char #\H) (pc:char #\h)) "ello")))

(define-test <*>
  :parent fp
  (is equal '("hi") (pc:parse (pc:<*> (pc:string "hi")) "hihohum!"))
  (is equal '("hi" "ho") (pc:parse (pc:<*> (pc:string "hi") (pc:string "ho")) "hihohum!"))
  (is equal '("hi" "ho" "hum") (pc:parse (pc:<*> (pc:string "hi") (pc:string "ho") (pc:string "hum")) "hihohum!")))

(define-test json)

(define-test objects
  :parent json
  (is = 0 (hash-table-count (pj:parse "{}")))
  (true (hash-table-p (pj:parse "{\"x\": 1, \"y\": 2}")))
  (true (hash-table-p (pj:parse "{ \"x\" : 1 , \"y\":2}"))))

(define-test arrays
  :parent json
  (is equalp #() (pj:parse "[]"))
  (is equalp #(1.0 2.0) (pj:parse "[1, 2]"))
  (is equalp #(1.0 2.0) (pj:parse "[ 1 , 2 ]")))

(define-test non-ascii
  :parent json
  (is equal "hēllお🐂" (pj:parse "\"hēllお🐂\"")))

(define-test numbers
  :parent json
  (is = 0.0 (pj:parse "0"))
  (is = 1234567890.0d0 (pj:parse "1234567890"))
  (is = -9876.543210d0 (pj:parse "-9876.543210"))
  (is = 23456789012d66 (pj:parse "23456789012E66"))
  (is = 1.234567890d+34 (pj:parse "1.234567890E+34"))
  (is = 0.123456789d-12 (pj:parse "0.123456789e-12")))
