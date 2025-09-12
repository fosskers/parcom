(defpackage parcom/tests
  (:use :cl :parachute)
  (:local-nicknames (#:p  #:parcom)
                    (#:pj #:parcom/json)
                    (#:pt #:parcom/toml)
                    (#:pd #:parcom/datetime)
                    (#:px #:parcom/xml)
                    (#:pe #:parcom/email)))

(in-package :parcom/tests)

(define-test types
  #-(or ccl abcl clisp cmucl)
  (is eq 'simple-array (car (type-of "How are you")))
  #-(or ccl abcl clisp cmucl)
  (is eq 'simple-array (car (type-of "Hōw are yōu")))
  #-(or ccl abcl clisp cmucl)
  (is eq 'simple-array (car (type-of "Hōw are う")))
  #-(or sbcl ccl abcl clisp cmucl)
  (is eq 'simple-array (car (type-of (format nil "How are you"))))
  #-(or ccl abcl clisp cmucl)
  (is eq 'simple-array (car (type-of (format nil "Hōw are yōu"))))
  #-(or ccl abcl clisp cmucl)
  (is eq 'simple-array (car (type-of (format nil "Hōw are う"))))
  (is equal #\newline #\linefeed))

(define-test parsers)

(define-test pure
  :parent parsers
  (is eq :pāx (p:parse (p:pure :pāx) "Bellum")))

(define-test char
  :parent parsers
  (is equal #\H (p:parse (p:char #\H) "Hello"))
  (fail (p:parse (p:char #\H) "ello"))
  (fail (p:parse (p:char #\H) "")))

(define-test string
  :parent parsers
  (is equal "" (p:parse (p:string "") "a"))
  (is equal "Hello" (p:parse (p:string "Hello") "Hello yes"))
  (is eq :fail (funcall (p:string "Hello") (p:in "")))
  (fail (p:parse (p:string "HellO") "Hello yes"))
  ;; Request is longer than total input.
  (fail (p:parse (p:string "arstneo") "a")))

(define-test unsigned
  :parent parsers
  (fail (p:parse #'p:unsigned "0123"))
  (is = 0 (p:parse #'p:unsigned "0"))
  (is = 123 (p:parse #'p:unsigned "123"))
  (is = 1234567890123456789 (p:parse #'p:unsigned "1234567890123456789"))
  (is = 123456789012345678901234567890 (p:parse #'p:unsigned "123456789012345678901234567890")))

(define-test integer
  :parent parsers
  (is = 123 (p:parse #'p:integer "123!"))
  (is = -123 (p:parse #'p:integer "-123!")))

(define-test float
  :parent parsers
  (is = 123.0456d0 (p:parse #'p:float "123.0456!"))
  (is = -123.0456d0 (p:parse #'p:float "-123.0456!"))
  (is = 1.0 (p:parse #'p:float "1"))
  (is = 1.0 (p:parse #'p:float "1.0"))
  (is = 0.0 (p:parse #'p:float "0"))
  (is = 2.3456789012d10 (p:parse #'p:float "23456789012")))

(define-test take
  :parent parsers
  (is equal "" (p:parse (p:take 0) "Arbor"))
  (is equal "Arb" (p:parse (p:take 3) "Arbor"))
  (is equal "Arbor" (p:parse (p:take 100) "Arbor"))
  (is equal "or" (p:parse (p:*> (p:take 3) (p:take 2)) "Arbor"))
  (is equal "or" (p:parse (p:*> (p:take 3) (p:take 3)) "Arbor"))
  (fail (p:parse (p:take -5) "Arbor")))

(define-test take-while
  :parent parsers
  (is equal "" (p:parse (p:take-while (lambda (c) (equal #\a c))) "bbb"))
  (is equal "aaa" (p:parse (p:take-while (lambda (c) (equal #\a c))) "aaabbb"))
  (is equal "bcd" (p:parse (p:*> (p:take-while (lambda (c) (equal #\a c)))
                                 (p:take-while (lambda (c)
                                                 (or (equal #\b c)
                                                     (equal #\c c)
                                                     (equal #\d c)))))
                           "aaabcd!")))

(define-test take-while1
  :parent parsers
  (is equal "aaa" (p:parse (p:take-while1 (lambda (c) (equal #\a c))) "aaabbb"))
  (fail (p:parse (p:take-while1 (lambda (c) (equal #\a c))) "bbb")))

(define-test space
  :parent parsers
  (is equal "" (p:parse #'p:space "hi"))
  (is equal "   " (p:parse #'p:space "   hi")))

(define-test space1
  :parent parsers
  (is equal "   " (p:parse #'p:space1 "   hi"))
  (fail (p:parse #'p:space1 "hi")))

(define-test multispace
  :parent parsers
  (let ((chars (concatenate 'string '(#\tab #\newline #\tab))))
    (is equal chars (p:parse #'p:multispace chars))))

(define-test multispace1
  :parent parsers
  (let ((chars (concatenate 'string '(#\tab #\newline #\tab))))
    (is equal chars (p:parse #'p:multispace1 chars)))
  (fail (p:parse #'p:multispace1 "hello")))

(define-test rest
  :parent parsers
  (is equal '("hi" "there") (p:parse (p:<*> (p:string "hi") (p:*> #'p:space #'p:rest)) "hi there")))

(define-test combinators)

(define-test opt
  :parent combinators
  (is equal "Ex" (p:parse (p:opt (p:string "Ex")) "Exercitus"))
  (is equal nil (p:parse (p:opt (p:string "Ex")) "Facēre")))

(define-test between
  :parent combinators
  (is equal "Salvē" (p:parse (p:between (p:char #\!) (p:string "Salvē") (p:char #\!)) "!Salvē!")))

(define-test many
  :parent combinators
  (is equal nil (p:parse (p:many (p:string "ovēs")) "ovis"))
  (is equal '("ovēs" "ovēs" "ovēs") (p:parse (p:many (p:string "ovēs")) "ovēsovēsovēs!"))
  (is equal '("ovēs" "ovēs" "avis") (p:parse (p:many (p:alt (p:string "ovēs") (p:string "avis"))) "ovēsovēsavis!")))

(define-test many1
  :parent combinators
  (fail (p:parse (p:many1 (p:string "ovēs")) "ovis"))
  (is equal '("ovēs" "ovēs" "ovēs") (p:parse (p:many1 (p:string "ovēs")) "ovēsovēsovēs!")))

(define-test sep
  :parent combinators
  (is equal nil (p:parse (p:sep (p:char #\!) (p:string "a")) "."))
  (is equal '("a") (p:parse (p:sep (p:char #\!) (p:string "a")) "a."))
  (is equal '("a" "a" "a") (p:parse (p:sep (p:char #\!) (p:string "a")) "a!a!a."))
  (fail (p:parse (p:sep (p:char #\!) (p:string "a")) "a!a!a!")))

(define-test sep1
  :parent combinators
  (fail (p:parse (p:sep1 (p:char #\!) (p:string "a")) "."))
  (is equal '("a") (p:parse (p:sep1 (p:char #\!) (p:string "a")) "a."))
  (is equal '("a" "a" "a") (p:parse (p:sep1 (p:char #\!) (p:string "a")) "a!a!a."))
  (fail (p:parse (p:sep1 (p:char #\!) (p:string "a")) "a!a!a!")))

(define-test sep-end
  :parent combinators
  (is equal nil (p:parse (p:sep-end (p:char #\!) (p:string "a")) "."))
  (is equal '("a") (p:parse (p:sep-end (p:char #\!) (p:string "a")) "a."))
  (is equal '("a" "a" "a") (p:parse (p:sep-end (p:char #\!) (p:string "a")) "a!a!a."))
  (is equal '("a" "a" "a") (p:parse (p:sep-end (p:char #\!) (p:string "a")) "a!a!a!")))

(define-test sep-end1
  :parent combinators
  (fail (p:parse (p:sep-end1 (p:char #\!) (p:string "a")) "."))
  (is equal '("a") (p:parse (p:sep-end1 (p:char #\!) (p:string "a")) "a."))
  (is equal '("a" "a" "a") (p:parse (p:sep-end1 (p:char #\!) (p:string "a")) "a!a!a."))
  (is equal '("a" "a" "a") (p:parse (p:sep-end1 (p:char #\!) (p:string "a")) "a!a!a!")))

(define-test skip
  :parent combinators
  (multiple-value-bind (res next)
      (funcall (p:skip (p:char #\!)) (p:in "!!!hi"))
    (declare (ignore res))
    (is = 3 next)))

(define-test peek
  :parent combinators
  (multiple-value-bind (res next) (funcall (p:peek (p:string "he")) (p:in "hello"))
    (declare (ignore next))
    (is equal "he" res)))

(define-test count
  :parent combinators
  (is equal '() (p:parse (p:count 0 (p:char #\a)) "aa"))
  (is equal '(#\a #\a #\a) (p:parse (p:count 3 (p:char #\a)) "aaaaaa"))
  (fail (p:parse (p:count 3 (p:char #\a)) "aa")))

(define-test recognize
  :parent combinators
  (is equal "hibye" (p:parse (p:recognize (p:<*> (p:string "hi") (p:string "bye"))) "hibyethere"))
  (fail (p:parse (p:recognize (p:<*> (p:string "hi") (p:string "bye"))) "hihi")))

(define-test fp)

(define-test alt
  :parent fp
  (is equal #\H (p:parse (p:alt (p:char #\H) (p:char #\h)) "Hello"))
  (is equal #\h (p:parse (p:alt (p:char #\H) (p:char #\h)) "hello"))
  (fail (p:parse (p:alt (p:char #\H) (p:char #\h)) "ello")))

(define-test <*>
  :parent fp
  (is equal '("hi") (p:parse (p:<*> (p:string "hi")) "hihohum!"))
  (is equal '("hi" "ho") (p:parse (p:<*> (p:string "hi") (p:string "ho")) "hihohum!"))
  (is equal '("hi" "ho" "hum") (p:parse (p:<*> (p:string "hi") (p:string "ho") (p:string "hum")) "hihohum!")))

(define-test pmap
  :parent fp
  (is = 124 (p:parse (p:pmap #'1+ #'p:unsigned) "123"))
  (is equal '("hi" 124) (p:parse (p:<*> (p:string "hi") (p:pmap #'1+ #'p:unsigned)) "hi123")))

(define-test json)

(define-test objects
  :parent json
  (is = 0 (hash-table-count (pj:parse "{}")))
  (true (hash-table-p (pj:parse "{\"x\": 1, \"y\": 2}")))
  (true (hash-table-p (pj:parse "{ \"x\" : 1 , \"y\":2}")))
  (of-type fixnum (gethash "x" (pj:parse "{\"x\": 1}"))))

(define-test arrays
  :parent json
  (is equalp #() (pj:parse "[]"))
  (is equalp #(1 2) (pj:parse "[1, 2]"))
  (is equalp #(1 2) (pj:parse "[ 1 , 2 ]"))
  (of-type fixnum (aref (pj:parse "[1]") 0)))

(define-test non-ascii
  :parent json
  (is equal "hello" (pj:parse "\"hello\""))
  (is equal "hēllお🐂" (pj:parse "\"hēllお🐂\""))
  (is equal "Hi α!" (pj:parse "\"Hi \\u03B1!\""))
  (is equal "/ & /" (pj:string (p:in "\"/ & \\/\"")))
  (is equal "\\u03" (pj:parse "\"\\u03\"")))

(define-test numbers
  :parent json
  (let ((n (pj:parse "0")))
    (is = 0 n)
    (of-type fixnum n))
  (multiple-value-bind (res next) (pj:number (p:in "1e00,"))
    (of-type double-float res)
    (is equal #\, (schar p::*input* next)))
  (let ((n (pj:parse "123456789")))
    (is = 123456789 n)
    (of-type fixnum n))
  (let ((n (pj:parse "1234567890")))
    (is = 1234567890 n)
    (of-type integer n))
  (is = -9876.543210d0 (pj:parse "-9876.543210"))
  (is = 23456789012d66 (pj:parse "23456789012E66"))
  (is = 1.234567890d+34 (pj:parse "1.234567890E+34"))
  (is = 0.123456789d-12 (pj:parse "0.123456789e-12")))

(define-test test-files
  :parent json
  (true (hash-table-p (pj:parse (uiop:read-file-string "tests/data/small.json"))))
  (true (arrayp (pj:parse (uiop:read-file-string "tests/data/pass1.json"))))
  (true (arrayp (pj:parse (uiop:read-file-string "tests/data/pass2.json"))))
  (true (hash-table-p (pj:parse (uiop:read-file-string "tests/data/beam-18.json")))))

(define-test toml)

(define-test toml-keys
  :parent toml
  (is equal "alex-h" (p:parse #'pt::bare-key "alex-h"))
  (is equal "123" (p:parse #'pt::bare-key "123"))
  (is equalp (pt::make-tiered-key :key '("physical" "shape"))
      (p:parse #'pt:key "physical.shape"))
  (is equalp (pt::make-tiered-key :key '("site" "google.com"))
      (p:parse #'pt:key "site.\"google.com\""))
  (is equalp (pt::make-tiered-key :key '("fruit" "colour"))
      (p:parse #'pt:key "fruit . colour")))

(define-test toml-pairs
  :parent toml
  (finish (p:parse #'pt::pair "tiered.key = true")))

(define-test toml-strings
  :parent toml
  (is equal "the cat's catnip" (p:parse #'pt::multiline-literal-string "'''the cat's catnip'''")))

(define-test toml-integers
  :parent toml
  (is = 123 (p:parse #'pt::integer "+123"))
  (is = -17 (p:parse #'pt::integer "-17"))
  (is = 0 (p:parse #'pt::integer "-0"))
  (is = 0 (p:parse #'pt::integer "+0"))
  (is = 1000 (p:parse #'pt::integer "1_000"))
  (is = 5349221 (p:parse #'pt::integer "5_349_221"))
  (is = 5349221 (p:parse #'pt::integer "53_49_221"))
  (is = 0 (p:parse #'pt::hex "0x0000_0000"))
  (fail (p:parse #'pt::hex "0x"))
  (true (= (p:parse #'pt::hex "0xDEADBEEF")
           (p:parse #'pt::hex "0xdeadbeef")
           (p:parse #'pt::hex "0xdead_beef")))
  (is = 342391 (p:parse #'pt::octal "0o01234567"))
  (fail (p:parse #'pt::octal "0o8"))
  (is = 10 (p:parse #'pt::binary "0b1010"))
  (is = 20 (p:parse #'pt::binary "0b1010_0")))

(define-test toml-floats
  :parent toml
  (is = 1.0d0 (p:parse #'pt:float "+1.0"))
  (is = 3.1415d0 (p:parse #'pt:float "3.1415"))
  (is = -0.01d0 (p:parse #'pt:float "-0.01"))
  #-ecl
  (is = 5d+22 (p:parse #'pt:float "5e+22"))
  (is = 1d06 (p:parse #'pt:float "1e06"))
  (is = -2d-2 (p:parse #'pt:float "-2E-2"))
  (is = 6.626d-34 (p:parse #'pt:float "6.626e-34"))
  (is = 224617.445991228d0 (p:parse #'pt:float "224_617.445_991_228")))

(define-test toml-tables
  :parent toml
  (is = 0 (hash-table-count (p:parse #'pt:inline-table "{}")))
  (is = 2 (hash-table-count (p:parse #'pt:inline-table "{ first = \"Tom\", last = \"Preston-Werner\" }"))))

(define-test toml-arrays
  :parent toml
  (is equal '() (p:parse #'pt:array "[]"))
  (is equal '(1 2 3) (p:parse #'pt:array "[1,2,3]"))
  (is equal '(1 2 3) (p:parse #'pt:array "[1,2,3,]"))
  (is equal '(1 2 3) (p:parse #'pt:array "[ 1 , 2 , 3 ]"))
  (is = 4 (length (p:parse #'pt:array "[1,[2],3,{\"foo\" = 1}]")))
  (is equal '((3.14d0)) (p:parse #'pt:array "[[3.14]]"))
  (is equal '(1 2 3) (p:parse #'pt:array "[
1,
2,  # comment!
3,
]")))

(define-test toml-documents
  :parent toml
  (let ((ht (pt:parse (uiop:read-file-string "tests/data/basic.toml"))))
    (of-type hash-table ht)
    (is equal "TOML Example" (gethash "title" ht))
    (true (gethash "key" (gethash "tiered" ht)))
    (is = 7 (gethash "still" (gethash "deeper" (gethash "tiered" ht))))
    (is equal "Tom Preston-Werner" (gethash "name" (gethash "owner" ht)))
    (true (gethash "enabled" (gethash "database" ht)))
    (is equal '(("delta" "phi") (3.14d0)) (gethash "data" (gethash "database" ht)))
    (is equal "frontend" (gethash "role" (gethash "alpha" (gethash "servers" ht))))
    (is equal "backend" (gethash "role" (gethash "beta" (gethash "servers" ht))))))

(define-test datetime)

(define-test local-date
  :parent datetime
  (is equalp (pd::make-local-date :year 70 :month 7 :day 20)
      (p:parse #'pd:local-date "0070-07-20"))
  (is equalp (pd::make-local-date :year 1979 :month 1 :day 2)
      (p:parse #'pd:local-date "1979-01-02"))
  (fail (p:parse #'pd:local-date "79-01-02"))
  (fail (p:parse #'pd:local-date "1979-1-02"))
  (fail (p:parse #'pd:local-date "1979-01-2")))

(define-test local-time
  :parent datetime
  (is equalp (pd::make-local-time :hour 0 :minute 32 :second 0 :millis 123)
      (p:parse #'pd:local-time "00:32:00.123"))
  (is equalp (pd::make-local-time :hour 0 :minute 32 :second 0 :millis 123)
      (p:parse #'pd:local-time "00:32:00.123456"))
  (is equalp (pd::make-local-time :hour 0 :minute 32 :second 0 :millis 100)
      (p:parse #'pd:local-time "00:32:00.1"))
  (is equalp (pd::make-local-time :hour 0 :minute 32 :second 0 :millis 0)
      (p:parse #'pd:local-time "00:32:00"))
  (is equalp (pd::make-local-time :hour 23 :minute 59 :second 60 :millis 0)
      (p:parse #'pd:local-time "23:59:60"))
  (fail (p:parse #'pd:local-time "00:76:00.123"))
  (fail (p:parse #'pd:local-time "0:10:00.123"))
  (fail (p:parse #'pd:local-time "123:10:00.123")))

(define-test local-date-time
  :parent datetime
  (is equalp (pd::make-local-date-time
              :date (pd::make-local-date :year 1979 :month 5 :day 27)
              :time (pd::make-local-time :hour 7 :minute 32 :second 0 :millis 0))
      (p:parse #'pd:local-date-time "1979-05-27T07:32:00"))
  (is equalp (pd::make-local-date-time
              :date (pd::make-local-date :year 1979 :month 5 :day 27)
              :time (pd::make-local-time :hour 7 :minute 32 :second 0 :millis 0))
      (p:parse #'pd:local-date-time "1979-05-27 07:32:00"))
  (finish (funcall (p:*> #'pd:local-date-time #'p:eof) (p:in "1979-05-27T00:32:00.999999"))))

(define-test offset-date-time
  :parent datetime
  (is equalp (pd::make-offset-date-time
              :date (pd::make-local-date :year 1979 :month 5 :day 27)
              :time (pd::make-local-time :hour 7 :minute 32 :second 0 :millis 0)
              :offset (pd::make-offset :hour 0 :minute 0))
      (p:parse #'pd:offset-date-time "1979-05-27T07:32:00Z"))
  (is equalp (pd::make-offset-date-time
              :date (pd::make-local-date :year 1979 :month 5 :day 27)
              :time (pd::make-local-time :hour 7 :minute 32 :second 0 :millis 0)
              :offset (pd::make-offset :hour 7 :minute 0))
      (p:parse #'pd:offset-date-time "1979-05-27T07:32:00+07:00"))
  (is equalp (pd::make-offset-date-time
              :date (pd::make-local-date :year 1979 :month 5 :day 27)
              :time (pd::make-local-time :hour 7 :minute 32 :second 0 :millis 0)
              :offset (pd::make-offset :hour -7 :minute 0))
      (p:parse #'pd:offset-date-time "1979-05-27T07:32:00-07:00")))

(define-test rfc-examples
  :parent datetime
  (of-type pd:offset-date-time (pd:parse "1985-04-12T23:20:50.52Z"))
  (of-type pd:offset-date-time (pd:parse "1996-12-19T16:39:57-08:00"))
  (of-type pd:offset-date-time (pd:parse "1990-12-31T23:59:60Z"))
  (of-type pd:offset-date-time (pd:parse "1990-12-31T15:59:60-08:00"))
  (of-type pd:offset-date-time (pd:parse "1937-01-01T12:00:27.87+00:20")))

(define-test xml)

(define-test xml-comment
  :parent xml
  (is equal " hello " (p:parse px::+comment+ "<!-- hello -->")))

(define-test xml-element
  :parent xml
  (let ((el (p:parse #'px::element "<greeting></greeting>")))
    (is equal "greeting" (px:element-name el))
    (is equal "" (px:element-content el)))
  (let ((el (p:parse #'px::element "<greeting>hi!</greeting>")))
    (is equal "greeting" (px:element-name el))
    (is equal "hi!" (px:element-content el)))
  (let ((el (p:parse #'px::element "<greeting>
hi!
</greeting>")))
    (is equal "greeting" (px:element-name el))
    (is equal "hi!" (px:element-content el)))
  (let ((el (p:parse #'px::element "<greeting>
hi!
<!-- comment -->
there!
</greeting>")))
    (is equal "greeting" (px:element-name el))
    (is equal '("hi!" "there!") (px:element-content el)))
  (let ((el (p:parse #'px::element "<greeting>
<!-- comment -->
hi!
<!-- comment -->
</greeting>")))
    (is equal "greeting" (px:element-name el))
    (is equal "hi!" (px:element-content el)))
  (is = 2 (hash-table-count (px:element-content (p:parse #'px::element "<phrases><greeting>hi!</greeting><farewell>bye!</farewell></phrases>")))))

(define-test xml-open-tag
  :parent xml
  (is equal "greeting" (p:parse #'px::open-tag "<greeting>"))
  (destructuring-bind (name . meta)
      (p:parse #'px::open-tag "<greeting foo=\"bar\" baz=\"zoo\">")
    (is equal "greeting" name)
    (is = 2 (hash-table-count meta)))
  (let ((elem (p:parse #'px::open-tag "<greeting foo=\"bar\" baz=\"zoo\"/>")))
    (is equal "greeting" (px:element-name elem))
    (is = 2 (hash-table-count (px:element-metadata elem))))
  (let ((elem (p:parse #'px::open-tag "<greeting/>")))
    (is equal "greeting" (px:element-name elem)))
  (let ((elem (p:parse #'px::open-tag "<greeting />")))
    (is equal "greeting" (px:element-name elem))))

(define-test xml-doctype
  :parent xml
  (finish (p:parse #'px::doctype "<!DOCTYPE supplementalData SYSTEM \"../../common/dtd/ldmlSupplemental.dtd\">")))

(define-test xml-documents
  :parent xml
  (finish (px:parse (uiop:read-file-string "tests/data/java.pom")))
  (finish (px:parse (uiop:read-file-string "tests/data/log4j.pom")))
  (finish (px:parse (uiop:read-file-string "tests/data/lang3.pom"))))

(define-test email)

(define-test email-basic
  :parent email
  (let ((e (pe:parse "alice@bob.com")))
    (is string= "alice" (pe:address-name e))
    (is string= "bob.com" (pe:address-domain e)))
  (let ((e (pe:parse "a.l.i.c.e@bob.com")))
    (is string= "a.l.i.c.e" (pe:address-name e))))

(define-test email-strange
  :parent email
  (finish (pe:parse "email@[123.123.123.123]"))
  (finish (pe:parse "much.\"more\\ unusual\"@example.com"))
  (finish (pe:parse "very.unusual.\"@\".unusual.com@example.com")))

(define-test email-from-spec
  (finish (pe:parse "1234@local.machine.example"))
  (finish (pe:parse "pete(his account)@silly.test(his host)"))
  (finish (pe:parse "c@(Chris's host.)public.example")))

;; Obsolete syntax that we must nonetheless parse.
(define-test email-obsolete
  :parent email
  (is string= "a.l.i.c.e" (pe:address-name (pe:parse "a . l . i . c . e@bob.com")))
  (finish (pe:parse "jdoe@machine(comment).  example")))

;; Things that are obviously false.
(define-test email-failures
  :parent email
  (fail (pe:parse "a..lice@bob.com"))
  (fail (pe:parse "@bob.com"))
  (fail (pe:parse "a@"))
  (fail (pe:parse "@"))
  (fail (pe:parse ""))
  (fail (pe:parse "alice@bob@charles.com"))
  (fail (pe:parse "#@%^%#$@#$@#.com"))
  (fail (pe:parse "\"(),:;<>[\\]@example.com")))

(define-test email-local
  :parent email
  (is string= "alice" (p:parse #'pe::local-part "   alice   "))
  (is string= "alice" (p:parse #'pe::local-part "   (comment)alice(comment)   "))
  (is string= "a.l.i.c.e" (p:parse #'pe::local-part "a . l . i . c . e")))
