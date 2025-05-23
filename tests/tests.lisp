(defpackage parcom/tests
  (:use :cl :parachute)
  (:local-nicknames (#:pc #:parcom)
                    (#:pj #:parcom/json)
                    (#:pt #:parcom/toml)
                    (#:pd #:parcom/datetime)
                    (#:px #:parcom/xml)))

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

(define-test pure
  :parent parsers
  (is eq :pāx (pc:parse (pc:pure :pāx) "Bellum")))

(define-test char
  :parent parsers
  (is equal #\H (pc:parse (pc:char #\H) "Hello"))
  (fail (pc:parse (pc:char #\H) "ello"))
  (fail (pc:parse (pc:char #\H) "")))

(define-test string
  :parent parsers
  (is equal "" (pc:parse (pc:string "") "a"))
  (is equal "Hello" (pc:parse (pc:string "Hello") "Hello yes"))
  (is eq :fail (funcall (pc:string "Hello") (pc:in "")))
  (fail (pc:parse (pc:string "HellO") "Hello yes"))
  ;; Request is longer than total input.
  (fail (pc:parse (pc:string "arstneo") "a")))

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
  (is equal "Arbor" (pc:parse (pc:take 100) "Arbor"))
  (is equal "or" (pc:parse (pc:*> (pc:take 3) (pc:take 2)) "Arbor"))
  (is equal "or" (pc:parse (pc:*> (pc:take 3) (pc:take 3)) "Arbor"))
  (fail (pc:parse (pc:take -5) "Arbor")))

(define-test take-while
  :parent parsers
  (is equal "" (pc:parse (pc:take-while (lambda (c) (equal #\a c))) "bbb"))
  (is equal "aaa" (pc:parse (pc:take-while (lambda (c) (equal #\a c))) "aaabbb"))
  (is equal "bcd" (pc:parse (pc:*> (pc:take-while (lambda (c) (equal #\a c)))
                                   (pc:take-while (lambda (c)
                                                    (or (equal #\b c)
                                                        (equal #\c c)
                                                        (equal #\d c)))))
                            "aaabcd!")))

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
  (multiple-value-bind (res next)
      (funcall (pc:skip (pc:char #\!)) (pc:in "!!!hi"))
    (declare (ignore res))
    (is = 3 next)))

(define-test peek
  :parent combinators
  (multiple-value-bind (res next) (funcall (pc:peek (pc:string "he")) (pc:in "hello"))
    (declare (ignore next))
    (is equal "he" res)))

(define-test count
  :parent combinators
  (is equal '() (pc:parse (pc:count 0 (pc:char #\a)) "aa"))
  (is equal '(#\a #\a #\a) (pc:parse (pc:count 3 (pc:char #\a)) "aaaaaa"))
  (fail (pc:parse (pc:count 3 (pc:char #\a)) "aa")))

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

(define-test pmap
  :parent fp
  (is = 124 (pc:parse (pc:pmap #'1+ #'pc:unsigned) "123"))
  (is equal '("hi" 124) (pc:parse (pc:<*> (pc:string "hi") (pc:pmap #'1+ #'pc:unsigned)) "hi123")))

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
  (is equal "hello" (pj:parse "\"hello\""))
  (is equal "hēllお🐂" (pj:parse "\"hēllお🐂\""))
  (is equal "Hi α!" (pj:parse "\"Hi \\u03B1!\""))
  (is equal "/ & /" (pj:string (pc:in "\"/ & \\/\"")))
  (is equal "\\u03" (pj:parse "\"\\u03\"")))

(define-test numbers
  :parent json
  (is = 0.0 (pj:parse "0"))
  (multiple-value-bind (res next) (pj:scientific (pc:in "1e00,"))
    (declare (ignore res))
    (is equal #\, (schar pc::*input* next)))
  (is = 1234567890.0d0 (pj:parse "1234567890"))
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
  (is equal "alex-h" (pc:parse #'pt::bare-key "alex-h"))
  (is equal "123" (pc:parse #'pt::bare-key "123"))
  (is equalp (pt::make-tiered-key :key '("physical" "shape"))
      (pc:parse #'pt:key "physical.shape"))
  (is equalp (pt::make-tiered-key :key '("site" "google.com"))
      (pc:parse #'pt:key "site.\"google.com\""))
  (is equalp (pt::make-tiered-key :key '("fruit" "colour"))
      (pc:parse #'pt:key "fruit . colour")))

(define-test toml-pairs
  :parent toml
  (finish (pc:parse #'pt::pair "tiered.key = true")))

(define-test toml-strings
  :parent toml
  (is equal "the cat's catnip" (pc:parse #'pt::multiline-literal-string "'''the cat's catnip'''")))

(define-test toml-integers
  :parent toml
  (is = 123 (pc:parse #'pt::integer "+123"))
  (is = -17 (pc:parse #'pt::integer "-17"))
  (is = 0 (pc:parse #'pt::integer "-0"))
  (is = 0 (pc:parse #'pt::integer "+0"))
  (is = 1000 (pc:parse #'pt::integer "1_000"))
  (is = 5349221 (pc:parse #'pt::integer "5_349_221"))
  (is = 5349221 (pc:parse #'pt::integer "53_49_221"))
  (is = 0 (pc:parse #'pt::hex "0x0000_0000"))
  (fail (pc:parse #'pt::hex "0x"))
  (true (= (pc:parse #'pt::hex "0xDEADBEEF")
           (pc:parse #'pt::hex "0xdeadbeef")
           (pc:parse #'pt::hex "0xdead_beef")))
  (is = 342391 (pc:parse #'pt::octal "0o01234567"))
  (fail (pc:parse #'pt::octal "0o8"))
  (is = 10 (pc:parse #'pt::binary "0b1010"))
  (is = 20 (pc:parse #'pt::binary "0b1010_0")))

(define-test toml-floats
  :parent toml
  (is = 1.0d0 (pc:parse #'pt:float "+1.0"))
  (is = 3.1415d0 (pc:parse #'pt:float "3.1415"))
  (is = -0.01d0 (pc:parse #'pt:float "-0.01"))
  #-ecl
  (is = 5d+22 (pc:parse #'pt:float "5e+22"))
  (is = 1d06 (pc:parse #'pt:float "1e06"))
  (is = -2d-2 (pc:parse #'pt:float "-2E-2"))
  (is = 6.626d-34 (pc:parse #'pt:float "6.626e-34"))
  (is = 224617.445991228d0 (pc:parse #'pt:float "224_617.445_991_228")))

(define-test toml-tables
  :parent toml
  (is = 0 (hash-table-count (pc:parse #'pt:inline-table "{}")))
  (is = 2 (hash-table-count (pc:parse #'pt:inline-table "{ first = \"Tom\", last = \"Preston-Werner\" }"))))

(define-test toml-arrays
  :parent toml
  (is equal '() (pc:parse #'pt:array "[]"))
  (is equal '(1 2 3) (pc:parse #'pt:array "[1,2,3]"))
  (is equal '(1 2 3) (pc:parse #'pt:array "[1,2,3,]"))
  (is equal '(1 2 3) (pc:parse #'pt:array "[ 1 , 2 , 3 ]"))
  (is = 4 (length (pc:parse #'pt:array "[1,[2],3,{\"foo\" = 1}]")))
  (is equal '((3.14d0)) (pc:parse #'pt:array "[[3.14]]"))
  (is equal '(1 2 3) (pc:parse #'pt:array "[
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
      (pc:parse #'pd:local-date "0070-07-20"))
  (is equalp (pd::make-local-date :year 1979 :month 1 :day 2)
      (pc:parse #'pd:local-date "1979-01-02"))
  (fail (pc:parse #'pd:local-date "79-01-02"))
  (fail (pc:parse #'pd:local-date "1979-1-02"))
  (fail (pc:parse #'pd:local-date "1979-01-2")))

(define-test local-time
  :parent datetime
  (is equalp (pd::make-local-time :hour 0 :minute 32 :second 0 :millis 123)
      (pc:parse #'pd:local-time "00:32:00.123"))
  (is equalp (pd::make-local-time :hour 0 :minute 32 :second 0 :millis 123)
      (pc:parse #'pd:local-time "00:32:00.123456"))
  (is equalp (pd::make-local-time :hour 0 :minute 32 :second 0 :millis 100)
      (pc:parse #'pd:local-time "00:32:00.1"))
  (is equalp (pd::make-local-time :hour 0 :minute 32 :second 0 :millis 0)
      (pc:parse #'pd:local-time "00:32:00"))
  (is equalp (pd::make-local-time :hour 23 :minute 59 :second 60 :millis 0)
      (pc:parse #'pd:local-time "23:59:60"))
  (fail (pc:parse #'pd:local-time "00:76:00.123"))
  (fail (pc:parse #'pd:local-time "0:10:00.123"))
  (fail (pc:parse #'pd:local-time "123:10:00.123")))

(define-test local-date-time
  :parent datetime
  (is equalp (pd::make-local-date-time
              :date (pd::make-local-date :year 1979 :month 5 :day 27)
              :time (pd::make-local-time :hour 7 :minute 32 :second 0 :millis 0))
      (pc:parse #'pd:local-date-time "1979-05-27T07:32:00"))
  (is equalp (pd::make-local-date-time
              :date (pd::make-local-date :year 1979 :month 5 :day 27)
              :time (pd::make-local-time :hour 7 :minute 32 :second 0 :millis 0))
      (pc:parse #'pd:local-date-time "1979-05-27 07:32:00"))
  (finish (funcall (pc:*> #'pd:local-date-time #'pc:eof) (pc:in "1979-05-27T00:32:00.999999"))))

(define-test offset-date-time
  :parent datetime
  (is equalp (pd::make-offset-date-time
              :date (pd::make-local-date :year 1979 :month 5 :day 27)
              :time (pd::make-local-time :hour 7 :minute 32 :second 0 :millis 0)
              :offset (pd::make-offset :hour 0 :minute 0))
      (pc:parse #'pd:offset-date-time "1979-05-27T07:32:00Z"))
  (is equalp (pd::make-offset-date-time
              :date (pd::make-local-date :year 1979 :month 5 :day 27)
              :time (pd::make-local-time :hour 7 :minute 32 :second 0 :millis 0)
              :offset (pd::make-offset :hour 7 :minute 0))
      (pc:parse #'pd:offset-date-time "1979-05-27T07:32:00+07:00"))
  (is equalp (pd::make-offset-date-time
              :date (pd::make-local-date :year 1979 :month 5 :day 27)
              :time (pd::make-local-time :hour 7 :minute 32 :second 0 :millis 0)
              :offset (pd::make-offset :hour -7 :minute 0))
      (pc:parse #'pd:offset-date-time "1979-05-27T07:32:00-07:00")))

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
  (is equal " hello " (pc:parse #'px::comment "<!-- hello -->")))

(define-test xml-element
  :parent xml
  (let ((el (pc:parse #'px::element "<greeting></greeting>")))
    (is equal "greeting" (px:element-name el))
    (is equal "" (px:element-content el)))
  (let ((el (pc:parse #'px::element "<greeting>hi!</greeting>")))
    (is equal "greeting" (px:element-name el))
    (is equal "hi!" (px:element-content el)))
  (let ((el (pc:parse #'px::element "<greeting>
hi!
</greeting>")))
    (is equal "greeting" (px:element-name el))
    (is equal "hi!" (px:element-content el)))
  (let ((el (pc:parse #'px::element "<greeting>
hi!
<!-- comment -->
there!
</greeting>")))
    (is equal "greeting" (px:element-name el))
    (is equal '("hi!" "there!") (px:element-content el)))
  (let ((el (pc:parse #'px::element "<greeting>
<!-- comment -->
hi!
<!-- comment -->
</greeting>")))
    (is equal "greeting" (px:element-name el))
    (is equal "hi!" (px:element-content el)))
  (is = 2 (hash-table-count (px:element-content (pc:parse #'px::element "<phrases><greeting>hi!</greeting><farewell>bye!</farewell></phrases>")))))

(define-test xml-open-tag
  :parent xml
  (is equal "greeting" (pc:parse #'px::open-tag "<greeting>"))
  (destructuring-bind (name . meta)
      (pc:parse #'px::open-tag "<greeting foo=\"bar\" baz=\"zoo\">")
    (is equal "greeting" name)
    (is = 2 (hash-table-count meta)))
  (let ((elem (pc:parse #'px::open-tag "<greeting foo=\"bar\" baz=\"zoo\"/>")))
    (is equal "greeting" (px:element-name elem))
    (is = 2 (hash-table-count (px:element-metadata elem))))
  (let ((elem (pc:parse #'px::open-tag "<greeting/>")))
    (is equal "greeting" (px:element-name elem))))

(define-test xml-documents
  :parent xml
  (finish (px:parse (uiop:read-file-string "tests/data/java.pom")))
  (finish (px:parse (uiop:read-file-string "tests/data/log4j.pom"))))
