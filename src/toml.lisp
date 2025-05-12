(defpackage parcom/toml
  (:use :cl)
  (:shadow #:string #:integer #:number #:boolean)
  (:import-from :parcom #:<*> #:<* #:*> #:<$)
  (:local-nicknames (#:p #:parcom)
                    (#:pd #:parcom/datetime))
  ;; --- Types --- ;;
  ;; --- Entry --- ;;
  ;; --- Parsers --- ;;
  (:export #:key))

(in-package :parcom/toml)

;; --- Types --- ;;

(defstruct tiered-key
  "A key that might point to a value several tables deep."
  (key nil :type list))

(defstruct table
  (key ""  :type tiered-key)
  (kvs nil :type hash-table))

;; --- Entry --- ;;

;; TODO: 2025-05-13 Don't forget about the "top-level table".
(defun toml (offset)
  "Parser: Parse a TOML document into a Hash Table."
  (funcall (p:alt #'comment #'pair) offset))

;; --- Parsers --- ;;

(defun comment (offset)
  "Parser: Skip over any comment line."
  (funcall (*> (p:char #\#)
               (p:consume (lambda (c) (not (equal c #\newline)))))
           offset))

#+nil
(comment (p:in "# yes"))

(defun string (offset)
  "Parser: One of the four TOML string types."
  (funcall (p:alt #'basic-string
                  #'multiline-basic-string
                  #'literal-string
                  #'multiline-literal-string)
           offset))

#+nil
(string (p:in "\"hel\\u00E9lo\""))

(defun basic-string (offset)
  "Parser: Parse the simplest kind of string."
  (p:fmap (lambda (chars) (concatenate 'cl:string chars))
          (funcall (p:between (p:char #\")
                              (p:many #'compound-char)
                              (p:char #\"))
                   offset)))

#+nil
(basic-string (p:in "\"hel\\u00E9lo\""))

(defun compound-char (offset)
  "Parser: Parse a char while being wary of escaping."
  (funcall (p:alt #'escaped-char (p:any-but #\")) offset))

(defun escaped-char (offset)
  (funcall (*> (p:peek (p:char #\\))
               (p:alt #'special-char #'p:control-char #'p:unicode))
           offset))

(defun special-char (offset)
  "Parser: Backslashes and quotes."
  (funcall (*> (p:char #\\)
               (p:alt (p:char #\\) (p:char #\")))
           offset))

(defun multiline-basic-string (offset)
  "Parser: Easily include newlines characters into strings and preserve them."
  (p:fmap (lambda (chars) (concatenate 'cl:string chars))
          (funcall (p:between (<* (p:string "\"\"\"") (p:opt #'p:newline))
                              (p:many (*> (p:opt (*> (p:char #\\)
                                                     #'p:multispace1))
                                          #'compound-char))
                              (p:string "\"\"\""))
                   offset)))

#+nil
(multiline-basic-string (p:in "\"\"\"hel\\u00E9lo\"\"\""))
#+nil
(multiline-basic-string (p:in "\"\"\"\\ a  \"\"\""))

(defun literal-string (offset)
  "Parser: Strings with no escaping. These parse much faster and are more
memory efficient than `basic-string'."
  (funcall (p:between (p:char #\')
                      (p:take-while (lambda (c) (not (equal c #\'))))
                      (p:char #\'))
           offset))

#+nil
(time (dotimes (n 10000)
        (literal-string (p:in "'yes indeed'"))))
#+nil
(time (dotimes (n 10000)
        (basic-string (p:in "\"yes indeed\""))))

(defun multiline-literal-string (offset)
  "Parser: Multiline strings with no escaping."
  (funcall (p:between (<* (p:string "'''") (p:opt #'p:newline))
                      (p:take-until (p:string "'''"))
                      (p:string "'''"))
           offset))

#+nil
(multiline-literal-string (p:in "'''the cat's catnip'''"))

(defun pair (offset)
  "Parser: A key-value pair."
  (funcall (<*> #'key
                (*> #'skip-space
                    (p:char #\=)
                    #'skip-space
                    #'value))
           offset))

(defun skip-space (offset)
  "A faster variant of `space' that just advances over whitespace chars."
  (funcall (p:consume (lambda (c) (or (equal c #\space) (equal c #\tab))))
           offset))

#+nil
(funcall #'skip-space (p:in "   abc"))

(defun key (offset)
  "Parser: A key that might be pointing several layers deep."
  (p:fmap (lambda (list) (make-tiered-key :key list))
          (funcall (p:sep (p:char #\.) (p:alt #'bare-key #'quoted-key))
                   offset)))

#+nil
(key (p:in "physical"))
#+nil
(key (p:in "physical.shape"))
#+nil
(key (p:in "site.\"google.com\""))

(defun bare-key (offset)
  "Parser: Just ASCII letters, digits, dashes, and underscores."
  (funcall (p:take-while1 (lambda (c)
                            (or (p:ascii-letter? c)
                                (p:digit? c)
                                (equal c #\-)
                                (equal c #\_))))
           offset))

#+nil
(bare-key (p:in "alex-honnold"))
#+nil
(bare-key (p:in "123"))

(defun quoted-key (offset)
  "Parser: Yuck don't do these."
  (funcall (p:alt #'basic-string #'literal-string) offset))

(defun table (offset)
  (p:fmap (lambda (x)
            (destructuring-bind (name kvs) x
              (let ((ht (make-hash-table :test #'equalp)))
                (dolist (pair kvs)
                  (setf (gethash (car pair) ht) (cadr pair)))
                (make-table :key name :kvs ht))))
          (funcall (<*> (<* (p:between (p:char #\[)
                                       #'key
                                       (p:char #\]))
                            #'p:multispace)
                        (p:sep-end #'p:multispace #'pair))
                   offset)))

#+nil
(table (p:in "[foo.bar]
bar = 1
baz = \"zoo\"
zoo = 1988-07-05
"))

(defun inline-table (offset)
  "Parser: The compact form of a table."
  (p:fmap (lambda (kvs)
            (let ((ht (make-hash-table :test #'equalp)))
              (dolist (kv kvs)
                (setf (gethash (car kv) ht) (cadr kv)))
              ht))
          (funcall (p:between (*> (p:char #\{) #'skip-space)
                              (p:sep (*> (p:char #\,) #'skip-space) #'pair)
                              (*> #'skip-space (p:char #\})))
                   offset)))

#+nil
(p:parse #'inline-table "{ first = \"Tom\", last = \"Preston-Werner\" }")

;; String
;; Integer
;; Float
;; Boolean
;; Offset-date time
;; Local date-time
;; Local date
;; Local time
;; Array
;; Inline table
(defun value (offset)
  "Parser: The value portion of a key-value pair."
  (funcall (p:alt #'string #'date-time #'number)
           offset))

(defun date-time (offset)
  (funcall (p:alt #'pd:offset-date-time #'pd:local-date-time #'pd:local-date #'pd:local-time)
           offset))

(defun number (offset)
  "Parser: Any number."
  (funcall (p:alt #'integer #'hex #'octal #'binary)
           offset))

(defun integer (offset)
  "Parser: Whole numbers."
  (p:fmap (lambda (ns)
            (destructuring-bind (head rest) ns
              (if (null rest)
                  head
                  (read-from-string (format nil "~{~a~}" (cons head rest))))))
          (funcall (<*> (p:alt (*> (p:opt (p:char #\+)) #'p:unsigned)
                               #'p:integer)
                        (p:opt (*> (p:char #\_)
                                   (p:sep (p:char #\_) (p:take-while1 #'p:digit?)))))
                   offset)))

#+nil
(integer (p:in "+123"))
#+nil
(integer (p:in "-17"))
#+nil
(integer (p:in "53_49_221"))
#+nil
(integer (p:in "1_001"))

(defun hex (offset)
  "Parser: A positive hexadecimal number."
  (p:fmap (lambda (ns) (read-from-string (format nil "#x~{~a~}" ns)))
          (funcall (*> (p:string "0x")
                       (p:sep1 (p:char #\_)
                               (p:take-while1 #'p:hex?)))
                   offset)))

#+nil
(hex (p:in "0xdead_beef"))

(defun octal (offset)
  "Parser: A positive base-8 number."
  (p:fmap (lambda (ns) (read-from-string (format nil "#o~{~a~}" ns)))
          (funcall (*> (p:string "0o")
                       (p:sep1 (p:char #\_)
                               (p:take-while1 #'p:octal?)))
                   offset)))

#+nil
(octal (p:in "0o01234567"))
#+nil
(octal (p:in "0o8"))

(defun binary (offset)
  "Parser: A positive base-2 number."
  (p:fmap (lambda (ns) (read-from-string (format nil "#b~{~a~}" ns)))
          (funcall (*> (p:string "0b")
                       (p:sep1 (p:char #\_)
                               (p:take-while1 #'p:binary?)))
                   offset)))

#+nil
(binary (p:in "0b1010"))

(defun boolean (offset)
  (funcall (p:alt (<$ t (p:string "true"))
                  (<$ nil (p:string "false")))
           offset))

#+nil
(boolean (p:in "true"))
