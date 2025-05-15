(defpackage parcom/toml
  (:use :cl)
  (:shadow #:string #:integer #:number #:boolean #:array #:float)
  (:import-from :parcom #:<*> #:<* #:*> #:<$)
  (:local-nicknames (#:p #:parcom)
                    (#:pd #:parcom/datetime))
  ;; --- Types --- ;;
  ;; --- Entry --- ;;
  ;; --- Parsers --- ;;
  (:export #:toml
           #:key
           #:number #:float
           #:table #:inline-table #:array))

(in-package :parcom/toml)

;; --- Types --- ;;

(defstruct tiered-key
  "A key that might point to a value several tables deep."
  (key nil :type list))

(defstruct table
  (key ""  :type tiered-key)
  (kvs nil :type hash-table))

(defstruct arrayed-table
  (key ""  :type tiered-key)
  (kvs nil :type hash-table))

;; --- Entry --- ;;

(defun toml (offset)
  "Parser: Parse a TOML document into a Hash Table."
  (p:fmap (lambda (xs)
            (destructuring-bind (top-level-pairs tables) xs
              (let ((ht (make-hash-table :test #'equal)))
                (dolist (pair top-level-pairs)
                  (write-into-hash-table ht (tiered-key-key (car pair)) (cadr pair)))
                (dolist (table tables)
                  (write-into-hash-table ht (tiered-key-key (table-key table))
                                         (table-kvs table)))

                ht)))
          (funcall (<*> (*> #'skip-all-space
                            (p:skip (*> #'comment #'skip-all-space))
                            (p:sep-end (*> #'skip-all-space
                                           (p:skip (*> #'comment #'skip-all-space)))
                                       #'pair))
                        (p:sep-end (*> #'skip-all-space
                                       (p:skip (*> #'comment #'skip-all-space)))
                                   #'table))
                   offset)))

#+nil
(p:parse #'toml (uiop:read-file-string "tests/data/basic.toml"))

;; TODO: Handle tiered keys within tables, when the `item' you're writing is
;; itself a hash table.
(defun write-into-hash-table (ht tiered-key item)
  "Descend into nested Hash Tables until we exhaust the depth of a tiered key,
and write its value there."
  (unless (null tiered-key)
    (destructuring-bind (head &rest rest) tiered-key
      (let ((next (gethash head ht)))
        (cond
          ;; The user is not allowed to set multiple values to the same key.
          ((and next (null rest))
           (error "Value already set at key: ~a" head))
          ;; Usual case (1): a value hasn't yet been set for this key, and the
          ;; value itself is a table, so we need to descend through it as well.
          ((and (null next) (null rest) (hash-table-p item))
           (let ((new (make-hash-table :test #'equal)))
             (setf (gethash head ht) new)
             (maphash (lambda (k v) (write-into-hash-table new (tiered-key-key k) v))
                      item)))
          ;; Usual case (2): a value hasn't yet been set for this key, and we
          ;; need not descend any further through the tiered-key.
          ((and (null next) (null rest))
           (setf (gethash head ht) item))
          ;; There is a nested table here, and we need to go deeper.
          ((and next (hash-table-p next) rest)
           (write-into-hash-table next rest item))
          ;; We need to go deeper, but no intermediate table has been written
          ;; yet.
          ((and (null next) rest)
           (let ((deeper (make-hash-table :test #'equal)))
             (setf (gethash head ht) deeper)
             (write-into-hash-table deeper rest item))))))))

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

(defun skip-all-space (offset)
  "Like `skip-space' but consumes newlines as well."
  (funcall (p:consume #'p:space?) offset))

(defun key (offset)
  "Parser: A key that might be pointing several layers deep."
  (p:fmap (lambda (list) (make-tiered-key :key list))
          (funcall (p:sep (*> (p:char #\.) #'skip-all-space)
                          (<* (p:alt #'bare-key #'quoted-key)
                              #'skip-all-space))
                   offset)))

#+nil
(key (p:in "physical"))
#+nil
(key (p:in "physical.shape"))

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
                            #'skip-all-space)
                        (p:sep-end #'skip-all-space #'pair))
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

(defun array (offset)
  "Parser: A list of values."
  (funcall (p:between (*> (p:char #\[) #'skip-all-space)
                      (p:sep-end (*> (p:char #\,) #'skip-all-space (p:opt #'comment) #'skip-all-space)
                                 (<* #'value #'skip-all-space))
                      (p:char #\]))
           offset))

#+nil
(p:parse #'array "[
1,
2,  # comment!
3,
]")

(defun table-array (offset)
  "Parser: An entry in an array-of-tables."
  (p:fmap (lambda (x)
            (destructuring-bind (name kvs) x
              (let ((ht (make-hash-table :test #'equalp)))
                (dolist (pair kvs)
                  (setf (gethash (car pair) ht) (cadr pair)))
                (make-arrayed-table :key name :kvs ht))))
          (funcall (<*> (<* (p:between (p:string "[[")
                                       #'key
                                       (p:string "]]"))
                            #'skip-all-space)
                        (p:sep-end #'skip-all-space #'pair))
                   offset)))

#+nil
(p:parse #'table-array "[[products]]
name = \"Hammer\"
sku = 12345")

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
  (funcall (p:alt #'string #'date-time #'number #'bool #'inline-table #'array)
           offset))

(defun bool (offset)
  "Parser: True or false."
  (funcall (p:alt (<$ t   (p:string "true"))
                  (<$ nil (p:string "false")))
           offset))

(defun date-time (offset)
  (funcall (p:alt #'pd:offset-date-time #'pd:local-date-time #'pd:local-date #'pd:local-time)
           offset))

(defun number (offset)
  "Parser: Any number."
  (funcall (p:alt #'integer #'hex #'octal #'binary)
           offset))

(defun float (offset)
  "Parser: A Lisp double-float. Does not support NaN or infinity."
  (p:fmap (lambda (parts)
            (destructuring-bind (sign before after exp) parts
              (let* ((*read-default-float-format* 'double-float)
                     (e (or exp ""))
                     (s (format nil "~{~a~}.~{~a~}~a" before after e))
                     (n (read-from-string s)))
                (if (eq :neg sign) (- n) n))))
          (funcall (<*> (p:opt (p:alt (<$ :pos (p:char #\+))
                                      (<$ :neg (p:char #\-))))
                        (p:alt (p:pmap #'list (p:char #\0))
                               (p:sep (p:char #\_)
                                      (p:take-while1 #'p:digit?)))
                        (p:opt (*> (p:char #\.)
                                   (p:sep (p:char #\_)
                                          (p:take-while1 #'p:digit?))))
                        (p:opt (p:recognize (*> (p:alt (p:char #\e) (p:char #\E))
                                                (p:opt (p:alt (p:char #\+) (p:char #\-)))
                                                (p:take-while1 #'p:digit?)))))
                   offset)))

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
