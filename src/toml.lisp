(defpackage parcom/toml
  (:use :cl)
  (:shadow #:string #:integer #:number #:boolean #:array #:float)
  (:import-from :parcom #:<* #:*> #:<$ #:fn #:-> #:always #:maybe)
  (:local-nicknames (#:p #:parcom)
                    (#:pd #:parcom/datetime))
  ;; --- Types --- ;;
  (:export #:table #:table-key #:table-kvs
           #:tiered-key #:tiered-key-key
           #:arrayed-table #:arrayed-table-key #:arrayed-table-kvs)
  ;; --- Entry --- ;;
  (:export #:parse)
  ;; --- Parsers --- ;;
  (:export #:toml
           #:key #:value #:pair
           #:number
           #:table #:inline-table #:array))

(in-package :parcom/toml)

;; --- Types --- ;;

(deftype toml-value ()
  "The value portion of a key-value pair."
  `(or cl:string pd::date-time fixnum double-float cl:boolean hash-table list))

(defstruct tiered-key
  "A key that might point to a value several tables deep."
  (key nil :type list))

(defstruct table
  (key ""  :type tiered-key)
  (kvs nil :type hash-table))

(defstruct arrayed-table
  (key ""  :type tiered-key)
  (kvs nil :type hash-table))

;; --- Static Parsers --- ;;

(defparameter +array-close+  (p:char #\]))
(defparameter +array-open+   (p:char #\[))
(defparameter +backslash+    (p:char #\\))
(defparameter +big-e+        (p:char #\E))
(defparameter +comma+        (p:char #\,))
(defparameter +dash+         (p:char #\-))
(defparameter +double-quote+ (p:char #\"))
(defparameter +equal+        (p:char #\=))
(defparameter +octothorp+    (p:char #\#))
(defparameter +period+       (p:char #\.))
(defparameter +plus+         (p:char #\+))
(defparameter +quote+        (p:char #\'))
(defparameter +small-e+      (p:char #\e))
(defparameter +table-close+  (p:char #\}))
(defparameter +table-open+   (p:char #\{))
(defparameter +underscore+   (p:char #\_))

(defparameter +bin-start+    (p:string "0b"))
(defparameter +false+        (p:string "false"))
(defparameter +hex-start+    (p:string "0x"))
(defparameter +oct-start+    (p:string "0o"))
(defparameter +ta-close+     (p:string "]]"))
(defparameter +ta-open+      (p:string "[["))
(defparameter +true+         (p:string "true"))
(defparameter +tripleq+      (p:string "'''"))
(defparameter +tripledq+     (p:string "\"\"\""))

(defparameter +chars1+       (p:take-while (lambda (c) (not (char= c #\')))))
(defparameter +digits1+      (p:take-while1 #'p:digit?))
(defparameter +digit-chunks+ (p:sep1 +underscore+ +digits1+))
(defparameter +not-quote+    (p:any-but #\"))

(defparameter +skip-til-end+ (p:consume (lambda (c) (not (char= c #\newline)))))
(defparameter +skip-space+   (p:consume (lambda (c) (or (equal c #\space) (equal c #\tab)))))
(defparameter +skip-junk+    (p:consume #'p:space?))
(defparameter +skip-space-and-comments+ (*> +skip-junk+ (p:skip (*> #'comment +skip-junk+))))

;; --- Entry --- ;;

(fn parse (-> p::char-string hash-table))
(defun parse (input)
  "Attempt to parse any JSON value."
  (p:parse (<* #'toml +skip-space-and-comments+ #'p:eof) input))

;; --- Parsers --- ;;

(fn toml (maybe hash-table))
(defun toml (offset)
  "Parser: Parse a TOML document into a Hash Table."
  (funcall
   (p:ap (lambda (top-level-pairs tables)
           (let ((ht (make-hash-table :test #'equal)))
             (dolist (pair top-level-pairs)
               (write-into-hash-table ht (tiered-key-key (car pair)) (cdr pair)))
             (dolist (table tables)
               (etypecase table
                 (table (write-into-hash-table ht (tiered-key-key (table-key table))
                                               (table-kvs table)))
                 (arrayed-table (write-into-hash-table ht (tiered-key-key (arrayed-table-key table))
                                                       (arrayed-table-kvs table)
                                                       :append t))))

             ht))
         (*> +skip-space-and-comments+
             (p:sep-end +skip-space-and-comments+ #'pair))
         (p:sep-end +skip-space-and-comments+
                    (p:alt #'table-array #'table)))
   offset))

#+nil
(p:parse #'toml (uiop:read-file-string "tests/data/basic.toml"))

(defun write-into-hash-table (ht tiered-key item &key (append nil))
  "Descend into nested Hash Tables until we exhaust the depth of a tiered key,
and write its value there."
  (unless (null tiered-key)
    (destructuring-bind (head &rest rest) tiered-key
      (multiple-value-bind (next existed?) (gethash head ht)
        (cond
          ;; The user is not allowed to set multiple values to the same key.
          ((and existed? (null rest) (not append))
           (error "Value already set at key: ~a" head))
          ;; An array-of-tables item needs to be appended.
          ((and existed? (listp next) (null rest) append (hash-table-p item))
           (let ((new (make-hash-table :test #'equal)))
             (setf (gethash head ht) (cons new next))
             (maphash (lambda (k v) (write-into-hash-table new (tiered-key-key k) v))
                      item)))
          ;; A new array-of-tables needs to be created.
          ((and (not existed?) (null rest) append (hash-table-p item))
           (let ((new (make-hash-table :test #'equal)))
             (setf (gethash head ht) (list new))
             (maphash (lambda (k v) (write-into-hash-table new (tiered-key-key k) v))
                      item)))
          ;; The case where they've mixed dotted keys and non-dotted keys under
          ;; a single parent array-of-tables. Consider the "fruits" example.
          ((and existed? rest append (listp next))
           (write-into-hash-table (car next) rest item :append t))
          ;; Usual case (1): a value hasn't yet been set for this key, and the
          ;; value itself is a table, so we need to descend through it as well.
          ((and (not existed?) (null rest) (hash-table-p item))
           (let ((new (make-hash-table :test #'equal)))
             (setf (gethash head ht) new)
             (maphash (lambda (k v) (write-into-hash-table new (tiered-key-key k) v))
                      item)))
          ;; Usual case (2): a value hasn't yet been set for this key, and we
          ;; need not descend any further through the tiered-key.
          ((and (not existed?) (null rest))
           (setf (gethash head ht) item))
          ;; There is a nested table here, and we need to go deeper.
          ((and existed? (hash-table-p next) rest)
           (write-into-hash-table next rest item :append append))
          ;; We need to go deeper, but no intermediate table has been written
          ;; yet.
          ((and (not existed?) rest)
           (let ((deeper (make-hash-table :test #'equal)))
             (setf (gethash head ht) deeper)
             (write-into-hash-table deeper rest item))))))))

(fn comment (maybe fixnum))
(defun comment (offset)
  "Parser: Skip over any comment line."
  (funcall (*> +octothorp+ +skip-til-end+) offset))

#+nil
(comment (p:in "# yes"))

(fn string (maybe cl:string))
(defun string (offset)
  "Parser: One of the four TOML string types."
  (funcall (p:alt #'basic-string
                  #'multiline-basic-string
                  #'literal-string
                  #'multiline-literal-string)
           offset))

#+nil
(string (p:in "\"hel\\u00E9lo\""))

(fn basic-string (maybe cl:string))
(defun basic-string (offset)
  "Parser: Parse the simplest kind of string."
  (funcall (p:ap (lambda (chars) (concatenate 'cl:string chars))
                 (p:between +double-quote+
                            (p:many #'compound-char)
                            +double-quote+))
           offset))

#+nil
(basic-string (p:in "\"hel\\u00E9lo\""))

(fn compound-char (maybe character))
(defun compound-char (offset)
  "Parser: Parse a char while being wary of escaping."
  (funcall (p:alt #'escaped-char +not-quote+) offset))

(fn escaped-char (maybe character))
(defun escaped-char (offset)
  (funcall (*> (p:peek +backslash+)
               (p:alt #'special-char #'p:control-char #'p:unicode))
           offset))

(fn special-char (maybe character))
(defun special-char (offset)
  "Parser: Backslashes and quotes."
  (funcall (*> +backslash+
               (p:alt +backslash+ +double-quote+))
           offset))

(fn multiline-basic-string (maybe cl:string))
(defun multiline-basic-string (offset)
  "Parser: Easily include newlines characters into strings and preserve them."
  (funcall (p:ap (lambda (chars) (concatenate 'cl:string chars))
                 (p:between (<* +tripledq+ (p:opt #'p:newline))
                            (p:many (*> (p:opt (*> +backslash+ #'p:multispace1))
                                        #'compound-char))
                            +tripledq+))
           offset))

#+nil
(multiline-basic-string (p:in "\"\"\"hel\\u00E9lo\"\"\""))
#+nil
(multiline-basic-string (p:in "\"\"\"\\ a  \"\"\""))

(fn literal-string (maybe cl:string))
(defun literal-string (offset)
  "Parser: Strings with no escaping. These parse much faster and are more
memory efficient than `basic-string'."
  (funcall (p:between +quote+ +chars1+ +quote+) offset))

#+nil
(time (dotimes (n 10000)
        (literal-string (p:in "'yes indeed'"))))
#+nil
(time (dotimes (n 10000)
        (basic-string (p:in "\"yes indeed\""))))

(fn multiline-literal-string (maybe cl:string))
(defun multiline-literal-string (offset)
  "Parser: Multiline strings with no escaping."
  (funcall (p:between (<* +tripleq+ (p:opt #'p:newline))
                      (p:take-until +tripleq+)
                      +tripleq+)
           offset))

#+nil
(multiline-literal-string (p:in "'''the cat's catnip'''"))

(fn pair (maybe cons))
(defun pair (offset)
  "Parser: A key-value pair."
  (funcall (p:ap #'cons
                 #'key
                 (*> +skip-space+ +equal+ +skip-space+ #'value))
           offset))

(fn key (maybe tiered-key))
(defun key (offset)
  "Parser: A key that might be pointing several layers deep."
  (funcall (p:ap (lambda (list) (make-tiered-key :key list))
                 (p:sep (*> +period+ +skip-junk+)
                        (<* (p:alt #'bare-key #'quoted-key)
                            +skip-junk+)))
           offset))

#+nil
(key (p:in "physical"))
#+nil
(key (p:in "physical.shape"))

(fn bare-key (maybe cl:string))
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

(fn quoted-key (maybe cl:string))
(defun quoted-key (offset)
  "Parser: Yuck don't do these."
  (funcall (p:alt #'basic-string #'literal-string) offset))

(fn table (maybe table))
(defun table (offset)
  (funcall (p:ap (lambda (name kvs)
                   (let ((ht (make-hash-table :test #'equalp)))
                     (dolist (pair kvs)
                       (setf (gethash (car pair) ht) (cdr pair)))
                     (make-table :key name :kvs ht)))
                 (<* (p:between +array-open+
                                #'key
                                +array-close+)
                     +skip-space-and-comments+)
                 (p:sep-end +skip-space-and-comments+ #'pair))
           offset))

#+nil
(table (p:in "[foo.bar]
bar = 1
baz = \"zoo\"
zoo = 1988-07-05
"))

(fn inline-table (maybe hash-table))
(defun inline-table (offset)
  "Parser: The compact form of a table."
  (funcall (p:ap (lambda (kvs)
                   (let ((ht (make-hash-table :test #'equalp)))
                     (dolist (kv kvs)
                       (setf (gethash (car kv) ht) (cdr kv)))
                     ht))
                 (p:between (*> +table-open+ +skip-space+)
                            (p:sep (*> +comma+ +skip-space+) #'pair)
                            (*> +skip-space+ +table-close+)))
           offset))

#+nil
(p:parse #'inline-table "{ first = \"Tom\", last = \"Preston-Werner\" }")

(fn array (maybe list))
(defun array (offset)
  "Parser: A list of values."
  (funcall (p:between (*> +array-open+ +skip-space-and-comments+)
                      (p:sep-end (*> +comma+ +skip-space-and-comments+)
                                 (<* #'value +skip-junk+))
                      (*> +skip-space-and-comments+ +array-close+))
           offset))

#+nil
(p:parse #'array "[ [\"delta\", \"phi\"], [3.14] ]")

(fn table-array (maybe arrayed-table))
(defun table-array (offset)
  "Parser: An entry in an array-of-tables."
  (funcall (p:ap (lambda (name kvs)
                   (let ((ht (make-hash-table :test #'equalp)))
                     (dolist (pair kvs)
                       (setf (gethash (car pair) ht) (cdr pair)))
                     (make-arrayed-table :key name :kvs ht)))
                 (<* (p:between +ta-open+ #'key +ta-close+)
                     +skip-junk+)
                 (p:sep-end +skip-junk+ #'pair))
           offset))

#+nil
(p:parse #'table-array "[[fruits.varieties]]
name = \"plantain\"")

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
(fn value (maybe toml-value))
(defun value (offset)
  "Parser: The value portion of a key-value pair."
  (funcall (p:alt #'string #'date-time #'number #'boolean #'inline-table #'array)
           offset))

(fn date-time (maybe (pd::date-time)))
(defun date-time (offset)
  (funcall (p:alt #'pd:offset-date-time #'pd:local-date-time #'pd:local-date #'pd:local-time)
           offset))

(fn number (maybe (or fixnum double-float)))
(defun number (offset)
  "Parser: Any number."
  (funcall (p:alt #'float-or-int #'hex #'octal #'binary)
           offset))

(fn float-or-int (maybe (or fixnum double-float)))
(defun float-or-int (offset)
  (funcall
   (p:ap (lambda (sign init init-rest after exp)
           (let ((n (if (and (null after) (null exp))
                        ;; We have parsed an integer.
                        (if (null init-rest)
                            init
                            ;; FIXME: 2025-10-20 Consider how I might use
                            ;; bit-shifting to hydrate a final value instead of
                            ;; asking Lisp to reparse the number.
                            (read-from-string (format nil "~a~{~a~}" init init-rest)))
                        ;; We have parsed a float.
                        (let* ((*read-default-float-format* 'double-float)
                               (e (or exp ""))
                               (a (or after '(0)))
                               (s (format nil "~a~{~a~}.~{~a~}~a" init init-rest a e)))
                          (read-from-string s)))))
             (if (eq :neg sign) (- n) n)))
         (p:opt (p:alt (<$ :pos +plus+)
                       (<$ :neg +dash+)))
         #'p:unsigned
         (p:opt (*> +underscore+ +digit-chunks+))
         (p:opt (*> +period+ +digit-chunks+))
         (p:opt (p:recognize (*> (p:alt +small-e+ +big-e+)
                                 (p:opt (p:alt +plus+ +dash+))
                                 +digits1+))))
   offset))

#+nil
(p:parse #'float-or-int "10_0000000000_1")
#+nil
(p:parse #'float-or-int "-1.2e10")

(fn hex (maybe fixnum))
(defun hex (offset)
  "Parser: A positive hexadecimal number."
  (funcall (p:ap (lambda (ns) (read-from-string (format nil "#x~{~a~}" ns)))
                 (*> +hex-start+ (p:sep1 +underscore+ (p:take-while1 #'p:hex?))))
           offset))

#+nil
(hex (p:in "0xdead_beef"))

(fn octal (maybe fixnum))
(defun octal (offset)
  "Parser: A positive base-8 number."
  (funcall (p:ap (lambda (ns) (read-from-string (format nil "#o~{~a~}" ns)))
                 (*> +oct-start+ (p:sep1 +underscore+ (p:take-while1 #'p:octal?))))
           offset))

#+nil
(octal (p:in "0o01234567"))
#+nil
(octal (p:in "0o8"))

(fn binary (maybe fixnum))
(defun binary (offset)
  "Parser: A positive base-2 number."
  (funcall (p:ap (lambda (ns) (read-from-string (format nil "#b~{~a~}" ns)))
                 (*> +bin-start+ (p:sep1 +underscore+ (p:take-while1 #'p:binary?))))
           offset))

#+nil
(binary (p:in "0b1010"))

(fn boolean (maybe cl:boolean))
(defun boolean (offset)
  "Parser: True or false."
  (funcall (p:alt (<$ t   +true+)
                  (<$ nil +false+))
           offset))

#+nil
(boolean (p:in "true"))
