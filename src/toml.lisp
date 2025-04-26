(defpackage parcom/toml
  (:use :cl)
  (:shadow #:string)
  (:import-from :parcom #:<*> #:<* #:*> #:<$)
  (:local-nicknames (#:p #:parcom)))

(in-package :parcom/toml)

(defun toml (input)
  "Parser: Parse a TOML document into a Hash Table."
  (funcall (p:alt #'comment #'pair) input))

(defun comment (input)
  "Parser: Skip over any comment line."
  (funcall (*> (p:char #\#)
               (p:consume (lambda (c) (not (equal c #\newline)))))
           input))

#+nil
(comment (p:in "# yes"))

(defun string (input)
  "Parser: One of the four TOML string types."
  (funcall (p:alt #'basic-string
                  #'multiline-basic-string
                  #'literal-string
                  #'multiline-literal-string)
           input))

#+nil
(string (p:in "\"hello\""))

(defun pair (input)
  "Parser: A key-value pair."
  (funcall (p:pair #'key (*> #'skip-space
                             (p:char #\=)
                             #'skip-space
                             #'value))
           input))

(defun skip-space (input)
  "A faster variant of `space' that just advances over whitespace chars."
  (funcall (p:consume (lambda (c) (or (equal c #\space) (equal c #\tab))))
           input))

#+nil
(funcall #'skip-space (p:in "   abc"))

(defun key (input)
  "Parser: The key portion of a key-value pair."
  (funcall (p:alt #'bare-key #'quoted-key #'dotted-key) input))

(defun bare-key (input)
  "Parser: Just ASCII letters, digits, dashes, and underscores."
  (funcall (p:take-while1 (lambda (c)
                            (or (p:ascii-letter? c)
                                (p:digit? c)
                                (equal c #\-)
                                (equal c #\_))))
           input))

#+nil
(bare-key (p:in "alex-honnold"))
#+nil
(bare-key (p:in "123"))

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
(defun value (input)
  "Parser: The value portion of a key-value pair.")
