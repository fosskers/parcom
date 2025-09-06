;;; RFC5322-compliant email parsing.
;;;
;;; For clarity, the name of each parser in this package is lifted directly from
;;; the spec.
;;;
;;; See also the following relevant sections in various RFCs.
;;;
;;; - https://datatracker.ietf.org/doc/html/rfc5322#section-3.4.1
;;; - https://datatracker.ietf.org/doc/html/rfc5322#section-3.6.4
;;; - https://datatracker.ietf.org/doc/html/rfc5321#section-4.1.2
;;; - https://datatracker.ietf.org/doc/html/rfc5234#appendix-B.1

(defpackage parcom/email
  (:use :cl)
  (:import-from :parcom #:<*> #:<* #:*> #:<$)
  (:local-nicknames (#:p #:parcom)))

(in-package :parcom/email)

;; --- Static Parsers --- ;;

(defparameter +@+ (p:char #\@))
(defparameter +bracket-open+ (p:char #\[))
(defparameter +bracket-close+ (p:char #\]))
(defparameter +period+ (p:char #\.))

;; --- Parsers --- ;;

(defun addr-spec (offset)
  (funcall (<*> local-part (*> +@+ domain)) offset))

(defun local-part (offset)
  (funcall (p:alt #'dot-atom #'quoted-string #'obs-local-part) offset))

(defun domain (offset)
  (funcall (p:alt #'dot-atom #'domain-literal #'obs-domain) offset))

;; Whitespace: https://datatracker.ietf.org/doc/html/rfc5322#section-3.2.2

;; TODO: 2025-09-05 Start here. Provide both the simpler `msg-id' parser and the
;; full `address' one from above. If you can prove the former works, then the
;; latter should be easier.

(defun msg-id (offset)
  (funcall (<*> #'id-left (*> +@+ #'id-right)) offset))

(defun id-left (offset)
  (funcall (p:alt #'dot-atom-text #'obs-id-left) offset))

(defun id-right (offset)
  (funcall (p:alt #'dot-atom-text #'no-fold-literal #'obs-id-right) offset))

(defun no-fold-literal (offset)
  (funcall (p:recognize (*> +bracket-open+ #'dtext +bracket-close+)) offset))

(defun dtext (offset)
  ())

(declaim (ftype (function (character) boolean) atext?))
(defun atext? (c)
  "Printable US-ASCII characters not including specials."
  (or (p:ascii-letter? c)
      (p:digit? c)
      (char= c #\!)
      (char= c #\#)
      (char= c #\$)
      (char= c #\%)
      (char= c #\&)
      (char= c #\')
      (char= c #\*)
      (char= c #\+)
      (char= c #\-)
      (char= c #\/)
      (char= c #\=)
      (char= c #\?)
      (char= c #\^)
      (char= c #\_)
      (char= c #\`)
      (char= c #\{)
      (char= c #\|)
      (char= c #\})
      (char= c #\~)))

(defun dot-atom-text (offset)
  "Parser: Simple dot-separated ascii atoms."
  (funcall (p:recognize (p:sep1 +period+ (p:take-while1 #'atext?))) offset))

#+nil
(p:parse #'dot-atom-text "foo.bar.baz")

;; dtext           =   %d33-90 /          ; Printable US-ASCII
;;                     %d94-126 /         ;  characters not including
;;                     obs-dtext          ;  "[", "]", or "\"

;; obs-dtext       =   obs-NO-WS-CTL / quoted-pair)

;; quoted-pair     =   ("\" (VCHAR / WSP)) / obs-qp

;; VCHAR          =  %x21-7E)
;;                        ; visible (printing) characters

;; WSP            =  SP / HTAB)
;;                        ; white space

;; SP             =  %x20)

;; HTAB           =  %x09))
;;                        ; horizontal tab

;; obs-qp          =   \ (%d0 / obs-NO-WS-CTL / LF / CR)

;; LF             =  %x0A
;;                        ; linefeed

;; CR             =  %x0D
;;                        ; carriage return

;; obs-NO-WS-CTL   =   %d1-8 /            ; US-ASCII control
;;                     %d11 /             ;  characters that do not
;;                     %d12 /             ;  include the carriage
;;                     %d14-31 /          ;  return, line feed, and
;;                     %d127              ;  white space characters
