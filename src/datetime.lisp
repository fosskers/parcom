;;; Dates and times.
;;;
;;; All dates are assumed to fall between 0 AD and 9999 AD.
;;;
;;; See also https://datatracker.ietf.org/doc/html/rfc3339

(defpackage parcom/datetime
  (:use :cl)
  (:shadow)
  (:import-from :parcom #:<*> #:<* #:*> #:<$)
  (:local-nicknames (#:p #:parcom))
  ;; --- Types --- ;;
  (:export #:local-date #:local-date-year #:local-date-month #:local-date-day)
  ;; --- Utilities --- ;;
  (:export #:leap-year?))

(in-package :parcom/datetime)

#+nil
(get-universal-time)

#+nil
(decode-universal-time (get-universal-time))

(defstruct local-date
  "A simple calendar date."
  (year  0 :type fixnum)
  (month 1 :type fixnum)
  (day   1 :type fixnum))

(defun local-date (input)
  "Parser: The YYYY-MM-DD portion."
  (let ((res (funcall (<*> #'p:unsigned
                           (*> (p:char #\-) (p:opt (p:char #\0)) #'p:unsigned)
                           (*> (p:char #\-) (p:opt (p:char #\0)) #'p:unsigned))
                      input)))
    (if (p:failure? res)
        res
        (destructuring-bind (year month day) (p:parser-value res)
          (cond ((not (<= 0 year 9999)) (p:fail "A year between 0 and 9999" (p:parser-input res)))
                ((not (<= 1 month 12)) (p:fail "A month between 1 and 12" (p:parser-input res)))
                ((not (<= 1 day (days-in-month-by-year year month)))
                 (p:fail (format nil "~d-~2,'0d does not have ~a days" year month day)
                     (p:parser-input res)))
                (t (p:ok (p:parser-input res)
                         (make-local-date :year year :month month :day day))))))))

#+nil
(local-date (p:in "1979-01-02"))

;; --- Utilities --- ;;

(defun leap-year? (year)
  "Must use a 4-digit year."
  (and (zerop (mod year 4))
       (or (not (zerop (mod year 100)))
           (zerop (mod year 400)))))

#+nil
(leap-year? 1600)

(defun days-in-month-by-year (year month)
  "The number of days in the given month for a particular year."
  (case month
    (1 31)
    (2 (if (leap-year? year) 29 28))
    (3 31)
    (4 30)
    (5 31)
    (6 30)
    (7 31)
    (8 31)
    (9 30)
    (10 31)
    (11 30)
    (12 31)))
