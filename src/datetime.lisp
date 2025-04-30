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
  (:export #:local-date #:local-date-year #:local-date-month #:local-date-day
           #:local-time #:local-time-hour #:local-time-minute #:local-time-second #:local-time-millis)
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
  (let ((res (funcall (<*> (*> (p:consume (lambda (c) (equal c #\0))) #'p:unsigned)
                           (*> (p:char #\-) (p:opt (p:char #\0)) #'p:unsigned)
                           (*> (p:char #\-) (p:opt (p:char #\0)) #'p:unsigned))
                      input)))
    (if (p:failure? res)
        res
        (destructuring-bind (year month day) (p:parser-value res)
          (cond ((not (<= 0 year 9999)) (p:fail "A year between 0 and 9999" input))
                ((not (<= 1 month 12)) (p:fail "A month between 1 and 12" input))
                ((not (<= 1 day (days-in-month-by-year year month)))
                 (p:fail (format nil "~d-~2,'0d does not have ~a days" year month day) input))
                (t (p:ok (p:parser-input res)
                         (make-local-date :year year :month month :day day))))))))

#+nil
(local-date (p:in "1979-01-02"))

(defstruct local-time
  "A time without any timezone considerations."
  (hour   0 :type fixnum)
  (minute 0 :type fixnum)
  (second 0 :type fixnum)
  (millis 0 :type fixnum))

(defun local-time (input)
  "Parser: A time in the format HH:MM:SS.XXX to millisecond precision. If
additional factional seconds are present, the value will be truncated. Parsing
of a leap second is generally permitted, since the year/month/day cannot be
known here."
  (let ((res (funcall (<*> (*> (p:opt (p:char #\0)) #'p:unsigned)
                           (*> (p:char #\:) (p:opt (p:char #\0)) #'p:unsigned)
                           (*> (p:char #\:) (p:opt (p:char #\0)) #'p:unsigned)
                           (p:opt (*> (p:char #\.) (p:many1 (p:any-if #'p:digit?)))))
                      input)))
    (if (p:failure? res)
        res
        (destructuring-bind (h m s millis) (p:parser-value res)
          (cond ((not (<= 0 h 23)) (p:fail "An hour between 0 and 23" input))
                ((not (<= 0 m 59)) (p:fail "A minute between 0 and 59" input))
                ((or (and (= h 23) (= m 59) (> s 60))
                     (and (not (= h 23))
                          (not (= m 59))
                          (not (<= 0 s 59))))
                 (p:fail "A legal second value" input))
                (t (p:ok (p:parser-input res)
                         (make-local-time :hour h :minute m :second s
                                          :millis (let ((a (nth 0 millis))
                                                        (b (nth 1 millis))
                                                        (c (nth 2 millis)))
                                                    (+ (if (null a) 0 (* 100 (digit-char-p a)))
                                                       (if (null b) 0 (* 10 (digit-char-p b)))
                                                       (if (null c) 0 (digit-char-p c))))))))))))

#+nil
(local-time (p:in "00:32:00.123"))
#+nil
(local-time (p:in "23:59:60"))

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

;; https://www.nist.gov/pml/time-and-frequency-division/time-realization/leap-seconds
(defun hour-needs-leap-second? (year month day)
  ""
  ())
