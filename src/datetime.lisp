;;; Dates and times.
;;;
;;; All dates are assumed to fall between 0 AD and 9999 AD.
;;;
;;; See also https://datatracker.ietf.org/doc/html/rfc3339

(defpackage parcom/datetime
  (:use :cl)
  (:shadow #:time #:format)
  (:import-from :parcom #:<* #:*> #:<$ #:fn #:-> #:maybe)
  (:local-nicknames (#:p #:parcom))
  ;; --- Entrypoints --- ;;
  (:export #:now #:parse)
  ;; --- Types --- ;;
  (:export #:local-date #:local-date-year #:local-date-month #:local-date-day
           #:local-time #:local-time-hour #:local-time-minute #:local-time-second #:local-time-millis
           #:local-date-time #:local-date-time-date #:local-date-time-time
           #:offset-date-time #:offset-date-time-date #:offset-date-time-time
           #:offset #:offset-hour #:offset-minute)
  ;; --- Other Parsers --- ;;
  (:export #:simple-local-time)
  ;; --- Generics --- ;;
  (:export #:date #:time #:format)
  ;; --- Utilities --- ;;
  (:export #:leap-year?))

(in-package :parcom/datetime)

;; --- Static Parsers --- ;;

(defparameter +T+      (p:char #\T))
(defparameter +Z+      (p:char #\Z))
(defparameter +dash+   (p:char #\-))
(defparameter +plus+   (p:char #\+))
(defparameter +colon+  (p:char #\:))
(defparameter +space+  (p:char #\space))
(defparameter +period+ (p:char #\.))
(defparameter +any-digit+ (p:any-if #'p:digit?))

;; --- Types --- ;;

(deftype date-time ()
  "Some date or time type."
  `(or offset-date-time local-date-time local-date local-time))

(defstruct local-date
  "A simple calendar date."
  (year  0 :type fixnum)
  (month 1 :type fixnum)
  (day   1 :type fixnum))

(defstruct local-time
  "A time without any timezone considerations."
  (hour   0 :type fixnum)
  (minute 0 :type fixnum)
  (second 0 :type fixnum)
  (millis 0 :type fixnum))

(defstruct local-date-time
  (date nil :type local-date)
  (time nil :type local-time))

(defstruct offset
  "A timezone offset from UTC."
  (hour   0 :type fixnum)
  (minute 0 :type fixnum))

(defstruct offset-date-time
  (date   nil :type local-date)
  (time   nil :type local-time)
  (offset nil :type offset))

;; --- Entry --- ;;

(fn parse (-> p::char-string date-time))
(defun parse (input)
  "Leniently parse some kind of date/time. It's up to the user to detect what they
actually received."
  (p:parse (p:alt #'offset-date-time #'local-date-time #'local-date #'local-time) input))

#+nil
(parse "1975-07-06")
#+nil
(parse "07:13:15")

(fn now (function () offset-date-time))
(defun now ()
  "A full offset date time representing the current moment."
  (multiple-value-bind (second minute hour day month year dow dst off)
      (decode-universal-time (get-universal-time))
    (declare (ignore dow dst))
    (make-offset-date-time
     :date (make-local-date :year year :month month :day day)
     :time (make-local-time :hour hour :minute minute :second second :millis 0)
     :offset (make-offset :hour (- off) :minute 0))))

#+nil
(now)

(fn local-date (maybe local-date))
(defun local-date (offset)
  "Parser: The YYYY-MM-DD portion."
  (funcall
   (p:ap (lambda (year month day)
           (cond ((not (<= 0 year 9999)) :fail)
                 ((not (<= 1 month 12)) :fail)
                 ((not (<= 1 day (days-in-month-by-year year month))) :fail)
                 (t (make-local-date :year year :month month :day day))))
         #'year
         (*> +dash+ #'2-digits)
         (*> +dash+ #'2-digits))
   offset))

#+nil
(format nil "~a" (p:parse #'local-date "1988-07-05"))
#+nil
(local-date (p:in "1979-01-02"))

(fn year (maybe fixnum))
(defun year (offset)
  "Parser: The YYYY year."
  (funcall (p:ap (lambda (a b c d)
                   (+ (* 1000 (digit-char-p a))
                      (*  100 (digit-char-p b))
                      (*   10 (digit-char-p c))
                      (digit-char-p d)))
                 +any-digit+
                 +any-digit+
                 +any-digit+
                 +any-digit+)
           offset))

(fn 2-digits (maybe fixnum))
(defun 2-digits (offset)
  "Parser: A two-digit unsigned number that might start with 0."
  (funcall (p:ap (lambda (a b)
                   (+ (* 10 (digit-char-p a))
                      (digit-char-p b)))
                 +any-digit+
                 +any-digit+)
           offset))

#+nil
(2-digits (p:in "07"))

(fn simple-local-time (maybe local-time))
(defun simple-local-time (offset)
  "Parser: Like `local-time', but only parses HH:MM(:SS). Seconds are optional
here, unlike the usual spec requirement."
  (funcall (p:ap (lambda (h m s)
                   (let ((s (or s 0)))
                     (cond ((not (<= 0 h 23)) :fail)
                           ((not (<= 0 m 59)) :fail)
                           ((or (and (= h 23) (= m 59) (> s 60))
                                (and (not (= h 23))
                                     (not (= m 59))
                                     (not (<= 0 s 59))))
                            :fail)
                           (t (make-local-time :hour h :minute m :second s :millis 0)))))
                 #'2-digits
                 (*> +colon+ #'2-digits)
                 (p:opt (*> +colon+ #'2-digits)))
           offset))

#+nil
(p:parse #'simple-local-time "13:00")

(fn local-time (maybe local-time))
(defun local-time (offset)
  "Parser: A time in the format HH:MM:SS.XXX to millisecond precision. If
additional factional seconds are present, the value will be truncated. Parsing
of a leap second is generally permitted, since the year/month/day cannot be
known here."
  (funcall (p:ap (lambda (h m s millis)
                   (cond ((not (<= 0 h 23)) :fail)
                         ((not (<= 0 m 59)) :fail)
                         ((or (and (= h 23) (= m 59) (> s 60))
                              (and (not (= h 23))
                                   (not (= m 59))
                                   (not (<= 0 s 59))))
                          :fail)
                         (t (make-local-time
                             :hour h
                             :minute m
                             :second s
                             :millis (let ((a (nth 0 millis))
                                           (b (nth 1 millis))
                                           (c (nth 2 millis)))
                                       (+ (if (null a) 0 (* 100 (digit-char-p a)))
                                          (if (null b) 0 (* 10 (digit-char-p b)))
                                          (if (null c) 0 (digit-char-p c))))))))
                 #'2-digits
                 (*> +colon+ #'2-digits)
                 (*> +colon+ #'2-digits)
                 (p:opt (*> +period+ (p:many1 (p:any-if #'p:digit?)))))
           offset))

#+nil
(local-time (p:in "00:32:00.123"))
#+nil
(local-time (p:in "23:59:60"))

(fn local-date-time (maybe local-date-time))
(defun local-date-time (offset)
  "Parser: A time and date with no indicated timezone offset."
  (funcall (p:ap (lambda (date time)
                   (make-local-date-time :date date :time time))
                 #'local-date
                 (*> (p:alt +T+ +space+)
                     #'local-time))
           offset))

#+nil
(local-date-time (p:in "1979-05-27T07:32:00"))

(fn offset (maybe offset))
(defun offset (offset)
  "Parser: A timezone offset."
  (funcall (p:alt #'greenwich
                  (p:ap (lambda (sign hours mins)
                          (make-offset :hour (if (equal #\- sign) (- hours) hours)
                                       :minute mins))
                        (p:alt +plus+ +dash+)
                        #'2-digits
                        (*> +colon+ #'2-digits)))
           offset))

#+nil
(offset (p:in "Z"))
#+nil
(offset (p:in "-07:00"))

(fn greenwich (maybe offset))
(defun greenwich (offset)
  (funcall (p:ap (lambda (off)
                   (declare (ignore off))
                   (make-offset :hour 0 :minute 0))
                 +Z+)
           offset))

(fn offset-date-time (maybe offset-date-time))
(defun offset-date-time (offset)
  "Parser: A time and date with some timezone offset, or Z to indicate UTC."
  (funcall (p:ap (lambda (ldt off)
                   (make-offset-date-time
                    :date (local-date-time-date ldt)
                    :time (local-date-time-time ldt)
                    :offset off))
                 #'local-date-time
                 #'offset)
           offset))

#+nil
(offset-date-time (p:in "1979-05-27T07:32:00Z"))

#+nil
(offset-date-time (p:in "1979-05-27T00:32:00-07:00"))

;; --- Generics --- ;;

(defgeneric date (x)
  (:documentation "The `date' slot of some type."))

(defmethod date ((x offset-date-time))
  (offset-date-time-date x))

(defmethod date ((x local-date-time))
  (local-date-time-date x))

(defmethod date ((x local-date))
  x)

(defgeneric time (x)
  (:documentation "The `time' slot of some type, without any timezone information."))

(defmethod time ((x offset-date-time))
  (offset-date-time-time x))

(defmethod time ((x local-date-time))
  (local-date-time-time x))

(defmethod time ((x local-time))
  x)

(defgeneric format (stream obj)
  (:documentation "Pretty-print a date/time object."))

(defmethod format (stream (obj local-date))
  (cl:format stream "~4,'0d-~2,'0d-~2,'0d"
             (local-date-year obj)
             (local-date-month obj)
             (local-date-day obj)))

#+nil
(format nil (p:parse #'local-date "1988-07-05"))

(defmethod format (stream (obj local-time))
  (cl:format stream "~2,'0d:~2,'0d:~2,'0d.~3,'0d"
             (local-time-hour obj)
             (local-time-minute obj)
             (local-time-second obj)
             (local-time-millis obj)))

#+nil
(format nil (p:parse #'local-time "06:59:04"))

(defmethod format (stream (obj local-date-time))
  (cl:format stream "~aT~a"
             (format stream (local-date-time-date obj))
             (format stream (local-date-time-time obj))))

#+nil
(format nil (p:parse #'local-date-time "2025-05-02T06:59:04"))

(defmethod format (stream (obj offset-date-time))
  (let ((hour (offset-hour (offset-date-time-offset obj))))
    (cl:format stream "~aT~a~a~2,'0d:~2,'0d"
               (format stream (offset-date-time-date obj))
               (format stream (offset-date-time-time obj))
               (if (< hour 0) #\- #\+)
               hour
               (offset-minute (offset-date-time-offset obj)))))

#+nil
(format nil (p:parse #'offset-date-time "2025-05-02T06:59:04Z"))

;; --- Utilities --- ;;

(fn leap-year? (-> fixnum boolean))
(defun leap-year? (year)
  "Must use a 4-digit year."
  (and (zerop (mod year 4))
       (or (not (zerop (mod year 100)))
           (zerop (mod year 400)))))

#+nil
(leap-year? 1600)

(fn days-in-month-by-year (-> fixnum fixnum fixnum))
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
