;;; For the large JSON file used in the benchmarks below, see the README.

(defpackage parcom/benchmarks
  (:use :cl)
  (:local-nicknames (#:pc #:parcom)
                    (#:pj #:parcom/json)
                    (#:jzon #:com.inuoe.jzon)))

(in-package :parcom/benchmarks)

#+sbcl
(require :sb-sprof)

;; --- COMPARISONS --- ;;

#+nil
(let* ((path #p"tests/data/large-file.json")
       (s (uiop:read-file-string path)))
  (format t "--- JZON ---~%")
  (time (jzon:parse s))
  (time (jzon:parse path))
  (format t "--- SHASHT ---~%")
  (time (shasht:read-json s))
  (format t "--- JSOWN ---~%")
  (time (jsown:parse s))
  (format t "--- YASON ---~%")
  (time (yason:parse s))
  (format t "--- PARCOM/JZON ---~%")
  (time (pj:parse s))
  t)

(defparameter +json-string+ "{\"key1\": \"value\\n\",
\"key2\":1,\"key3\":[\"Hello \\u2604\",  1.2e-34 ,true,
  false,null]}")

#+nil
(progn
  (format t "--- JZON ---~%")
  (time (dotimes (n 10000)
          (jzon:parse +json-string+)))
  (format t "--- SHASHT ---~%")
  (time (dotimes (n 10000)
          (shasht:read-json +json-string+)))
  (format t "--- JSOWN ---~%")
  (time (dotimes (n 10000)
          (jsown:parse +json-string+)))
  (format t "--- PARCOM/JZON ---~%")
  (time (dotimes (n 10000)
          (pj:parse +json-string+))))

;; --- HOT SPOT DETECTION --- ;;

;; MEMORY
#+nil
(let ((s (uiop:read-file-string "tests/data/large-file.json")))
  (sb-sprof:with-profiling (:max-samples 100000 :sample-interval 0.00001 :report :graph :mode :alloc)
    (aref (pj:parse s) 0)))

;; SPEED
#+nil
(let ((s (uiop:read-file-string "tests/data/large-file.json")))
  (sb-sprof:with-profiling (:max-samples 100000 :sample-interval 0.00001 :report :graph)
    (aref (pj:parse s) 0)))

;; (1) Base: 6.5b bytes, 3.5s
;; (2) Failure cons cell: 6.0b bytes, 3.35s
;; (3) Avoid alloc in `string': 6.0b bytes, 3.35s (but it is faster in microbenches)
;; (4) Avoid `make-parser': 5.4b bytes, 2.55s
;; (5) Failing is just `:fail': 5.0b bytes, 2.5s
;; (6) Avoiding minor allocs: 4.9b bytes, 2.5s
;; (7) Avoid recursion in `many': 4.87b bytes, 2.4s?
;; (8) Cache on `char': 3.95b bytes, 2.4s
;; (9) Cache on `any-but': 3.2b bytes, 2.7s
;; (10) Add `sneak': 2.48b bytes, 2.8s
;; (11) `fmap' mutates: 2.39b bytes, 2.7s
;; (12) Avoid `make-input': 1.96b bytes, 2.65s
;; (13) Avoid fmap/const in `<*': 1.91b bytes, 2.65s
;; (14) `multiple-value-bind': 1.35b bytes, 3.3s
;; (15) Yield offset directly: 0.92b bytes, 3.0s
;; (16) Type signatures for JSON: 0.92b bytes, 2.7s
;; (17) Avoid `many' in JSON strings: 0.69b bytes, 1.05s
;; (18) Avoid `take-while': 0.62b bytes, 0.95s
;; (19) Avoid `pair': 0.59b bytes, 0.90s
;; (20) Cache on `consume': 0.41b bytes, 0.87s
;; (21) Cache on `between': 0.29b bytes, 0.90s
;; (22) Cache on `sep': 0.26b bytes, 0.87s
;; (23) `(simple-array character (*))': 0.26b, 0.84s
;; (24) Detect when escaping isn't necessary: 0.26b, 0.8xs
;; (25) Use lower-level string copying: 0.26b, 0.8xs (nice speed up on ECL and ABCL)
