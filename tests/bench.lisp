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

#+nil
(let ((s (uiop:read-file-string "tests/data/large-file.json")))
  (sb-sprof:with-profiling (:max-samples 20000 :sample-interval 0.0001 :report :graph)
    (pj:parse s)
    t))

;; (1) Base: 6.5b bytes, 3.5s
;; (2) Failure cons cell: 6.0b bytes, 3.35s
;; (3) Avoid alloc in `string': 6.0b bytes, 3.35s (but it is faster in microbenches)
;; (4) Avoid `make-parser': 5.4b bytes, 2.55s
;; (5) Failing is just `:fail': 5.0b bytes, 2.5s
