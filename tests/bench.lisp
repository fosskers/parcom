;;; For the large JSON file used in the benchmarks below, see the README.

(defpackage parcom/benchmarks
  (:use :cl)
  (:local-nicknames (#:pc #:parcom)
                    (#:pj #:parcom/json)
                    (#:px #:parcom/xml)
                    (#:pe #:parcom/email)
                    #-cmucl (#:jzon #:com.inuoe.jzon)))

(in-package :parcom/benchmarks)

#+sbcl
(require :sb-sprof)

(defun average (list)
  (/ (apply #'+ list) (float (length list))))

#+nil
(average '(0.508 0.440 0.619 0.491 0.431))

;; --- Integer Parsing --- ;;

#+nil
(time (dotimes (n 1000000)
        (pc:parse #'pc:integer "-1234567890")))
#+nil
(time (dotimes (n 1000000)
        (pc:parse #'pc::integer2 "-1234567890")))

#+nil
(progn
  (format t "--- NUMBER ---~%")
  (time (dotimes (n 1000000)
          (pc:parse #'pj::number "-1234567890")))
  (format t "--- NUMBER2 ---~%")
  (time (dotimes (n 1000000)
          (pc:parse #'pj::number2 "-1234567890")))
  (format t "--- SCIENTIFIC ---~%")
  (time (dotimes (n 1000000)
          (pc:parse #'pj::scientific "-1234567890"))))

;; RESULT: `number' is slightly faster than `number2' for integer inputs. Both
;; use about half as much memory as `scientific'.

#+nil
(progn
  (format t "--- NUMBER ---~%")
  (time (dotimes (n 1000000)
          (pc:parse #'pj::number "-123.456E10")))
  (format t "--- NUMBER2 ---~%")
  (time (dotimes (n 1000000)
          (pc:parse #'pj::number2 "-123.456E10")))
  (format t "--- SCIENTIFIC ---~%")
  (time (dotimes (n 1000000)
          (pc:parse #'pj::scientific "-123.456E10"))))

;; RESULT: `number2' (the highly optimized implementation) is 2x faster for
;; float inputs than both `number' and `scientific' and uses half as much
;; memory.

;; --- ABCL --- ;;

#+nil
(let ((s (uiop:read-file-string "tests/data/large-file.json"))) (parcom/json:parse s) t)

;; --- COMPARISONS --- ;;

(defun large-json ()
  (let* ((path #p"tests/data/large-file.json")
         (s (uiop:read-file-string path)))
    #-cmucl
    (progn
      (format t "--- JZON ---~%")
      (time (jzon:parse s))
      (time (jzon:parse s))
      (time (jzon:parse s))
      (time (jzon:parse s))
      (time (jzon:parse s)))
    (progn
      (format t "--- SHASHT ---~%")
      (time (shasht:read-json s))
      (time (shasht:read-json s))
      (time (shasht:read-json s))
      (time (shasht:read-json s))
      (time (shasht:read-json s)))
    (progn
      (format t "--- JSOWN ---~%")
      (time (jsown:parse s))
      (time (jsown:parse s))
      (time (jsown:parse s))
      (time (jsown:parse s))
      (time (jsown:parse s)))
    (progn
      (format t "--- YASON ---~%")
      (time (yason:parse s))
      (time (yason:parse s))
      (time (yason:parse s))
      (time (yason:parse s))
      (time (yason:parse s)))
    (progn
      (format t "--- PARCOM/JSON ---~%")
      (time (pj:parse s))
      (time (pj:parse s))
      (time (pj:parse s))
      (time (pj:parse s))
      (time (pj:parse s)))
    t))

#+nil
(large-json)

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
;; (26) Avoid generic `>=': 0.26b, 0.75s
;; (27) New baseline: 0.26b, 0.9s
;; (28) Static parsers: 0.26b, 0.50s

;; --- XML --- ;;

#+nil
(let ((s (uiop:read-file-string "tests/data/java.pom")))
  (time (px:parse s)))

#+nil
(let ((s (uiop:read-file-string "tests/data/java.pom")))
  #-cmucl
  (progn
    (format t "--- PLUMP ---~%")
    (time (dotimes (n 1000)
            (plump:parse s)))
    (time (dotimes (n 1000)
            (plump:parse s)))
    (time (dotimes (n 1000)
            (plump:parse s))))
  (format t "--- CXML ---~%")
  (time (dotimes (n 1000)
          (cxml:parse s (cxml-dom:make-dom-builder))))
  (time (dotimes (n 1000)
          (cxml:parse s (cxml-dom:make-dom-builder))))
  (time (dotimes (n 1000)
          (cxml:parse s (cxml-dom:make-dom-builder))))
  (format t "--- PARCOM/XML ---~%")
  (time (dotimes (n 1000)
          (px:parse s)))
  (time (dotimes (n 1000)
          (px:parse s)))
  (time (dotimes (n 1000)
          (px:parse s))))

;; MEMORY
#+nil
(let ((s (uiop:read-file-string "tests/data/java.pom")))
  (sb-sprof:with-profiling (:max-samples 100000 :sample-interval 0.00001 :report :graph :mode :alloc)
    (dotimes (n 2000)
      (px:parse s))))

;; SPEED
#+nil
(let ((s (uiop:read-file-string "tests/data/java.pom")))
  (sb-sprof:with-profiling (:max-samples 100000 :sample-interval 0.00001 :report :graph)
    (dotimes (n 2000)
      (px:parse s))))

;; Good initial news: I can already read 2000 of such complicated XML files in
;; 1s. Assuming ECL is 10x slower, I could do 200 files in 1s, which is likely
;; already far more than any ABCL-based project would need. So I can mostly just
;; optimize for memory, the reduced usage of which will speed me up for free.
;;
;; (0) Base: 1.5b bytes, 1.14s (no obvious speed hotspots)
;; (1) Cache on `string': 1.24b bytes, 1.43s
;; (2) `:id' on `between': 1.00b bytes, 1.44s
;; (3) Cache on `skip': 0.93b bytes, 1.45s
;; (4) `:id' on `consume': 0.84b bytes, 1.45s
;; (5) Cache on `take-until': 0.75b bytes, 1.48s
;; (6) `consume' over `take-while': 0.65b bytes, 1.40s
;; (7) Pre-saved global parsers: 0.61 bytes, 0.83s

;; --- EMAIL --- ;;

#+nil
(let ((email "alice.fun.party@bobs.house.com"))
  (time (dotimes (n 400000)
          (pe:parse email))))

;; MEMORY
#+nil
(let ((email "alice.fun.party@bobs.house.com"))
  (sb-sprof:with-profiling (:max-samples 100000 :sample-interval 0.00001 :report :graph :mode :alloc)
    (dotimes (n 400000)
      (pe:parse email))))

;; TIME
#+nil
(let ((email "alice.fun.party@bobs.house.com"))
  (sb-sprof:with-profiling (:max-samples 100000 :sample-interval 0.00001 :report :graph)
    (dotimes (n 400000)
      (pe:parse email))))

;; (0) Base: 1.3b bytes, 966ms
;; (1) Static `any-if': 998m bytes, 855ms
;; (2) Static `consume': 895m bytes, 805ms
;; (3) `consume1': 716m bytes, 725ms
;; (4) `opt' sweep: 511m bytes, 680ms
;; (5) `between' allocation: 396m bytes, 645ms

;; TODO: 2025-09-11 Start here. Reduce allocations of `between' and `opt'.
