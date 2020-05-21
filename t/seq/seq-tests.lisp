(defpackage #:com.inuoe.seq-tests
  (:use
   #:cl
   #:com.inuoe.seq)
  (:import-from
   #:alexandria
   #:plist-hash-table)
  (:import-from
   #:fiveam
   #:def-suite
   #:finishes
   #:in-suite
   #:is
   #:signals
   #:test)
  (:export
   ;;; Test suites for specializing the 'expressions'
   #:seq
   ;;; Programmatic run-tests
   #:run
   #:main))

(in-package #:com.inuoe.seq-tests)

(def-suite seq
  :description "Tests for the seq library.")

(defun run ()
  (fiveam:run! 'seq))

(defun main (&rest args)
  (declare (ignore args))
  (let ((result (run)))
    (if result 0 -1)))

(in-suite seq)

(test col-seq-list-returns-self
  (is (equal '(1 2 3) (col-seq '(1 2 3)))))

(test seq-first-list-returns-car
  (is (= 1 (seq-first '(1 2 3)))))

(test seq-first-list-returns-cdr
  (is (equal '(2 3) (seq-rest '(1 2 3)))))

(test col-seq-vector-returns-self
  (is (equalp #(1 2 3) (col-seq #(1 2 3)))))

(test col-seq-empty-vector-returns-nil
  (is (null (col-seq #()))))

(test seq-first-vector-returns-first-elt
  (is (= 1 (seq-first #(1 2 3)))))

(test seq-first-on-rest-rest-vector-returns-second-elt
  (is (= 2 (seq-first (col-seq (seq-rest #(1 2 3)))))))

(test can-traverse-vector-elts
  (let ((seq (col-seq #(1 2 3))))
    (is (= 1 (seq-first seq)))
    (setf seq (col-seq (seq-rest seq)))
    (is (= 2 (seq-first seq)))
    (setf seq (col-seq (seq-rest seq)))
    (is (= 3 (seq-first seq)))
    (setf seq (col-seq (seq-rest seq)))
    (is (null seq))))

(test col-seq-empty-hash-returns-nil
  (is (null (col-seq (make-hash-table)))))

(test col-seq-hash-returns-something
  (is (not (null (col-seq (plist-hash-table '(:a 0)))))))

(test seq-first-hash-returns-kvp-cons
  (is (equal '(:a . 0) (seq-first (plist-hash-table '(:a 0))))))

(test seq-rest-single-hash-returns-nil
  (is (null (seq-rest (plist-hash-table '(:a 0))))))

(test seq-rest-multi-hash-returns-something
  (is (not (null (seq-rest (plist-hash-table '(:a 0 :b 1)))))))

(test lazy-seq-is-lazy
  (finishes (lazy-seq (error "whoops"))))

(test lazy-seq-col-seq-forces-value
  (signals error
    (col-seq (lazy-seq (error "whoops")))))

(test lazy-seq-seq-first-gets-value
  (is (= 1 (seq-first (lazy-seq (cons 1 nil))))))

(test lazy-seq-seq-first-gets-value
  (is (= 1 (seq-first (lazy-seq (list 1))))))

(test lazy-seq-caches-value
  (let* ((i 0)
         (seq (lazy-seq (list (incf i)))))
    (col-seq seq)
    (is (= 1 (seq-first (col-seq seq))))))
