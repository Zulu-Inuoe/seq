(defpackage #:com.inuoe.doseq-tests
  (:use
   #:alexandria
   #:cl
   #:com.inuoe.doseq)
  (:import-from
   #:fiveam
   #:def-suite
   #:in-suite
   #:test
   #:is
   #:finishes)
  (:export
   ;;; Test suites
   #:doseq

   #:run
   #:main))

(in-package #:com.inuoe.doseq-tests)

(def-suite doseq
  :description "Tests the doseq macro.")

(defun run ()
  (fiveam:run! 'doseq))

(defun main (&rest args)
  (declare (ignore args))
  (let ((result (run)))
    (if result 0 -1)))

(in-suite doseq)

(defun test-fn-list ()
  (list 1 2 3))

(defun test-fn-vector ()
  (vector 1 2 3))

(defun test-fn-hash ()
  (plist-hash-table (list :a 1 :b 2 :c 3)))

(test doseq.list
  (finishes
    (let ((elts ()))
      (doseq (x (the list (test-fn-list)))
        (push x elts))
      (setf elts (nreverse elts))
      (is (equal (test-fn-list) elts)))))

(test doseq.vector
  (finishes
    (let ((elts ()))
      (doseq (x (the vector (test-fn-vector)))
        (push x elts))
      (setf elts (copy-sequence 'vector (nreverse elts)))
      (is (equalp (test-fn-vector) elts)))))

(test doseq.hash
  (finishes
    (let ((kvps ()))
      (doseq (kvp (the hash-table (test-fn-hash)))
        (push kvp kvps))
      (setf kvps (alist-hash-table kvps))
      (is (equalp (test-fn-hash) kvps)))))
