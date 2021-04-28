;; (delete-package '#:com.inuoe.doseq-tests)
(defpackage #:com.inuoe.doseq-tests
  (:use #:cl)
  (:import-from
   #:alexandria
   #:copy-sequence
   #:nreversef
   #:alist-hash-table
   #:plist-hash-table)
  (:import-from
   #:com.inuoe.doseq
   #:doseq)
  (:import-from
   #:fiveam
   #:def-suite
   #:in-suite
   #:test
   #:is
   #:finishes)
  (:local-nicknames
   (#:seq #:com.inuoe.seq))
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

(defun test-fn-lazy-seq ()
  (seq:lazy-seq (cons 1 (seq:lazy-seq (cons 2 (seq:lazy-seq (cons 3 nil)))))))

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

(test doseq.lazy-seq
  (finishes
    (let ((values ()))
      (doseq (x (the seq:lazy-seq (test-fn-lazy-seq)))
        (push x values))
      (setf values (nreverse values))
      (is (equalp '(1 2 3) values)))))

(test doseq.list-var-with-index
  (finishes
    (let ((values.indices ()))
      (doseq ((x i) '(1 2 3))
        (push (cons x i) values.indices))
      (setf values.indices (nreverse values.indices))
      (is (equalp '((1 . 0) (2 . 1) (3 . 2)) values.indices)))))

(test doseq.vector-var-with-index
  (finishes
    (let ((values.indices ()))
      (doseq ((x i) #(1 2 3))
        (push (cons x i) values.indices))
      (setf values.indices (nreverse values.indices))
      (is (equalp '((1 . 0) (2 . 1) (3 . 2)) values.indices)))))

(test doseq.hash-var-with-index
  (finishes
    (let ((values.indices ()))
      (doseq ((kvp i) (the hash-table (plist-hash-table '(a 1 b 2 c 3))))
        (push (cons (cdr kvp) i) values.indices))
      (setf values.indices (nreverse values.indices))
      (is (equalp '((1 . 0) (2 . 1) (3 . 2)) values.indices)))))

(test doseq.hash-var-with-index-no-infer
  (finishes
    (let ((values.indices ()))
      (doseq ((kvp i) (the t (plist-hash-table '(a 1 b 2 c 3))))
        (push (cons (cdr kvp) i) values.indices))
      (setf values.indices (nreverse values.indices))
      (is (equalp '((1 . 0) (2 . 1) (3 . 2)) values.indices)))))
