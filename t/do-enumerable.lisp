(in-package #:enumerable-tests)

(5am:def-suite do-enumerable
  :description "Tests the enumerable expressions with vectors."
  :in enumerable)

(5am:in-suite do-enumerable)

(defun test-fn-list ()
  (list 1 2 3))

(defun test-fn-vector ()
  (vector 1 2 3))

(defun test-fn-hash ()
  (plist-hash-table (list :a 1 :b 2 :c 3)))

(5am:test do-enumerable.list
  (5am:finishes
    (let ((elts ()))
      (do-enumerable (x (the list (test-fn-list)))
        (push x elts))
      (setf elts (nreverse elts))
      (5am:is (equal (test-fn-list) elts)))))

(5am:test do-enumerable.vector
  (5am:finishes
    (let ((elts ()))
      (do-enumerable (x (the vector (test-fn-vector)))
        (push x elts))
      (setf elts (copy-sequence 'vector (nreverse elts)))
      (5am:is (equalp (test-fn-vector) elts)))))

(5am:test do-enumerable.hash
  (5am:finishes
    (let ((kvps ()))
      (do-enumerable (kvp (the hash-table (test-fn-hash)))
        (push kvp kvps))
      (setf kvps (alist-hash-table kvps))
      (5am:is (equalp (test-fn-hash) kvps)))))

