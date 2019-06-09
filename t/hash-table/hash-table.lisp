(in-package #:enumerable-tests.hash-table)

(5am:def-suite enumerable.hash-table
  :description "Tests for enumerable interface implementation for hash tables."
  :in enumerable)

(5am:in-suite enumerable.hash-table)

(5am:test hash.get-enumerator
  (5am:finishes
    (let* ((keys (list :a 0 :b 1 :c 2 :d 3))
           (enumerator (get-enumerator (alexandria:plist-hash-table keys))))
      (5am:is (move-next enumerator))
      (let ((kvp (current enumerator)))
        (5am:is (= (getf keys (car kvp)) (cdr kvp)))
        (5am:is (remf keys (car kvp))))
      (5am:is (move-next enumerator))
      (let ((kvp (current enumerator)))
        (5am:is (= (getf keys (car kvp)) (cdr kvp)))
        (5am:is (remf keys (car kvp))))
      (5am:is (move-next enumerator))
      (let ((kvp (current enumerator)))
        (5am:is (= (getf keys (car kvp)) (cdr kvp)))
        (5am:is (remf keys (car kvp))))
      (5am:is (move-next enumerator))
      (let ((kvp (current enumerator)))
        (5am:is (= (getf keys (car kvp)) (cdr kvp)))
        (5am:is (remf keys (car kvp))))
      (5am:is-false (move-next enumerator)))
    (let ((enumerator (get-enumerator (make-hash-table))))
      (5am:is-false (move-next enumerator))
      (5am:is-false (move-next enumerator)))))
