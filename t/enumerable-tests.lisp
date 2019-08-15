(in-package #:enumerable-tests)

(5am:def-suite enumerable
  :description "Tests for the enumerable library.")

(5am:def-suite expressions
  :description "Tests the enumerable expressions."
  :in enumerable)

(5am:in-suite expressions)

(defun eequal (e1 e2)
  "As `equal', but forces arguments by `to-list'"
  (equal (to-list e1) (to-list e2)))

(defun eequalp (e1 e2)
  "As `equalp', but forces arguments by `to-list'"
  (equalp (to-list e1) (to-list e2)))

(defun eset-equal (e1 e2 &key (test #'eql) (key #'identity))
  "As `set-equal', but forces arguments by `to-list'"
  (set-equal (to-list e1) (to-list e2) :test test :key key))

(defun run-tests ()
  (5am:run! 'enumerable))

(defun main (&rest args)
  (declare (ignore args))
  (let ((result (run-tests)))
    (if result 0 -1)))
