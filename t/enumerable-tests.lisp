(in-package #:enumerable-tests)

(5am:def-suite enumerable
  :description "Tests for the enumerable library.")

(5am:def-suite expressions
  :description "Tests the enumerable expressions."
  :in enumerable)

(5am:in-suite expressions)

(defun main (&rest args)
  (declare (ignore args))
  (let ((result (5am:run! 'enumerable)))
    (if result 0 -1)))
