;;;enumerable-tests - tests for the enumerable library
;;;Written in 2018 by Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>
;;;
;;;To the extent possible under law, the author(s) have dedicated all copyright
;;;and related and neighboring rights to this software to the public domain
;;;worldwide. This software is distributed without any warranty.
;;;You should have received a copy of the CC0 Public Domain Dedication along
;;;with this software. If not, see
;;;<http://creativecommons.org/publicdomain/zero/1.0/>.

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
