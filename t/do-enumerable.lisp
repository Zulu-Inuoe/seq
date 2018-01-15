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

(5am:def-suite do-enumerable
  :description "Tests the enumerable expressions with vectors."
  :in enumerable)

(5am:in-suite do-enumerable)

(defun test-fn ()
  (list 1 2 3))

(defun test-undefined-fn ()
  (do-enumerable (x (test-fn))
    (declare (ignore x))))

(5am:test undefined-fn
  (5am:finishes
    (do-enumerable (x (test-fn))
      (declare (ignore x)))))
