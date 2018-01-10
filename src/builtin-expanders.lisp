;;;enumerable - enumerable implementation for CL, using cl-cont
;;;Written in 2018 by Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>
;;;
;;;To the extent possible under law, the author(s) have dedicated all copyright
;;;and related and neighboring rights to this software to the public domain
;;;worldwide. This software is distributed without any warranty.
;;;You should have received a copy of the CC0 Public Domain Dedication along
;;;with this software. If not, see
;;;<http://creativecommons.org/publicdomain/zero/1.0/>.

(in-package #:enumerable)

(define-do-enumerable-expander list
    (type var enumerable result body env)
  `(dolist (,var ,enumerable ,result)
     ,@body))

(define-do-enumerable-expander vector
    (type var enumerable result body env)
  (with-gensyms (vec i)
    `(let ((,vec ,enumerable))
       (dotimes (,i (length ,vec) (let (,var) (declare (ignorable ,var)) ,result))
         (let ((,var (aref ,vec ,i)))
           ,@body)))))

(define-do-enumerable-expander hash-table
    (type var enumerable result body env)
  (with-gensyms (iter more? key value)
    `(with-hash-table-iterator (,iter ,enumerable)
         (loop
           (multiple-value-bind (,more? ,key ,value)
               (,iter)
             (unless ,more?
               (return (let ((,var)) (declare (ignorable ,var)) ,result)))
             (let ((,var (cons ,key ,value)))
               ,@body))))))
