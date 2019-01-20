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
    (multiple-value-bind (body decls)
        (parse-body body)
      `(let ((,vec ,enumerable))
         (dotimes (,i (length ,vec) (let (,var) ,var ,result))
           (let ((,var (aref ,vec ,i)))
             ,@decls
             (tagbody ,@body)))))))

(define-do-enumerable-expander hash-table
    (type var enumerable result body env)
  (with-gensyms (iter more? key value)
    (multiple-value-bind (body decls)
        (parse-body body)
      `(with-hash-table-iterator (,iter ,enumerable)
         (loop
           (multiple-value-bind (,more? ,key ,value)
               (,iter)
             (unless ,more?
               (return (let (,var) ,var ,result)))
             (let ((,var (cons ,key ,value)))
               ,@decls
               (tagbody ,@body))))))))
#+sbcl
(define-do-enumerable-expander sequence
    (type var enumerable result body env)
  `(sb-sequence:dosequence (,var ,enumerable ,result)
     ,@body))
