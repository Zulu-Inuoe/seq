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

(defmacro with-enumerable (&body body)
  "Run `body' in an enumerable context, where `yield' will yield a new value in
the enumeration, and `yield-break' will end the enumeration.
Example:
(to-list
  (with-enumerable
    (yield 1)
    (yield 2)
    (when (zerop (random 2))
      (yield-break))
    (yield 3)))
=>
(1 2)
OR
(1 2 3)

depending on the output of RANDOM."
  (with-gensyms (cont)
    `(make-instance
      'continuation-enumerable
      :starter
      (lambda ()
        (let (,cont)
          (lambda ()
            (if ,cont
                (funcall ,cont)
                (cl-cont:with-call/cc
                  (macrolet ((yield (result)
                               (with-gensyms (cc)
                                 `(cl-cont:let/cc ,cc
                                    (setf ,',cont ,cc)
                                    (values ,result t))))
                             (yield-break ()
                               (with-gensyms (cc)
                                 `(cl-cont:let/cc ,cc
                                    (declare (ignore ,cc))
                                    (setf ,',cont (lambda () (values nil nil)))
                                    (values nil nil))))
                             (do-enumerable ((var enumerable &optional result)
                                             &body body
                                             &environment env)
                               (%do-enumerable-expand #'%loop-expander var enumerable result body env)))
                    (progn ,@body)
                    (yield-break))))))))))

(defmacro lambdae (&whole whole args &body body)
  "Lambda who's body is enumerable."
  (multiple-value-bind (body decls doc-string)
      (parse-body body :documentation t :whole whole)
    `(lambda ,args
       ,@(if doc-string (list doc-string) ())
       ,@decls
       (with-enumerable ,@body))))

(defmacro defenumerable (&whole whole name args &body body)
  "Like defun, for a function who's body is enumerable."
  (multiple-value-bind (body decls doc-string)
      (parse-body body :documentation t :whole whole)
    `(defun ,name ,args
       ,@(if doc-string (list doc-string) ())
       ,@decls
       (with-enumerable ,@body))))
