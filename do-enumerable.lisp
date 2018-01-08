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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *%do-enumerable-expanders* ())

  (defun %default-expander (type var enumerable result body env)
    (declare (ignore type env))
    `(block nil
       (map-enumerable (lambda (,var) ,@body) ,enumerable)
       (let ((,var nil))
         (declare (ignorable ,var))
         ,result)))

  (defun %get-expander (decl-type)
    (cond
      ((null decl-type) #'%default-expander)
      (t
       (loop :for (type . expander) :in *%do-enumerable-expanders*
             :if (subtypep decl-type type)
               :return expander
             :finally
                (return #'%default-expander)))))

  (defun %set-expander (type expander)
    (cond
      (expander
       (let ((cell (assoc type *%do-enumerable-expanders*)))
         (unless cell
           (setf cell (cons type nil))
           (push cell *%do-enumerable-expanders*))
         (setf (cdr cell) expander)))
      (t
       (setf *%do-enumerable-expanders*
             (delete type *%do-enumerable-expanders* :test #'equal :key #'car)))))

  (defmacro define-do-enumerable-expander
      (type (iter-type iter-var iter-enum iter-res iter-body iter-env)
       &body body)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (%set-expander
        ',type
        (lambda (,iter-type ,iter-var ,iter-enum ,iter-res ,iter-body ,iter-env)
          (declare (ignorable ,iter-type ,iter-var ,iter-enum ,iter-res ,iter-body ,iter-env))
          ,@body))
       ',type)))

(defmacro do-enumerable ((var enumerable &optional result)
                         &body body
                         &environment env)
  (let* ((enumerable (macroexpand enumerable env))
         (decl-type
           (cond
             ((constantp enumerable) (type-of enumerable))
             ((symbolp enumerable)
              (multiple-value-bind (bind-type local decl-info)
                  (cltl2:variable-information enumerable env)
                (declare (ignore bind-type local))
                (cdr (assoc 'type decl-info))))
             ((listp enumerable)
              (multiple-value-bind (bind-type local decl-info)
                  (cltl2:function-information (car enumerable) env)
                (declare (ignore bind-type local))
                (when-let ((ftype (cdr (assoc 'ftype decl-info))))
                  (destructuring-bind (function params return-values)
                      ftype
                    (declare (ignore function params))
                    (cond
                      ((and (listp return-values)
                            (eq (car return-values) 'values))
                       (cadr return-values))
                      (t
                       return-values))))))
             (t nil))))
    (funcall (%get-expander decl-type)
             decl-type var enumerable result body env)))
