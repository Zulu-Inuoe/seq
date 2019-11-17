(in-package #:enumerable)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *%do-enumerable-expanders* ())

  (defun %map-expander (type var enumerable result body env)
    (declare (ignore type env))
    (multiple-value-bind (body decl)
        (parse-body body)
      `(block nil
         (map-enumerable (lambda (,var) ,@decl (tagbody ,@body)) ,enumerable)
         ,(when result `(let (,var) ,var ,result)))))

  (defun %subtype< (t1 t2)
    "Returns t if t1 is definitely a subtype of t2,
and t2 is not a subtype of t1."
    (and (subtypep t1 t2)
         (not (subtypep t2 t1))))

  (defun %get-expander (decl-type)
    (cond
      ((null decl-type)
       nil)
      (t
       (loop :for (type . expander) :in *%do-enumerable-expanders*
             :if (subtypep decl-type type)
               :return expander
             :finally
                (return nil)))))

  (defun %set-expander (type expander)
    (cond
      (expander
       (let ((cell (assoc type *%do-enumerable-expanders* :test #'type=)))
         (unless cell
           (setf cell (cons type nil))
           (push cell *%do-enumerable-expanders*))
         (setf (cdr cell) expander)))
      (t
       (setf *%do-enumerable-expanders*
             (delete type *%do-enumerable-expanders* :test #'type= :key #'car))))
    (setf *%do-enumerable-expanders* (stable-sort *%do-enumerable-expanders* #'%subtype< :key #'car)))

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

