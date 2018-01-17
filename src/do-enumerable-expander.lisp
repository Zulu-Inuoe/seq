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

  (defun %map-expander (type var enumerable result body env)
    (declare (ignore type env))
    `(block nil
       (map-enumerable (lambda (,var) ,@body) ,enumerable)
       (let ((,var nil))
         (declare (ignorable ,var))
         ,result)))

  (defun %loop-expander (type var enumerable result body env)
    (declare (ignore type env))
    (with-gensyms (enumerator-sym)
      `(loop
         :with ,enumerator-sym := (get-enumerator ,enumerable)
         :while (move-next ,enumerator-sym)
         :do (let ((,var (current ,enumerator-sym))) ,@body)
         :finally (return ,result))))

  (defun %typecase-map-expander (type var enumerable result body env)
    (with-gensyms (enumerable-sym)
      `(let ((,enumerable-sym ,enumerable))
         (typecase ,enumerable-sym
           ,@(mapcar
              (lambda (expander)
                (destructuring-bind (expander-type . expansion-fn)
                    expander
                  `(,expander-type
                    ,(funcall expansion-fn type var enumerable-sym result body env))))
              *%do-enumerable-expanders*)
           (t
            ,(%map-expander type var enumerable-sym result body env))))))

  (defun %typecase-loop-expander (type var enumerable result body env)
    (with-gensyms (enumerable-sym)
      `(let ((,enumerable-sym ,enumerable))
         (typecase ,enumerable-sym
           ,@(mapcar
              (lambda (expander)
                (destructuring-bind (expander-type . expansion-fn)
                    expander
                  `(,expander-type
                    ,(funcall expansion-fn type var enumerable-sym result body env))))
              *%do-enumerable-expanders*)
           (t
            ,(%loop-expander type var enumerable-sym result body env))))))

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

(defun enumerablep (x)
  (and
   (or (member x *%do-enumerable-expanders* :key #'car :test #'typep)
       (compute-applicable-methods #'get-enumerator (list x)))
   t))