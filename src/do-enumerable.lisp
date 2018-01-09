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
    (with-gensyms (enum-sym)
      `(let ((,enum-sym ,enumerable))
         (typecase ,enum-sym
           ,@(mapcar
              (lambda (expander)
                (destructuring-bind (expander-type . expansion-fn)
                    expander
                  `(,expander-type
                    ,(funcall expansion-fn type var enum-sym result body env))))
              *%do-enumerable-expanders*)
           (t
            (block nil
              (map-enumerable (lambda (,var) ,@body) ,enum-sym)
              (let ((,var nil))
                (declare (ignorable ,var))
                ,result)))))))

  (defun %subtype< (t1 t2)
    "Returns t if t1 is definitely a subtype of t2,
and t2 is not a subtype of t1."
    (and (subtypep t1 t2)
         (not (subtypep t2 t1))))

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %derived-fun-type (function)
    #+sbcl
    (sb-impl::%fun-type function))

  (defun %expression-type (exp env)
    (setf exp (macroexpand exp env))
    (cond
      ((constantp exp)
       ;;Constant expression, just get its type
       (type-of exp))
      ((symbolp exp)
       (multiple-value-bind (bind-type local decl-info)
           (cltl2:variable-information exp env)
         (declare (ignore bind-type local))
         ;;Try and find its declared type
         (cdr (assoc 'type decl-info))))
      ((listp exp)
       (destructuring-bind (fn &rest args) exp
         (declare (ignore args))
         (cond
           ((symbolp fn)
            ;;fn
            (multiple-value-bind (bind-type local decl-info)
                (cltl2:function-information fn env)
              (case bind-type
                (nil)
                (:function
                 (when-let ((ftype (cond
                                     (local
                                      ;;If it's local, try and get decl info
                                      (cdr (assoc 'ftype decl-info)))
                                     (t
                                      ;;Otherwise, try and get the decl info or the derived
                                      (or (cdr (assoc 'ftype decl-info))
                                          (%derived-fun-type (symbol-function fn)))))))
                   ;;ftype definition is
                   (destructuring-bind (function params return-values)
                       ftype
                     (declare (ignore function params))
                     (cond
                       ((listp return-values)
                        ;;(values ...)
                        (cadr return-values))
                       (t
                        return-values)))))
                (:macro)
                (:special-form
                 ;;We could code walk..
                 ))))
           (t
            ;;(lambda ...)
            ;;We can't derive anything
            ))))
      (t nil))))

(defmacro do-enumerable ((var enumerable &optional result)
                         &body body
                         &environment env)
  (let ((exp-type (%expression-type enumerable env)))
    (funcall (%get-expander exp-type) exp-type var enumerable result body env)))
