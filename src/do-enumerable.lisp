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
                                          (and (fboundp fn)
                                               (%derived-fun-type (symbol-function fn))))))))
                   ;;ftype definition is
                   (destructuring-bind (function params return-values)
                       ftype
                     (declare (ignore function params))
                     (cond
                       ((listp return-values)
                        ;;(values ...)
                        (cadr return-values))
                       ((eq return-values '*)
                        nil)
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
      (t nil)))

  (defun %do-enumerable-expand (default-expander var enumerable result body env)
    (let* ((exp-type (%expression-type enumerable env))
           (expander (or (%get-expander exp-type) default-expander)))
      (funcall expander exp-type var enumerable result body env))))

(defmacro do-enumerable ((var enumerable &optional result)
                         &body body
                         &environment env)
  (%do-enumerable-expand #'%typecase-map-expander var enumerable result body env))
