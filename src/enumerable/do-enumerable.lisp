(in-package #:enumerable)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %derived-fun-type (function)
    #+sbcl
    (sb-impl::%fun-type function))

  (defun %expression-type (exp env)
    (setf exp (macroexpand exp env))
    (cond
      ((constantp exp env)
       ;;Constant expression, just get its type
       (type-of exp))
      ((symbolp exp)
       (multiple-value-bind (bind-type local decl-info)
           (introspect-environment:variable-information exp env)
         (declare (ignore bind-type local))
         ;;Try and find its declared type
         (cdr (assoc 'type decl-info))))
      ((listp exp)
       (destructuring-bind (fn &rest args) exp
         (cond
           ((symbolp fn)
            ;;fn
            (multiple-value-bind (bind-type local decl-info)
                (introspect-environment:function-information fn env)
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
                (:macro
                 ;; Shouldn't be here given we macroexpanded the expression..
                 )
                (:special-form
                 (case fn
                   ((load-time-value)
                    (%expression-type (car args) env))
                   ((progn)
                    (%expression-type (car (last args)) env))
                   ((setq)
                    (%expression-type (car (last args)) env))
                   ((tagbody) ;; tagbody always returns nil
                    'null)
                   ((the #+sbcl sb-ext:truly-the)
                    (car args))
                   ((unwind-protect)
                    (%expression-type (car args) env)))))))
           (t
            ;;((lambda ...) args)
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
  (%do-enumerable-expand #'%map-expander var enumerable result body env))
