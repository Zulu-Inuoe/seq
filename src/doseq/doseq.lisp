(defpackage #:com.inuoe.doseq
  (:use
   #:cl)
  (:import-from
   #:alexandria
   #:parse-body
   #:type=
   #:when-let
   #:with-gensyms)
  (:import-from
   #:introspect-environment
   #:function-information
   #:variable-information)
  (:import-from
   #:com.inuoe.seq
   #:mapcol
   #:lazy-seq)
  (:export
   #:doseq

   #:define-doseq-expander))

(in-package #:com.inuoe.doseq)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *%doseq-expanders* ())

  (defun %map-expander (whole type var col result body env)
    (declare (ignore type env))
    (multiple-value-bind (body decl)
        (parse-body body :whole whole)
      `(block nil
         (mapcol (lambda (,var) ,@decl (tagbody ,@body)) ,col)
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
       (loop :for (type . expander) :in *%doseq-expanders*
             :if (subtypep decl-type type)
               :return expander
             :finally
                (return nil)))))

  (defun %set-expander (type expander)
    (cond
      (expander
       (let ((cell (assoc type *%doseq-expanders* :test #'type=)))
         (unless cell
           (setf cell (cons type nil))
           (push cell *%doseq-expanders*))
         (setf (cdr cell) expander)))
      (t
       (setf *%doseq-expanders*
             (delete type *%doseq-expanders* :test #'type= :key #'car))))
    (setf *%doseq-expanders* (stable-sort *%doseq-expanders* #'%subtype< :key #'car)))

  (defmacro define-doseq-expander
      (type (iter-whole iter-type iter-var iter-enum iter-res iter-body iter-env)
       &body body)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (%set-expander
        ',type
        (lambda (,iter-whole ,iter-type ,iter-var ,iter-enum ,iter-res ,iter-body ,iter-env)
          (declare (ignorable ,iter-whole ,iter-type ,iter-var ,iter-enum ,iter-res ,iter-body ,iter-env))
          ,@body))
       ',type)))

(define-doseq-expander vector
    (whole type var col result body env)
  (with-gensyms (vec i)
    (multiple-value-bind (body decls) (parse-body body :whole whole)
      `(let ((,vec ,col))
         (dotimes (,i (length ,vec) ,@(when result `((let (,var) ,var ,result))))
           (let ((,var (aref ,vec ,i)))
             ,@decls
             (tagbody ,@body)))))))

(define-doseq-expander hash-table
    (whole type var col result body env)
  (with-gensyms (iter more? key value)
    (multiple-value-bind (body decls) (parse-body body :whole whole)
      `(with-hash-table-iterator (,iter ,col)
         (do ()
             (nil)
           (multiple-value-bind (,more? ,key ,value)
               (,iter)
             (unless ,more?
               (return ,@(when result `((let (,var) ,var ,result)))))
             (let ((,var (cons ,key ,value)))
               ,@decls
               (tagbody ,@body))))))))

(define-doseq-expander stream
    (whole type var col result body env)
  (with-gensyms (enum-sym elt-sym)
    (multiple-value-bind (body decls) (parse-body body :whole whole)
      `(let ((,enum-sym ,col))
         (cond
           ((subtypep (stream-element-type ,enum-sym) 'integer)
            (do ((,elt-sym (read-byte ,enum-sym nil nil) (read-byte ,enum-sym nil nil)))
                ((null ,elt-sym) ,@(when result `((let (,var) ,var ,result))))
              (let ((,var ,elt-sym))
                ,@decls
                (tagbody ,@body))))
           ((subtypep (stream-element-type ,enum-sym) 'character)
            (do ((,elt-sym (read-char ,enum-sym nil nil) (read-char ,enum-sym nil nil)))
                ((null ,elt-sym) ,@(when result `((let (,var) ,var ,result))))
              (let ((,var ,elt-sym))
                ,@decls
                (tagbody ,@body))))
           (t (error "unsupported stream type '~A'" (stream-element-type ,enum-sym))))))))

(define-doseq-expander lazy-seq
    (whole type var col result body env)
  (with-gensyms (seq-sym)
    (multiple-value-bind (body decls) (parse-body body :whole whole)
      `(do ((,seq-sym (col-seq ,col) (col-seq (seq-rest ,seq-sym))))
           ((null ,seq-sym) ,@(when result `((let (,var) ,var ,result))))
         (let ((,var (seq-first ,seq-sym)))
           ,@decls
           (tagbody ,@body))))))

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
           (variable-information exp env)
         (declare (ignore bind-type local))
         ;;Try and find its declared type
         (cdr (assoc 'type decl-info))))
      ((listp exp)
       (destructuring-bind (fn &rest args) exp
         (cond
           ((symbolp fn)
            ;;fn
            (multiple-value-bind (bind-type local decl-info)
                (function-information fn env)
              (case bind-type
                (nil nil)
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
            nil))))
      (t nil)))

  (defun %doseq-expand (default-expander whole var col result body env)
    (let* ((exp-type (%expression-type col env))
           (expander (or (%get-expander exp-type) default-expander)))
      (funcall expander whole exp-type var col result body env))))

(defmacro doseq (&whole whole (var col &optional result)
                 &body body
                 &environment env)
  (%doseq-expand #'%map-expander whole var col result body env))
