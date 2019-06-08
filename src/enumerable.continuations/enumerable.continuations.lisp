(defpackage #:enumerable.continuations
  (:use
   #:alexandria
   #:cl
   #:enumerable)
  (:export
   #:with-enumerable
   #:lamdae
   #:defenumerable
   #:yield
   #:yield-break))

(in-package #:enumerable.continuations)

(defclass continuation-enumerable ()
  ((starter
    :type (function () continuation-enumerator)
    :initarg :starter
    :initform (error "must supply starter"))))

(defclass continuation-enumerator ()
  ((current
    :type t
    :initform nil)
   (continuation
    :type (function () (values t boolean))
    :initarg :continuation
    :initform (error "must supply continuation"))))

(defmethod get-enumerator ((enumerable continuation-enumerable))
  (with-slots (starter)
      enumerable
    (make-instance
     'continuation-enumerator
     :continuation (funcall starter))))

(defmethod current ((enumerator continuation-enumerator))
  (slot-value enumerator 'current))

(defmethod move-next ((enumerator continuation-enumerator))
  (with-slots (current continuation)
      enumerator
    (multiple-value-bind (val valid)
        (funcall continuation)
      (setf current val)
      valid)))

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
                             (do-enumerable ((var enumerable &optional result) &body body)
                               (with-gensyms (enumerator-sym)
                                 (multiple-value-bind (body decls)
                                     (parse-body body)
                                   `(do ((,enumerator-sym (get-enumerator ,enumerable)))
                                        ((not (move-next ,enumerator-sym)) ,@(when result `((let (,var) ,var ,result))))
                                      (let ((,var (current ,enumerator-sym)))
                                        ,@decls
                                        (tagbody ,@body)))))))
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
