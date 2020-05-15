(defpackage #:com.inuoe.seq.generators
  (:use
   #:cl)
  (:import-from
   #:alexandria
   #:if-let
   #:ensure-list
   #:parse-body
   #:with-gensyms)
  (:import-from
   #:com.inuoe.seq
   #:col-seq
   #:seq-first
   #:seq-rest
   #:lazy-seq)
  (:export
   #:with-generator
   #:generator
   #:defgenerator
   #:yield
   #:yield-break))

(in-package #:com.inuoe.seq.generators)

(defmacro with-generator (&body body)
  "Run `body' in an context where `yield' will yield a new value in
 the generator, and `yield-break' will end the generator.
 Example:
  (to-list
    (with-generator
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

  (with-gensyms (cc)
    `(labels ((,cc (,cc)
                (multiple-value-bind (value valid next) (funcall ,cc)
                  (when valid
                    (cons value (lazy-seq (,cc next)))))))
       (lazy-seq
         (,cc (lambda ()
                (cl-cont:with-call/cc
                  (flet ((yield (result)
                           (cl-cont:let/cc ,cc
                             (values result t ,cc)))
                         (yield-break ()
                           (cl-cont:let/cc ,cc
                             (declare (ignore ,cc))
                             (values nil nil (lambda () (values nil nil nil))))))
                    (progn ,@body)
                    (yield-break)))))))))

(defmacro generator (&whole whole args &body body)
  "Lambda who's body is a generator."
  (multiple-value-bind (body decls doc)
      (parse-body body :documentation t :whole whole)
    `(lambda ,args
       ,@(ensure-list doc)
       ,@decls
       (with-generator ,@body))))

(defmacro defgenerator (&whole whole name args &body body)
  "Like defun, for a function who's body is a generator."
  (multiple-value-bind (body decls doc)
      (parse-body body :documentation t :whole whole)
    `(defun ,name ,args
       ,@(ensure-list doc)
       ,@decls
       (with-generator ,@body))))
