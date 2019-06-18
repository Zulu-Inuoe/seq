(in-package #:enumerable)

(defstruct (%seq-enumerator
            (:conc-name nil)
            (:constructor %make-seq-enumerator (%seq-enumerator-current))
            (:copier nil))
  (%seq-enumerator-current (required-argument)
   :type t))

(defun make-seq-enumerator (col)
  (when-let* ((seq (col-seq col)))
    (%make-seq-enumerator (cons nil seq))))

(defmethod current ((enumerator %seq-enumerator))
  (seq-first (%seq-enumerator-current enumerator)))

(defmethod move-next ((enumerator %seq-enumerator))
  (and (setf (%seq-enumerator-current enumerator) (col-seq (seq-rest (%seq-enumerator-current enumerator))))
       t))

(defmethod map-enumerable (fn (enumerable lazy-seq))
  (loop
    :for seq := (col-seq enumerable) :then (col-seq (seq-rest seq))
    :while seq
    :do (funcall fn (seq-first seq))))

(defmethod get-enumerator ((col lazy-seq))
  (make-seq-enumerator col))
