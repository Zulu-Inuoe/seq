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

(defmethod get-enumerator ((col lazy-seq))
  (make-seq-enumerator col))
