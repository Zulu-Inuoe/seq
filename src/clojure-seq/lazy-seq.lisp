(in-package #:clojure-seq)

(defstruct (lazy-seq
            (:conc-name nil)
            (:constructor %make-lazy-seq (%lazy-seq-factory))
             (:copier nil))
  (%lazy-seq-factory (required-argument)
   :type function
   :read-only t)
  (%lazy-seq-realized-p nil
   :type boolean)
  (%lazy-seq-value nil
   :type t))

(defmethod print-object ((object lazy-seq) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "[~:[ ~;~S~]]" (%lazy-seq-realized-p object) (%lazy-seq-value object))))

(defmethod col-seq ((lazy-seq lazy-seq))
  (with-accessors ((%lazy-seq-factory %lazy-seq-factory)
                   (%lazy-seq-realized-p %lazy-seq-realized-p)
                   (%lazy-seq-value %lazy-seq-value))
      lazy-seq
    (unless %lazy-seq-realized-p
      (setf %lazy-seq-value (funcall %lazy-seq-factory)
            %lazy-seq-realized-p t))
     %lazy-seq-value))

(defmethod seq-first ((lazy-seq lazy-seq))
  (seq-first (col-seq lazy-seq)))

(defmethod seq-rest ((lazy-seq lazy-seq))
  (seq-rest (col-seq lazy-seq)))

(defmacro lazy-seq (&body body)
  `(%make-lazy-seq (lambda () (col-seq (progn ,@body)))))
