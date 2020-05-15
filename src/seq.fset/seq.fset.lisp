(defpackage #:com.inuoe.seq.fset
  (:use
   #:cl)
  (:import-from
   #:com.inuoe.seq
   #:col-seq
   #:lazy-seq))

(in-package #:com.inuoe.seq.fset)

(defmethod col-seq ((col fset:collection))
  (lazy-seq
    (let ((iterator (fset:iterator col)))
      (labels ((recurse ()
                 (multiple-value-bind (value more?) (funcall iterator :get)
                   (when more?
                     (cons value (lazy-seq (recurse)))))))
        (recurse)))))

(defmethod col-seq ((col fset:map))
  (lazy-seq
    (let ((iterator (fset:iterator col)))
      (labels ((recurse ()
                 (multiple-value-bind (key value more?) (funcall iterator :get)
                   (when more?
                     (cons (cons key value) (lazy-seq (recurse)))))))
        (recurse)))))
