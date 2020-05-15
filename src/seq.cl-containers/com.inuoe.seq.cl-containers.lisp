(defpackage #:com.inuoe.seq.cl-containers
  (:use
   #:cl)
  (:import-from
   #:com.inuoe.seq
   #:col-seq
   #:lazy-seq)
  (:import-from
   #:cl-containers
   #:iteratable-container-mixin
   #:make-iterator
   #:move-forward
   #:current-element))

(in-package #:com.inuoe.seq.cl-containers)

(defmethod col-seq ((col iteratable-container-mixin))
  (let ((iterator (make-iterator col)))
    (labels ((recurse ()
               (unless (move-forward iterator)
                 (cons (current-element iterator)
                       (lazy-seq (recurse))))))
      (recurse))))
