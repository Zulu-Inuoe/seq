(defpackage #:com.inuoe.seq.xpath
  (:use #:cl)
  (:import-from
   #:com.inuoe.seq
   #:col-seq
   #:lazy-seq
   #:mapcol))

(in-package #:com.inuoe.seq.xpath)

(defmethod col-seq ((node-set xpath:node-set))
  (lazy-seq
    (let ((it (xpath:make-node-set-iterator node-set)))
      (labels ((recurse ()
                 (unless (xpath:node-set-iterator-end-p it)
                   (cons (xpath:node-set-iterator-current it)
                         (lazy-seq
                           (xpath:node-set-iterator-next it)
                           (recurse))))))
        (recurse)))))

(defmethod mapcol ((node-set xpath:node-set) fn)
  (xpath:map-node-set fn node-set)
  (values))
