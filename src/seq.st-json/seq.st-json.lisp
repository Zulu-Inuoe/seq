(defpackage #:com.inuoe.seq.st-json
  (:use #:cl)
  (:import-from
   #:com.inuoe.seq
   #:col-seq
   #:lazy-seq))

(in-package #:com.inuoe.seq.st-json)

(defmethod col-seq ((obj st-json:jso))
  (lazy-seq
    (st-json:getjso*)
    (let ((it (xpath:make-node-set-iterator node-set)))
      (labels ((recurse ()
                 (unless (xpath:node-set-iterator-end-p it)
                   (cons (xpath:node-set-iterator-current it)
                         (lazy-seq
                           (xpath:node-set-iterator-next it)
                           (recurse))))))
        (recurse)))))
