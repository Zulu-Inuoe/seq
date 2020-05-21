(defpackage #:com.inuoe.seq-fastio
  (:use #:cl)
  (:import-from
   #:alexandria
   #:when-let)
  (:import-from
   #:com.inuoe.seq
   #:col-seq
   #:lazy-seq
   #:mapcol))

(in-package #:com.inuoe.seq-fastio)

(defmethod col-seq ((buffer fast-io:input-buffer))
  (labels ((recurse-bytes ()
                    (when-let ((val (fast-io:fast-read-byte buffer nil)))
                      (cons val (lazy-seq (recurse-bytes))))))
    (recurse-bytes)))

(defmethod mapcol (fn (buffer fast-io:input-buffer))
  (when-let ((vect (fast-io:input-buffer-vector buffer)))
    (let ((pos (fast-io::input-buffer-pos buffer))
          (len (length vect)))
      (when (< pos len)
        ;; We can map through the vector elements eagerly since no side effects
        (unwind-protect
             (loop :for i :from pos :below len
                   :do (funcall fn (aref vect i)))
          ;; And afterwards set the buffer position
          (setf (fast-io:buffer-position buffer) len)))))
  ;; And now we can continue reading lazily on the stream
  (loop
    :for b := (fast-io:fast-read-byte buffer nil)
    :while b
    :do (funcall fn b))
  (values))
