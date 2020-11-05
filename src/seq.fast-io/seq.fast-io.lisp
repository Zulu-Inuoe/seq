(defpackage #:com.inuoe.seq.fast-io
  (:use #:cl)
  (:import-from
   #:alexandria
   #:if-let
   #:when-let)
  (:import-from
   #:com.inuoe.seq
   #:col-seq
   #:lazy-seq
   #:mapcol))

(in-package #:com.inuoe.seq.fast-io)

(defmethod col-seq ((buffer fast-io:input-buffer))
  (labels ((recurse-segment ()
             ;; Copy the immediately available segment
             (if-let ((vec (fast-io:input-buffer-vector buffer)))
               (let ((pos (fast-io::input-buffer-pos buffer))
                     (len (length vec)))
                 (if (< pos len)
                     (loop
                       :with head := (cons (aref vec pos) nil)
                       :for i :from pos :below len
                       :for tail := head :then (setf (cdr tail) (cons (aref vec i) nil))
                       :finally
                          (setf (fast-io::input-buffer-pos buffer) len
                                (cdr tail) (lazy-seq (recurse-segment)))
                          (return head))
                     ;; Nothing available
                     (recurse-byte)))
               ;; Nothing available
               (recurse-byte)))
           (recurse-byte ()
             (when-let ((val (fast-io:fast-read-byte buffer nil)))
               (cons val (lazy-seq (recurse-byte))))))
    (recurse-segment)))

(defmethod mapcol ((buffer fast-io:input-buffer) fn)
  (when-let ((vect (fast-io:input-buffer-vector buffer)))
    (let ((pos (fast-io::input-buffer-pos buffer))
          (len (length vect)))
      (when (< pos len)
        ;; We can map through the vector elements eagerly since no side effects
        (unwind-protect
             (loop :for i :from pos :below len
                   :do (funcall fn (aref vect i)))
          ;; And afterwards set the buffer position
          (setf (fast-io::input-buffer-pos buffer) len)))))
  ;; And now we can continue reading lazily on the stream
  (loop
    :for b := (fast-io:fast-read-byte buffer nil)
    :while b
    :do (funcall fn b))
  (values))
