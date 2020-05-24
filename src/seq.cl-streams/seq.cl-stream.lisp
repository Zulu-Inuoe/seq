(defpackage #:com.inuoe.seq.cl-stream
  (:use #:cl)
  (:import-from
   #:alexandria
   #:if-let
   #:required-argument)
  (:import-from
   #:com.inuoe.seq
   #:col-seq
   #:lazy-seq
   #:seq-first
   #:seq-rest)
  (:export
   #:make-seq-input-stream))

(in-package #:com.inuoe.seq.cl-stream)

(defmethod col-seq ((stream cl-stream:input-stream))
  (labels ((recurse ()
             (multiple-value-bind (value indicator) (cl-stream:read stream)
               (unless (eq indicator :eof)
                 (cons value (lazy-seq (recurse)))))))
    (lazy-seq (recurse))))

(defstruct (seq-input-stream (:conc-name nil)
                             (:constructor make-seq-input-stream (seq-input-stream-col))
                             (:copier nil)
                             (:predicate nil))
  (seq-input-stream-col (required-argument :seq-input-stream)))

(defmethod cl-stream:stream-read ((stream seq-input-stream))
  (if-let ((seq (col-seq (seq-input-stream-col stream))))
    (progn
      (setf (seq-input-stream-col stream) (seq-rest seq))
      (values (seq-first seq) nil))
    (values nil :eof)))
