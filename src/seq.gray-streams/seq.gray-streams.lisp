(defpackage #:com.inuoe.seq.gray-streams
  (:use #:cl)
  (:import-from
   #:alexandria
   #:if-let
   #:required-argument)
  (:import-from
   #:trivial-gray-streams
   #:fundamental-input-stream)
  (:import-from
   #:com.inuoe.seq
   #:col-seq
   #:seq-first
   #:seq-rest)
  (:local-nicknames
   (#:gs #:trivial-gray-streams))
  (:export
   #:seq-input-stream
   #:make-seq-input-stream))

(in-package #:com.inuoe.seq.gray-streams)

(defclass seq-input-stream (gs:fundamental-input-stream)
  ((%col
    :initarg :col
    :initform (required-argument :col)
    :accessor %col)
   (%unread-chars
    :initform nil
    :type list
    :accessor %unread-chars)))

(defun make-seq-input-stream (col)
  (make-instance 'seq-input-stream :col col))

(defmethod gs:stream-read-char ((stream seq-input-stream))
  (cond
    ((%unread-chars stream)
     (pop (%unread-chars stream)))
    (t
     (if-let ((seq (col-seq (%col stream))))
       (prog1 (seq-first seq)
         (setf (%col stream) (seq-rest seq)))
       :eof))))

(defmethod gs:stream-read-line ((stream seq-input-stream))
  (let ((col (%col stream)))
    (unwind-protect
         (let ((res (make-string-output-stream))
               (eof t))
           (loop
             :for c := (pop (%unread-chars stream))
             :while c
             :do (case c
                   (#\Newline
                    (setf eof nil)
                    (loop-finish))
                   (write-char c res)))
           (when eof
             (loop
               :for seq := (col-seq col)
               :while seq
               :do (let ((c (seq-first seq)))
                     (setf col (seq-rest seq))
                     (case c
                       (#\Newline
                        (setf eof nil)
                        (loop-finish))
                       (t
                        (write-char c res))))))
           (values (get-output-stream-string res) eof))
      (setf (%col stream) col))))

(defmethod gs:stream-unread-char ((stream seq-input-stream) character)
  (push character (%unread-chars stream))
  (values))

(defmethod gs:stream-listen ((stream seq-input-stream))
  t)

(defmethod gs:stream-read-byte ((stream seq-input-stream))
  (if-let ((seq (col-seq (%col stream))))
    (prog1 (seq-first seq)
      (setf (%col stream) (seq-rest seq)))
    (prog1 :eof
      (setf (%col stream) nil))))

(defmethod gs:stream-read-sequence ((stream seq-input-stream) (sequence list) start end &key &allow-other-keys)
  (let ((col (%col stream)))
    (unwind-protect
         (loop
           :for pos :from start
           :for cell :on (nthcdr start sequence)
           :for seq := (col-seq col)
           :while (and seq (or (null end) (< pos end)))
           :do (setf (car cell) (seq-first seq)
                     col (seq-rest seq))
           :finally (return pos))
      (setf (%col stream) col))))

(defmethod gs:stream-read-sequence ((stream seq-input-stream) (sequence vector) start end &key &allow-other-keys)
  (let ((col (%col stream)))
    (unwind-protect
         (loop
           :for pos :from start :below (let ((len (length sequence)))
                                         (min len (or end len)))
           :for seq := (col-seq col)
           :while seq
           :do (setf (aref sequence pos) (seq-first seq)
                     col (seq-rest seq))
           :finally (return pos))
      (setf (%col stream) col))))

(defmethod gs:stream-read-sequence ((stream seq-input-stream) (sequence sequence) start end &key &allow-other-keys)
  (let ((col (%col stream)))
    (unwind-protect
         (let ((pos 0))
           (block nil
             (map-into sequence
                       (lambda (elt)
                         (cond
                           ((< pos start)
                            (incf pos)
                            elt)
                           ((or (null end) (< pos end))
                            (if-let ((seq (col-seq col)))
                              (prog1 (seq-first seq)
                                (incf pos)
                                (setf col (seq-rest seq)))
                              (return)))
                           (t
                            (return))))
                       sequence))
           pos)
      (setf (%col stream) col))))
