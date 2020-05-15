(defpackage #:com.inuoe.seq.gray-streams
  (:use
   #:cl
   #:trivial-gray-streams)
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
  (:export
   #:seq-input-stream
   #:make-seq-input-stream))

(in-package #:com.inuoe.seq.gray-streams)

(defclass seq-input-stream (fundamental-input-stream)
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

(defmethod stream-read-char ((stream seq-input-stream))
  (cond
    ((%unread-chars stream)
     (pop (%unread-chars stream)))
    (t
     (if-let ((seq (col-seq (%col stream))))
       (prog1 (seq-first seq)
         (setf (%col stream) (seq-rest seq)))
       :eof))))

(defmethod stream-read-line ((stream seq-input-stream))
  (let ((res (make-string-output-stream)))
    (with-slots (%col) stream
      (loop
        :with eof := t
        :for seq := (col-seq %col) :then (col-seq (seq-rest seq))
        :while seq
        :for c := (seq-first seq)
        :if (char= c #\NewLine)
          :do (progn
                (setf eof nil)
                (loop-finish))
        :else
          :do (write-char c res)
        :finally
           (setf %col (seq-rest seq))
           (return (values (get-output-stream-string res) eof))))))

(defmethod stream-unread-char ((stream seq-input-stream) character)
  (push character (%unread-chars stream))
  (values))

(defmethod stream-listen ((stream seq-input-stream))
  t)

(defmethod stream-read-byte ((stream seq-input-stream))
  (if-let ((seq (col-seq (%col stream))))
    (prog1 (seq-first seq)
      (setf (%col stream) (seq-rest seq)))
    (prog1 :eof
      (setf (%col stream) nil))))

(defmethod stream-read-sequence ((stream seq-input-stream) (sequence list) start end &key &allow-other-keys)
  (loop
    :with col := (%col stream)
    :for pos :from start
    :for cell :on (nthcdr start sequence)
    :while (and (or (null end) (< pos end)))
    :do
       (if-let ((seq (col-seq col)))
         (setf (car cell) (seq-first seq)
               col (seq-rest seq))
         (loop-finish))
    :finally
       (setf (%col stream) col)
       (return pos)))

(defmethod stream-read-sequence ((stream seq-input-stream) (sequence vector) start end &key &allow-other-keys)
  (loop
    :with col := (%col stream)
    :for pos :from start
    :while (and (or (null end) (< pos end))
                (< pos (length sequence)))
    :do
       (if-let ((seq (col-seq col)))
         (setf (aref sequence pos) (seq-first seq)
               col (seq-rest seq))
         (loop-finish))
    :finally
       (setf (%col stream) col)
       (return pos)))

(defmethod stream-read-sequence ((stream seq-input-stream) (sequence sequence) start end &key &allow-other-keys)
  (let ((pos 0)
        (col (%col stream)))
    (block nil
      (map-into sequence
                (lambda (elt)
                  (cond
                    ((< pos start)
                     (incf pos)
                     elt)
                    (t
                     (if-let ((seq (and (or (null end) (< pos end))
                                        (col-seq col))))
                       (prog1 (seq-first seq)
                         (incf pos)
                         (setf col (seq-rest seq)))
                       (progn
                         (setf col nil)
                         (return))))))
                sequence))
    (setf (%col stream) col)
    pos))
