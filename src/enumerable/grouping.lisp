(in-package #:enumerable)

(defclass grouping ()
  ((key
    :initarg :key
    :initform (error "group: must supply key")
    :reader grouping-key)
   (%enumerable
    :initarg :enumerable)))

(defmethod print-object ((object grouping) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~S ~S" (grouping-key object) (slot-value object '%enumerable))))

(defmethod get-enumerator ((enumerable grouping))
  (get-enumerator (slot-value enumerable '%enumerable)))

(defun make-grouping (key values)
  (make-instance 'grouping :key key :enumerable values))
