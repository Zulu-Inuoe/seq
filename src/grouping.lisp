;;;enumerable - enumerable implementation for CL, using cl-cont
;;;Written in 2018 by Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>
;;;
;;;To the extent possible under law, the author(s) have dedicated all copyright
;;;and related and neighboring rights to this software to the public domain
;;;worldwide. This software is distributed without any warranty.
;;;You should have received a copy of the CC0 Public Domain Dedication along
;;;with this software. If not, see
;;;<http://creativecommons.org/publicdomain/zero/1.0/>.

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
    (princ (grouping-key object) stream)))

(defmethod get-enumerator ((enumerable grouping))
  (get-enumerator (slot-value enumerable '%enumerable)))

(defun make-grouping (key values)
  (make-instance 'grouping :key key :enumerable values))
