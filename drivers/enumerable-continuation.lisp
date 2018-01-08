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

(defclass continuation-enumerable ()
  ((starter
    :type (function () continuation-enumerator)
    :initarg :starter
    :initform (error "must supply starter"))))

(defclass continuation-enumerator ()
  ((current
    :type t
    :initform nil)
   (continuation
    :type (function () (values t boolean))
    :initarg :continuation
    :initform (error "must supply continuation"))))

(defmethod get-enumerator ((enumerable continuation-enumerable))
  (with-slots (starter)
      enumerable
    (make-instance
     'continuation-enumerator
     :continuation (funcall starter))))

(defmethod current ((enumerator continuation-enumerator))
  (slot-value enumerator 'current))

(defmethod move-next ((enumerator continuation-enumerator))
  (with-slots (current continuation)
      enumerator
    (multiple-value-bind (val valid)
        (funcall continuation)
      (setf current val)
      valid)))
