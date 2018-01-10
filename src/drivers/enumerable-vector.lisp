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

(defmethod map-enumerable (fn (enumerable vector))
  (loop :for x :across enumerable
        :do (funcall fn x))
  (values))

(defmethod eappend ((enumerable vector) element)
  (with-enumerable
    (loop
      :for x :across enumerable
      :do (yield x)
      :finally (yield element))))

(defmethod element-at ((enumerable vector) index &optional default)
  (cond
    ((< index (length enumerable))
     (aref enumerable index))
    (t
     default)))

(defmethod elast ((enumerable vector) &optional default)
  (let ((len (length enumerable)))
    (cond
      ((zerop len) default)
      (t
       (aref enumerable (1- len))))))

(defmethod elast* ((enumerable vector) predicate &optional default)
  (loop
    :for i :from (1- (length enumerable)) :downto 0
    :for elt := (aref enumerable i)
    :if (funcall predicate elt)
      :return elt
    :finally (return default)))

(defmethod prepend ((enumerable vector) element)
  (with-enumerable
    (loop
      :initially (yield element)
      :for x :across enumerable
      :do (yield x))))

(defmethod select ((enumerable vector) selector)
  (with-enumerable
    (loop
      :for x :across enumerable
      :do (yield (funcall selector x)))))

(defmethod select* ((enumerable vector) selector)
  (with-enumerable
    (loop
      :for x :across enumerable
      :for i :from 0 :by 1
      :do (yield (funcall selector x i)))))

(defmethod select-many ((enumerable vector) selector &optional (result-selector #'identity))
  (with-enumerable
    (loop
      :for elt :across enumerable
      :do
         (do-enumerable (sub-elt (funcall selector elt))
           (yield (funcall result-selector sub-elt))))))

(defmethod select-many* ((enumerable vector) selector &optional (result-selector #'identity))
  (with-enumerable
    (loop
      :for elt :across enumerable
      :for i :from 0 :by 1
      :do
         (do-enumerable (sub-elt (funcall selector elt i))
           (yield (funcall result-selector sub-elt))))))

(defmethod take ((enumerable vector) count)
  (with-enumerable
    (loop
      :repeat count
      :for x :across enumerable
      :do (yield x))))

(defmethod take-while ((enumerable vector) predicate)
  (with-enumerable
    (loop
      :for x :across enumerable
      :while (funcall predicate x)
      :do (yield x))))

(defmethod where ((enumerable vector) predicate)
  (with-enumerable
    (loop
      :for x :across enumerable
      :if (funcall predicate x)
        :do (yield x))))

(defmethod to-vector ((enumerable sequence) &key (element-type (array-element-type enumerable)) adjustable fill-pointer-p)
  (make-array (length enumerable)
              :element-type element-type
              :initial-contents enumerable
              :adjustable adjustable
              :fill-pointer (and fill-pointer-p t)))
