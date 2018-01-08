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

(define-do-enumerable-expander vector
    (type var enumerable result body env)
  `(loop :for ,var :across ,enumerable
         :do (progn ,@body)
         :finally
            (return ,result)))

(defmethod map-enumerable (fn (enumerable vector))
  (loop :for x :across enumerable
        :do (funcall fn x))
  (values))

(defmethod select ((enumerable vector) selector)
  (enumerable
    (loop
      :for x :across enumerable
      :do (yield (funcall selector x)))))

(defmethod select* ((enumerable vector) selector)
  (enumerable
    (loop
      :for x :across enumerable
      :for i :from 0 :by 1
      :do (yield (funcall selector x i)))))

(defmethod take ((enumerable vector) count)
  (enumerable
    (loop
      :repeat count
      :for x :across enumerable
      :do (yield x))))

(defmethod take-while ((enumerable vector) predicate)
  (enumerable
    (loop
      :for x :across enumerable
      :while (funcall predicate x)
      :do (yield x))))

(defmethod where ((enumerable vector) predicate)
  (enumerable
    (loop
      :for x :across enumerable
      :if (funcall predicate x)
        :do (yield x))))
