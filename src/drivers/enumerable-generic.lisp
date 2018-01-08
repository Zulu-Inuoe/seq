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

(defmethod map-enumerable (fn enumerable)
  (loop :with enumerator := (get-enumerator enumerable)
        :while (move-next enumerator)
        :for x := (current enumerator)
        :do (funcall fn x))
  (values))

(defmethod any (enumerable)
  (do-enumerable (x enumerable)
    (declare (ignore x))
    (return t)))

(defmethod any* (enumerable predicate)
  (do-enumerable (x enumerable)
    (when (funcall predicate x)
      (return t))))

(defmethod contains (enumerable item &optional (test #'eql))
  (any* enumerable (lambda (x) (funcall test x item))))

(defmethod ecount (enumerable)
  (let ((count 0))
    (do-enumerable (x enumerable)
      (declare (ignore x))
      (incf count))
    count))

(defmethod ecount* (enumerable predicate)
  (let ((count 0))
    (do-enumerable (x enumerable)
      (when (funcall predicate x)
        (incf count)))
    count))

(defmethod default-if-empty (enumerable &optional default)
  (if (any enumerable)
      enumerable
      (list default)))

(defmethod efirst (enumerable &optional default)
  (do-enumerable (x enumerable)
    (return-from efirst x))
  default)

(defmethod efirst* (enumerable predicate &optional default)
  (do-enumerable (x enumerable)
    (when (funcall predicate x)
      (return-from efirst* x)))
  default)

(defmethod elast (enumerable &optional default)
  (let ((last-res default))
    (do-enumerable (x enumerable)
      (setf last-res x))
    last-res))

(defmethod elast* (enumerable predicate &optional default)
  (let ((last-res default))
    (do-enumerable (x enumerable)
      (when (funcall predicate x)
        (setf last-res x)))
    last-res))

(defmethod select (enumerable selector)
  (enumerable
    (loop :with enumerator := (get-enumerator enumerable)
          :while (move-next enumerator)
          :for x := (current enumerator)
          :do (yield (funcall selector x)))))

(defmethod select* (enumerable selector)
  (enumerable
    (loop :with enumerator := (get-enumerator enumerable)
          :while (move-next enumerator)
          :for x := (current enumerator)
          :for i :from 0 :by 1
          :do (yield (funcall selector x i)))))

(defmethod skip (enumerable count)
  (enumerable
    (let ((enumerator (get-enumerator enumerable)))
      (unless (zerop count)
        (loop :repeat count
              :for valid := (move-next enumerator)
              :while valid
              :finally
                 (unless valid
                   (yield-break))))
      (loop :while (move-next enumerator)
            :do (yield (current enumerator))))))

(defmethod skip-while (enumerable predicate)
  (enumerable
    (let ((enumerator (get-enumerator enumerable)))
      (loop :while (move-next enumerator)
            :for x := (current enumerator)
            :while (funcall predicate x)
            :finally (yield x))
      (loop :while (move-next enumerator)
            :for x := (current enumerator)
            :do (yield x)))))

(defmethod take (enumerable count)
  (enumerable
    (loop
      :repeat count
      :with enumerator := (get-enumerator enumerable)
      :while (move-next enumerator)
      :for x := (current enumerator)
      :do (yield x))))

(defmethod take-while (enumerable predicate)
  (enumerable
    (loop
      :with enumerator := (get-enumerator enumerable)
      :while (move-next enumerator)
      :for x := (current enumerator)
      :if (funcall predicate x)
        :do (yield x)
      :else
        :do (yield-break))))

(defmethod where (enumerable predicate)
  (enumerable
    (loop
      :with enumerator := (get-enumerator enumerable)
      :while (move-next enumerator)
      :for x := (current enumerator)
      :if (funcall predicate x)
        :do (yield x))))

(defmethod to-list (enumerable)
  (let ((res ()))
    (do-enumerable (x enumerable)
      (push x res))
    (nreverse res)))

(defmethod to-vector (enumerable)
  (let ((res ())
        (len 0))
    (do-enumerable (x enumerable)
      (push x res)
      (incf len))
    (make-array len :initial-contents (nreverse res))))
