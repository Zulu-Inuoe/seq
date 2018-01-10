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

(defmethod all (enumerable predicate)
  (not (any* enumerable (complement predicate))))

(defmethod any (enumerable)
  (do-enumerable (x enumerable nil)
    (declare (ignore x))
    (return t)))

(defmethod any* (enumerable predicate)
  (do-enumerable (x enumerable)
    (when (funcall predicate x)
      (return t))))

(defmethod eappend (enumerable element)
  (enumerable
    (loop :with enumerator := (get-enumerator enumerable)
          :while (move-next enumerator)
          :do (yield (current enumerator)))
    (yield element)))

(defmethod concat (first second)
  (enumerable
    (loop :with enumerator := (get-enumerator first)
          :while (move-next enumerator)
          :do (yield (current enumerator)))
    (loop :with enumerator := (get-enumerator second)
          :while (move-next enumerator)
          :do (yield (current enumerator)))))

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

(defmethod distinct (enumerable &optional (test #'eql))
  (enumerable
    (let ((known-elements ()))
      (loop :with enumerator := (get-enumerator enumerable)
            :while (move-next enumerator)
            :for x := (current enumerator)
            :unless (find x known-elements :test test)
              :do (progn
                    (yield x)
                    (push x known-elements))))))

(defmethod element-at (enumerable index &optional default)
  (efirst (skip enumerable index) default))

(defmethod except (first second &optional (test #'eql))
  (where first (lambda (elt) (not (contains second elt test)))))

(defmethod efirst (enumerable &optional default)
  (do-enumerable (x enumerable default)
    (return-from efirst x)))

(defmethod efirst* (enumerable predicate &optional default)
  (do-enumerable (x enumerable default)
    (when (funcall predicate x)
      (return-from efirst* x))))

(defmethod elast (enumerable &optional default)
  (let ((last-res default))
    (do-enumerable (x enumerable last-res)
      (setf last-res x))))

(defmethod elast* (enumerable predicate &optional default)
  (let ((last-res default))
    (do-enumerable (x enumerable last-res)
      (when (funcall predicate x)
        (setf last-res x)))))

(defmethod prepend (enumerable element)
  (enumerable
    (yield element)
    (loop :with enumerator := (get-enumerator enumerable)
          :while (move-next enumerator)
          :do (yield (current enumerator)))))

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

(defmethod select-many (enumerable selector &optional (result-selector #'identity))
  (enumerable
    (loop :with enumerator := (get-enumerator (select enumerable selector))
          :while (move-next enumerator)
          :for elt := (current enumerator)
          :do
             (loop :with elt-enumerator := (get-enumerator elt)
                   :while (move-next elt-enumerator)
                   :do (yield (funcall result-selector (current elt-enumerator)))))))

(defmethod select-many* (enumerable selector &optional (result-selector #'identity))
  (enumerable
    (loop :with enumerator := (get-enumerator (select* enumerable selector))
          :while (move-next enumerator)
          :for elt := (current enumerator)
          :do
             (loop :with elt-enumerator := (get-enumerator elt)
                   :while (move-next elt-enumerator)
                   :do (yield (funcall result-selector (current elt-enumerator)))))))

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

(defmethod to-vector (enumerable &key (element-type t) adjustable fill-pointer-p)
  (let ((res ())
        (len 0))
    (do-enumerable (x enumerable)
      (push x res)
      (incf len))
    (make-array len
                :element-type element-type
                :initial-contents (nreverse res)
                :adjustable adjustable
                :fill-pointer (and fill-pointer-p t))))
