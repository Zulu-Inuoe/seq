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

(defmethod map-enumerable (fn (enumerable list))
  (mapc fn enumerable)
  (values))

(defmethod get-enumerator ((enumerable list))
  (cons nil enumerable))

(defmethod current ((enumerator cons))
  (car enumerator))

(defmethod move-next ((enumerator cons))
  (if (cdr enumerator)
      (progn
        (setf (car enumerator) (cadr enumerator))
        (setf (cdr enumerator) (cddr enumerator))
        t)
      nil))

(defmethod eappend ((enumerable list) element)
  (with-enumerable
    (loop
      :for x :in enumerable
      :do (yield x)
      :finally (yield element))))

(defmethod consume ((enumerable list))
  (values))

(defmethod element-at ((enumerable list) index &optional default)
  (if-let ((cell (nthcdr index enumerable)))
    (car cell)
    default))

(defmethod evaluate ((functions list))
  (with-enumerable
    (dolist (fn functions)
      (yield (funcall fn)))))

(defmethod elast ((enumerable list) &optional default)
  (cond
    (enumerable
     (last enumerable))
    (t
     default)))

(defmethod elast* ((enumerable list) predicate &optional default)
  (let ((last-res default))
    (dolist (x enumerable last-res)
      (when (funcall predicate x)
        (setf last-res x)))))

(defmethod prepend ((enumerable list) element)
  (cons element enumerable))

(defmethod select ((enumerable list) selector)
  (with-enumerable
    (dolist (x enumerable)
      (yield (funcall selector x)))))

(defmethod select* ((enumerable list) selector)
  (with-enumerable
    (loop
      :for x :in enumerable
      :for i :from 0 :by 1
      :do (yield (funcall selector x i)))))

(defmethod select-many ((enumerable list) selector &optional (result-selector #'identity))
  (with-enumerable
    (loop
      :for elt :in enumerable
      :do
         (do-enumerable (sub-elt (funcall selector elt))
           (yield (funcall result-selector sub-elt))))))

(defmethod select-many* ((enumerable list) selector &optional (result-selector #'identity))
  (with-enumerable
    (loop
      :for elt :in enumerable
      :for i :from 0 :by 1
      :do
         (do-enumerable (sub-elt (funcall selector elt i))
           (yield (funcall result-selector sub-elt))))))

(defmethod skip ((enumerable list) count)
  (cond
    ((zerop count)
     enumerable)
    (t
     (with-enumerable
       (dolist (x (nthcdr count enumerable))
         (yield x))))))

(defmethod skip-last ((enumerable list) count)
  (butlast enumerable count))

(defmethod skip-until ((enumerable list) predicate)
  (with-enumerable
    (let ((cell enumerable))
      (loop
        :while (and cell (not (funcall predicate (car cell))))
        :do (setf cell (cdr cell)))
      (dolist (x cell)
        (yield x)))))

(defmethod skip-while ((enumerable list) predicate)
  (with-enumerable
    (let ((cell enumerable))
      (loop
        :while (and cell (funcall predicate (car cell)))
        :do (setf cell (cdr cell)))
      (dolist (x cell)
        (yield x)))))

(defmethod take ((enumerable list) count)
  (cond
    ((zerop count)
     nil)
    (t
     (with-enumerable
       (loop
         :repeat count
         :for x :in enumerable
         :do (yield x))))))

(defmethod take-every ((enumerable list) step)
  (with-enumerable
    (let ((i 0)
          (next-index 0))
      (dolist (elt enumerable)
        (when (= i next-index)
          (yield elt)
          (incf next-index step))
        (incf i)))))

(defmethod take-last ((enumerable list) count)
  (cond
    ((zerop count)
     nil)
    (t
     (let ((res (make-array count :fill-pointer t))
           (index 0)
           (filled nil))
       (dolist (elt enumerable)
         (setf (aref res index) elt)
         (when (= (incf index) count)
           (setf index 0)
           (setf filled t)))

       (if filled
           ;;Organize the array by shifting things to the left by `index'
           (loop :repeat index
                 :do
                    (loop :for i :from 0 :below (1- count)
                          :do (rotatef (aref res i)
                                       (aref res (1+ i)))))
           (setf (fill-pointer res) index))
       res))))

(defmethod take-until ((enumerable list) predicate)
  (with-enumerable
    (loop
      :for x :in enumerable
      :until (funcall predicate x)
      :do (yield x))))

(defmethod take-while ((enumerable list) predicate)
  (with-enumerable
    (loop
      :for x :in enumerable
      :while (funcall predicate x)
      :do (yield x))))

(defmethod where ((enumerable list) predicate)
  (with-enumerable
    (loop
      :for x :in enumerable
      :if (funcall predicate x)
        :do (yield x))))
