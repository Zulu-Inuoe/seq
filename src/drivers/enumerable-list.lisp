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

(define-do-enumerable-expander list
    (type var enumerable result body env)
  `(dolist (,var ,enumerable ,result)
     ,@body))

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

(defmethod any ((enumerable list))
  (and enumerable t))

(defmethod any* ((enumerable list) predicate)
  (and (find-if predicate enumerable) t))

(defmethod efirst ((enumerable list) &optional default)
  (cond
    (enumerable
      (car enumerable))
    (t
     default)))

(defmethod efirst* ((enumerable list) predicate &optional default)
  (dolist (x enumerable default)
    (when (funcall predicate x)
      (return x))))

(defmethod elast ((enumerable list) &optional default)
  (cond
    (enumerable
      (last enumerable))
    (t
     default)))

(defmethod elast* ((enumerable list) predicate &optional default)
  (let ((res default))
    (dolist (x enumerable)
      (when (funcall predicate x)
        (setf res x)))
    res))

(defmethod select ((enumerable list) selector)
  (enumerable
    (dolist (x enumerable)
      (yield (funcall selector x)))))

(defmethod select* ((enumerable list) selector)
  (enumerable
    (loop
      :for x :in enumerable
      :for i :from 0 :by 1
      :do (yield (funcall selector x i)))))

(defmethod skip ((enumerable list) count)
  (enumerable
    (dolist (x (nthcdr count enumerable))
      (yield x))))

(defmethod skip-while ((enumerable list) predicate)
  (enumerable
    (let ((cell enumerable))
      (loop
        :while (and cell (funcall predicate (car cell)))
        :do (setf cell (cdr cell)))
      (dolist (x cell)
        (yield x)))))

(defmethod take ((enumerable list) count)
  (enumerable
    (loop
      :repeat count
      :for x :in enumerable
      :do (yield x))))

(defmethod take-while ((enumerable list) predicate)
  (enumerable
    (loop
      :for x :in enumerable
      :while (funcall predicate x)
      :do (yield x))))

(defmethod where ((enumerable list) predicate)
  (enumerable
    (loop
      :for x :in enumerable
      :if (funcall predicate x)
        :do (yield x))))

(defmethod to-list ((enumerable list))
  (copy-list enumerable))
