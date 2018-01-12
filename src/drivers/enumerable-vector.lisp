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

(defmethod any* ((enumerable vector) predicate)
  (loop
    :for i :from 0 :below (length enumerable)
    :if (funcall predicate (aref enumerable i))
      :return t
    :finally (return nil)))

(defmethod eappend ((enumerable vector) element)
  (with-enumerable
    (loop
      :for x :across enumerable
      :do (yield x)
      :finally (yield element))))

(defmethod consume ((enumerable vector))
  (values))

(defmethod contains ((enumerable vector) item &optional (test #'eql))
  (loop
    :for i :from 0 :below (length enumerable)
    :if (funcall test item (aref enumerable i))
      :return t
    :finally (return nil)))

(defmethod element-at ((enumerable vector) index &optional default)
  (cond
    ((< index (length enumerable))
     (aref enumerable index))
    (t
     default)))

(defmethod evaluate ((functions vector))
  (with-enumerable
    (loop
      :for fn :across functions
      :do (yield (funcall fn)))))

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

(defmethod single ((enumerable vector) &optional default)
  (let ((len (length enumerable)))
    (cond
      ((> len 1)
       (error "more than one element present in the enumerable"))
      ((= len 1)
       (aref enumerable 0))
      (t
       default))))

(defmethod single* ((enumerable vector) predicate &optional default)
  (loop
    :with found-value := nil
    :with ret := default
    :for elt :across enumerable
    :if (funcall predicate elt)
      :do (if found-value
              (error "more than one element present in the enumerable matches predicate")
              (setf found-value t
                    ret elt))
    :finally (return ret)))

(defmethod skip ((enumerable vector) count)
  (cond
    ((zerop count)
     enumerable)
    (t
     (with-enumerable
       (let ((i count)
             (len (length enumerable)))
         (loop
           :while (< i len)
           :do
              (yield (aref enumerable i))
              (incf i)))))))

(defmethod skip-last ((enumerable vector) count)
  (let ((len (length enumerable)))
    (cond
      ((>= count len)
       nil)
      (t
       (with-enumerable
         (loop :for i :from 0 :below (- len count)
               :do (yield (aref enumerable i))))))))

(defmethod skip-until ((enumerable vector) predicate)
  (with-enumerable
    (let ((i 0)
          (len (length enumerable)))
      (loop
        :while (and (< i len) (not (funcall predicate (aref enumerable i))))
        :do (incf i))
      (loop
        :while (< i len)
        :do
           (yield (aref enumerable i))
           (incf i)))))

(defmethod skip-while ((enumerable vector) predicate)
  (with-enumerable
    (let ((i 0)
          (len (length enumerable)))
      (loop
        :while (and (< i len) (funcall predicate (aref enumerable i)))
        :do (incf i))
      (loop
        :while (< i len)
        :do
           (yield (aref enumerable i))
           (incf i)))))

(defmethod take ((enumerable vector) count)
  (with-enumerable
    (loop
      :repeat count
      :for x :across enumerable
      :do (yield x))))

(defmethod take-every ((enumerable vector) step)
  (unless (and (integerp step)
               (plusp step))
    (error "step must be a positive integer, was ~A" step))
  (with-enumerable
    (loop
      :for i :from 0 :below (length enumerable) :by step
      :do (yield (aref enumerable i)))))

(defmethod take-last ((enumerable vector) count)
  (when (minusp count)
    (error "count cannot be negative, was ~A" count))
  (let ((len (length enumerable)))
    (cond
      ((>= count len)
       enumerable)
      ((zerop count)
       nil)
      (t
       (with-enumerable
         (loop :for i :from (- len count) :below len
               :do (yield (aref enumerable i))))))))

(defmethod take-until ((enumerable vector) predicate)
  (with-enumerable
    (loop
      :for x :across enumerable
      :until (funcall predicate x)
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

(defmethod to-vector ((enumerable vector) &key (element-type (array-element-type enumerable)) adjustable fill-pointer-p)
  (make-array (length enumerable)
              :element-type element-type
              :initial-contents enumerable
              :adjustable adjustable
              :fill-pointer (and fill-pointer-p t)))
