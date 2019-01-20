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

(defmethod map-enumerable (fn (enumerable sequence))
  (map nil fn enumerable)
  (values))

(defstruct (%sequence-enumerator
            (:conc-name nil)
            (:constructor %make-sequence-enumerator (%sequence-enumerator-sequence))
            (:copier nil))
  (%sequence-enumerator-sequence (required-argument '%sequence-enumerator-sequence)
   :type sequence
   :read-only t)
  (%sequence-enumerator-position -1
   :type (integer -1 *)))

(defmethod get-enumerator ((enumerable sequence))
  (%make-sequence-enumerator enumerable))

(defmethod current ((enumerator %sequence-enumerator))
  (unless (or (= -1 (%sequence-enumerator-position enumerator))
              (= (%sequence-enumerator-position enumerator) (length (%sequence-enumerator-sequence enumerator))))
    (aref (%sequence-enumerator-sequence enumerator) (%sequence-enumerator-position enumerator))))

(defmethod move-next ((enumerator %vector-enumerator))
  (when (< (%vector-enumerator-position enumerator)
           (length (%vector-enumerator-vector enumerator)))
    (incf (%vector-enumerator-position enumerator))
    (/= (%vector-enumerator-position enumerator)
        (length (%vector-enumerator-vector enumerator)))))

(defmethod aggregate ((enumerable sequence) aggregator)
  (when (emptyp enumerable)
    (error "enumerable contains no elements."))
  (reduce aggregator enumerable))

(defmethod aggregate* ((enumerable sequence) aggregator seed)
  (reduce aggregator enumerable :initial-value seed))

(defmethod all ((enumerable sequence) predicate)
  (every predicate enumerable))

(defmethod any ((enumerable sequence))
  (not (emptyp enumerable)))

(defmethod any* ((enumerable sequence) predicate)
  (and (position-if predicate enumerable) t))

(defmethod eappend ((enumerable sequence) element)
  (with-enumerable
    (loop
      :for i :from 0 :below (length enumerable)
      :do (yield (elt enumerable i))
      :finally (yield element))))

(defmethod consume ((enumerable sequence))
  (values))

(defmethod contains ((enumerable sequence) item &optional (test #'eql))
  (and (position item enumerable :test test) t))

(defmethod ecount ((enumerable sequence))
  (length enumerable))

(defmethod ecount* ((enumerable sequence) predicate)
  (count-if predicate enumerable))

(defmethod default-if-empty ((enumerable sequence) &optional default)
  (if (emptyp enumerable)
      (list default)
      enumerable))

(defmethod evaluate ((functions sequence))
  (with-enumerable
    (loop
      :for i :from 0 :below (length functions)
      :do (yield (funcall (elt functions i))))))

(defmethod elast ((enumerable sequence) &optional default)
  (let ((len (length enumerable)))
    (cond
      ((zerop len) default)
      (t
       (elt enumerable (1- len))))))

(defmethod elast* ((enumerable sequence) predicate &optional default)
  (if-let ((pos (position-if predicate enumerable :from-end t)))
    (elt enumerable pos)
    default))

(defmethod prepend ((enumerable sequence) element)
  (with-enumerable
    (loop
      :initially (yield element)
      :for i :from 0 :below (length enumerable)
      :do (yield (elt enumerable i)))))

(defmethod select ((enumerable sequence) selector)
  (with-enumerable
    (loop
      :for i :from 0 :below (length enumerable)
      :do (yield (funcall selector (elt enumerable i))))))

(defmethod select* ((enumerable sequence) selector)
  (with-enumerable
    (loop
      :for i :from 0 :below (length enumerable)
      :do (yield (funcall selector (elt enumerable i) i)))))

(defmethod select-many ((enumerable sequence) selector &optional (result-selector #'identity))
  (with-enumerable
    (loop
      :for i :from 0 :below (length enumerable)
      :do
         (do-enumerable (sub-elt (funcall selector (elt enumerable i)))
           (yield (funcall result-selector sub-elt))))))

(defmethod select-many* ((enumerable sequence) selector &optional (result-selector #'identity))
  (with-enumerable
    (loop
      :for i :from 0 :below (length enumerable)
      :do
         (do-enumerable (sub-elt (funcall selector (elt enumerable i) i))
           (yield (funcall result-selector sub-elt))))))

(defmethod single ((enumerable sequence) &optional default)
  (let ((len (length enumerable)))
    (cond
      ((> len 1)
       (error "more than one element present in the enumerable"))
      ((= len 1)
       (elt enumerable 0))
      (t
       default))))

(defmethod single* ((enumerable sequence) predicate &optional default)
  (if-let ((pos (position-if predicate enumerable)))
    (if (position-if predicate enumerable :start (1+ pos))
        (error "more than one element present in the enumerable matches predicate")
        (elt enumerable pos))
    default))

(defmethod skip ((enumerable sequence) count)
  (with-enumerable
    (let ((i count)
          (len (length enumerable)))
      (loop
        :while (< i len)
        :do
           (yield (elt enumerable i))
           (incf i)))))

(defmethod skip-last ((enumerable sequence) count)
  (let ((len (length enumerable)))
    (cond
      ((>= count len)
       nil)
      (t
       (with-enumerable
         (loop :for i :from 0 :below (- len count)
               :do (yield (elt enumerable i))))))))

(defmethod skip-until ((enumerable sequence) predicate)
  (with-enumerable
    (when-let ((pos (position-if predicate enumerable)))
      (loop
        :for i :from pos :below (length enumerable)
        :do (yield (elt enumerable i))))))

(defmethod skip-while ((enumerable sequence) predicate)
  (with-enumerable
    (when-let ((pos (position-if-not predicate enumerable)))
      (loop
        :for i :from pos :below (length enumerable)
        :do (yield (elt enumerable i))))))

(defmethod take ((enumerable sequence) count)
  (cond
    ((zerop count)
     nil)
    (t
     (with-enumerable
       (loop
         :for i :from 0 :below (length enumerable)
         :while (< i count)
         :for elt := (elt enumerable i)
         :do (yield elt))))))

(defmethod take-every ((enumerable sequence) step)
  (unless (and (integerp step)
               (plusp step))
    (error "step must be a positive integer, was ~A" step))
  (if (= step 1)
      enumerable
      (with-enumerable
        (loop
          :for i :from 0 :below (length enumerable) :by step
          :do (yield (elt enumerable i))))))

(defmethod take-last ((enumerable sequence) count)
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
               :do (yield (elt enumerable i))))))))

(defmethod take-until ((enumerable sequence) predicate)
  (with-enumerable
    (loop
      :for i :from 0 :below (length enumerable)
      :for elt := (elt enumerable i)
      :until (funcall predicate elt)
      :do (yield elt))))

(defmethod take-while ((enumerable sequence) predicate)
  (with-enumerable
    (loop
      :for i :from 0 :below (length enumerable)
      :for elt := (elt enumerable i)
      :while  (funcall predicate elt)
      :do (yield elt))))

(defmethod to-list ((enumerable sequence))
  (copy-sequence 'list enumerable))

(defmethod to-vector ((enumerable sequence) &key (element-type t) adjustable fill-pointer-p)
  (make-array (length enumerable)
              :element-type element-type
              :initial-contents enumerable
              :adjustable adjustable
              :fill-pointer (and fill-pointer-p t)))
