(in-package #:enumerable)

(defun %make-collapsed-displaced-vector (vec offset count)
  (check-type offset array-index)
  (check-type count array-length)
  (labels ((recurse (vec offset count)
             (check-type vec vector)
             (multiple-value-bind (displaced-to displaced-offset)
                 (array-displacement vec)
               (cond
                 (displaced-to
                  (recurse displaced-to (+ offset displaced-offset) count))
                 ((= count (length vec))
                  vec)
                 (t
                  (make-array count :element-type (array-element-type vec) :displaced-to vec :displaced-index-offset offset))))))
    (recurse vec offset count)))

(defmethod map-enumerable (fn (enumerable vector))
  (loop :for x :across enumerable
        :do (funcall fn x))
  (values))

(defstruct (%vector-enumerator
            (:conc-name nil)
            (:constructor %make-vector-enumerator (%vector-enumerator-vector))
            (:copier nil))
  (%vector-enumerator-vector (required-argument '%vector-enumerator-vector)
   :type vector
   :read-only t)
  (%vector-enumerator-position -1
   :type (integer -1 #.array-dimension-limit)))

(defmethod get-enumerator ((enumerable vector))
  (%make-vector-enumerator enumerable))

(defmethod current ((enumerator %vector-enumerator))
  (unless (or (= -1 (%vector-enumerator-position enumerator))
              (= (%vector-enumerator-position enumerator) (length (%vector-enumerator-vector enumerator))))
    (aref (%vector-enumerator-vector enumerator) (%vector-enumerator-position enumerator))))

(defmethod move-next ((enumerator %vector-enumerator))
  (when (< (%vector-enumerator-position enumerator)
           (length (%vector-enumerator-vector enumerator)))
    (incf (%vector-enumerator-position enumerator))
    (/= (%vector-enumerator-position enumerator)
        (length (%vector-enumerator-vector enumerator)))))

(defmethod any* ((enumerable vector) predicate)
  (loop
    :for i :from 0 :below (length enumerable)
    :if (funcall predicate (aref enumerable i))
      :return t
    :finally (return nil)))

(defmethod batch ((enumerable vector) size &key (element-type (array-element-type enumerable)) adjustable fill-pointer-p)
  (labels ((recurse (pos e-len)
             (when (< pos e-len)
               (let ((len (min size (- e-len pos))))
                 (lazy-seq
                   (cons
                    (replace (make-array len :element-type element-type :adjustable adjustable :fill-pointer (and fill-pointer-p t))
                             enumerable
                             :start2 pos)
                    (recurse (+ pos size) e-len)))))))
    (lazy-seq (recurse 0 (length enumerable)))))

(defmethod consume ((enumerable vector))
  (values))

(defmethod contains ((enumerable vector) item &optional (test #'eql))
  (loop
    :for i :from 0 :below (length enumerable)
    :if (funcall test item (aref enumerable i))
      :return t
    :finally (return nil)))

(defmethod element-at ((enumerable vector) index &optional default)
  (if (< index (length enumerable))
      (aref enumerable index)
      default))

(defmethod elast ((enumerable vector) &optional default)
  (let ((len (length enumerable)))
    (cond
      ((zerop len) default)
      (t (aref enumerable (1- len))))))

(defmethod elast* ((enumerable vector) predicate &optional default)
  (loop
    :for i :from (1- (length enumerable)) :downto 0
    :for elt := (aref enumerable i)
    :if (funcall predicate elt)
      :return elt
    :finally (return default)))

(defmethod ereverse ((enumerable vector))
  (labels ((recurse (i)
             (when (>= i 0)
               (lazy-seq
                 (cons (aref enumerable i)
                       (recurse (1- i)))))))
    (recurse (1- (length enumerable)))))

(defmethod single ((enumerable vector) &optional default)
  (let ((len (length enumerable)))
    (cond
      ((> len 1) (error "more than one element present in the enumerable"))
      ((= len 1) (aref enumerable 0))
      (t default))))

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
  (let* ((len (length enumerable))
         (remaining (- len count)))
    (cond
      ((<= remaining 0) nil)
      ((>= remaining len) enumerable)
      (t (%make-collapsed-displaced-vector enumerable count remaining)))))

(defmethod skip-last ((enumerable vector) count)
  (let ((len (length enumerable)))
    (cond
      ((>= count len) nil)
      (t (%make-collapsed-displaced-vector enumerable 0 (min len (- len count)))))))

(defmethod skip-until ((enumerable vector) predicate)
  (lazy-seq
    (loop
      :with len := (length enumerable)
      :for i :below len
      :if (funcall predicate (aref enumerable i))
        :return (%make-collapsed-displaced-vector enumerable i (- len i)))))

(defmethod skip-while ((enumerable vector) predicate)
  (lazy-seq
    (loop
      :with len := (length enumerable)
      :for i :below len
      :if (not (funcall predicate (aref enumerable i)))
        :return (%make-collapsed-displaced-vector enumerable i (- len i)))))

(defmethod take ((enumerable vector) count)
  (let* ((len (length enumerable))
         (to-take (max 0 (min len count))))
    (cond
      ((zerop to-take) nil)
      ((= to-take len) enumerable)
      (t (%make-collapsed-displaced-vector enumerable 0 count)))))

(defmethod take-last ((enumerable vector) count)
  (when (minusp count)
    (error "count cannot be negative, was ~A" count))
  (let ((len (length enumerable)))
    (cond
      ((>= count len) enumerable)
      ((zerop count) nil)
      (t (%make-collapsed-displaced-vector enumerable (- len count) count)))))

(defmethod window ((enumerable vector) size &key (element-type (array-element-type enumerable)) adjustable fill-pointer-p)
  (cond
    ((< (length enumerable) size)
     nil)
    ((= (length enumerable) size)
     (list (make-array size :initial-contents enumerable
                            :element-type element-type
                            :adjustable adjustable
                            :fill-pointer (and fill-pointer-p t))))
    (t
     (labels ((recurse (i end-idx)
                (unless (> i end-idx)
                  (lazy-seq
                    (cons
                     (replace (make-array size :element-type element-type :adjustable adjustable :fill-pointer (and fill-pointer-p t))
                              enumerable
                              :start2 i)
                     (recurse (1+ i) end-idx))))))
       (recurse 0 (- (length enumerable) size))))))

(defmethod to-vector ((enumerable vector) &key (element-type (array-element-type enumerable)) adjustable fill-pointer-p)
  (make-array (length enumerable)
              :element-type element-type
              :initial-contents enumerable
              :adjustable adjustable
              :fill-pointer (and fill-pointer-p t)))
