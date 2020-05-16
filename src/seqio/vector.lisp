(in-package #:com.inuoe.seqio)

;; Optimizations for vectors since they allow fast random access

(defun %make-collapsed-displaced-vector (vec offset count)
  (check-type offset array-index)
  (check-type count array-length)
  (labels ((recurse (vec offset count)
             (check-type vec vector)
             (multiple-value-bind (displaced-to displaced-offset) (array-displacement vec)
               (cond
                 (displaced-to
                  (recurse displaced-to (+ offset displaced-offset) count))
                 ((= count (length vec))
                  vec)
                 (t
                  (make-array count :element-type (array-element-type vec) :displaced-to vec :displaced-index-offset offset))))))
    (recurse vec offset count)))

(defmethod any* ((col vector) predicate)
  (loop
    :for i :from 0 :below (length col)
    :if (funcall predicate (aref col i))
      :return t
    :finally (return nil)))

(defmethod batch ((col vector) size &key (element-type (array-element-type col)) adjustable fill-pointer-p)
  (labels ((recurse (pos e-len)
             (when (< pos e-len)
               (let ((len (min size (- e-len pos))))
                 (lazy-seq
                   (cons
                    (replace (make-array len :element-type element-type :adjustable adjustable :fill-pointer (and fill-pointer-p t))
                             col
                             :start2 pos)
                    (recurse (+ pos size) e-len)))))))
    (lazy-seq (recurse 0 (length col)))))

(defmethod consume ((col vector))
  (values))

(defmethod contains ((col vector) item &optional (test #'eql))
  (loop
    :for i :from 0 :below (length col)
    :if (funcall test item (aref col i))
      :return t
    :finally (return nil)))

(defmethod element-at ((col vector) index &optional default)
  (if (< index (length col))
      (aref col index)
      default))

(defmethod elast ((col vector) &optional default)
  (let ((len (length col)))
    (cond
      ((zerop len) default)
      (t (aref col (1- len))))))

(defmethod elast* ((col vector) predicate &optional default)
  (loop
    :for i :from (1- (length col)) :downto 0
    :for elt := (aref col i)
    :if (funcall predicate elt)
      :return elt
    :finally (return default)))

(defmethod pad ((col vector) width &optional padding)
  (check-type width (integer 0))
  (let ((len (length col)))
    (if (<= width len)
        col
        (labels ((yield-col (i)
                   (if (< i len)
                       (cons (aref col i)
                             (lazy-seq (yield-col (1+ i))))
                       (yield-padding i)))
                 (yield-padding (i)
                   (when (< i width)
                     (cons padding (lazy-seq (yield-padding (1+ i)))))))
          (lazy-seq (yield-col 0))))))

(defmethod pad* ((col vector) width &optional padding-selector
                 &aux (padding-selector (or padding-selector #'identity)))
  (check-type width (integer 0))
  (let ((len (length col)))
    (if (<= width len)
        col
        (labels ((yield-col (i)
                   (if (< i len)
                       (cons (aref col i)
                             (lazy-seq (yield-col (1+ i))))
                       (yield-padding i)))
                 (yield-padding (i)
                   (when (< i width)
                     (cons (funcall padding-selector i)
                           (lazy-seq (yield-padding (1+ i)))))))
          (lazy-seq (yield-col 0))))))

(defmethod ereverse ((col vector))
  (labels ((recurse (i)
             (when (>= i 0)
               (lazy-seq
                 (cons (aref col i)
                       (recurse (1- i)))))))
    (recurse (1- (length col)))))

(defmethod single ((col vector) &optional default)
  (let ((len (length col)))
    (cond
      ((> len 1) (error "more than one element present in the col"))
      ((= len 1) (aref col 0))
      (t default))))

(defmethod single* ((col vector) predicate &optional default)
  (loop
    :with found-value := nil
    :with ret := default
    :for elt :across col
    :if (funcall predicate elt)
      :do (if found-value
              (error "more than one element present in the col matches predicate")
              (setf found-value t
                    ret elt))
    :finally (return ret)))

(defmethod skip ((col vector) count)
  (let* ((len (length col))
         (remaining (- len count)))
    (cond
      ((<= remaining 0) nil)
      ((>= remaining len) col)
      (t (%make-collapsed-displaced-vector col count remaining)))))

(defmethod skip-last ((col vector) count)
  (let ((len (length col)))
    (cond
      ((>= count len) nil)
      (t (%make-collapsed-displaced-vector col 0 (min len (- len count)))))))

(defmethod skip-until ((col vector) predicate)
  (lazy-seq
    (loop
      :with len := (length col)
      :for i :below len
      :if (funcall predicate (aref col i))
        :return (%make-collapsed-displaced-vector col i (- len i)))))

(defmethod skip-while ((col vector) predicate)
  (lazy-seq
    (loop
      :with len := (length col)
      :for i :below len
      :if (not (funcall predicate (aref col i)))
        :return (%make-collapsed-displaced-vector col i (- len i)))))

(defmethod take ((col vector) count)
  (let* ((len (length col))
         (to-take (max 0 (min len count))))
    (cond
      ((zerop to-take) nil)
      ((= to-take len) col)
      (t (%make-collapsed-displaced-vector col 0 count)))))

(defmethod take-last ((col vector) count)
  (when (minusp count)
    (error "count cannot be negative, was ~A" count))
  (let ((len (length col)))
    (cond
      ((>= count len) col)
      ((zerop count) nil)
      (t (%make-collapsed-displaced-vector col (- len count) count)))))

(defmethod window ((col vector) size &key (element-type (array-element-type col)) adjustable fill-pointer-p)
  (cond
    ((< (length col) size)
     nil)
    ((= (length col) size)
     (list (make-array size :initial-contents col
                            :element-type element-type
                            :adjustable adjustable
                            :fill-pointer (and fill-pointer-p t))))
    (t
     (labels ((recurse (i end-idx)
                (unless (> i end-idx)
                  (lazy-seq
                    (cons
                     (replace (make-array size :element-type element-type
                                               :adjustable adjustable
                                               :fill-pointer (and fill-pointer-p t))
                              col
                              :start2 i)
                     (recurse (1+ i) end-idx))))))
       (recurse 0 (- (length col) size))))))

(defmethod to-vector ((col vector) &key (element-type (array-element-type col)) adjustable fill-pointer-p)
  (make-array (length col)
              :element-type element-type
              :initial-contents col
              :adjustable adjustable
              :fill-pointer (and fill-pointer-p t)))
