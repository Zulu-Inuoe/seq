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
    (elt (%sequence-enumerator-sequence enumerator) (%sequence-enumerator-position enumerator))))

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

(defmethod elast ((enumerable sequence) &optional default)
  (let ((len (length enumerable)))
    (cond
      ((zerop len) default)
      (t (elt enumerable (1- len))))))

(defmethod elast* ((enumerable sequence) predicate &optional default)
  (if-let ((pos (position-if predicate enumerable :from-end t)))
    (elt enumerable pos)
    default))

(defmethod single ((enumerable sequence) &optional default)
  (let ((len (length enumerable)))
    (cond
      ((> len 1) (error "more than one element present in the enumerable"))
      ((= len 1) (elt enumerable 0))
      (t default))))

(defmethod single* ((enumerable sequence) predicate &optional default)
  (if-let ((pos (position-if predicate enumerable)))
    (if (and (< pos (1- (length enumerable)))
             (position-if predicate enumerable :start (1+ pos)))
        (error "more than one element present in the enumerable matches predicate")
        (elt enumerable pos))
    default))

(defmethod skip-last ((enumerable sequence) count)
  (let ((len (length enumerable)))
    (cond
      ((>= count len) nil)
      ((<= count 0) enumerable)
      (t
       (let ((end (- len count)))
         (labels ((recurse (i)
                    (when (< i end)
                      (lazy-seq
                        (cons (elt enumerable i)
                              (recurse (1+ i)))))))
           (recurse 0)))))))

(defmethod take ((enumerable sequence) count)
  (cond
    ((<= count 0) nil)
    ((>= count (length enumerable)) enumerable)
    (t (call-next-method))))

(defmethod take-every ((enumerable sequence) step)
  (unless (and (integerp step)
               (plusp step))
    (error "step must be a positive integer, was ~A" step))
  (if (= step 1)
      enumerable
      (let ((len (length enumerable)))
        (labels ((recurse (i)
                   (when (< i len)
                     (lazy-seq
                       (cons (elt enumerable i)
                             (recurse (+ i step)))))))
          (recurse 0)))))

(defmethod take-last ((enumerable sequence) count)
  (when (minusp count)
    (error "count cannot be negative, was ~A" count))
  (unless (zerop count)
    (let* ((len (length enumerable)))
      (if (>= count len)
          enumerable
        (let ((start (- len count)))
          (labels ((recurse (i)
                     (when (< i count)
                       (lazy-seq
                         (cons (elt enumerable (+ start i))
                               (recurse (1+ i)))))))
            (recurse 0)))))))

(defmethod to-list ((enumerable sequence))
  (copy-sequence 'list enumerable))

(defmethod to-vector ((enumerable sequence) &key (element-type t) adjustable fill-pointer-p)
  (make-array (length enumerable)
              :element-type element-type
              :initial-contents enumerable
              :adjustable adjustable
              :fill-pointer (and fill-pointer-p t)))
