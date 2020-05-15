(in-package #:com.inuoe.seqio)

(defmethod aggregate (col aggregator)
  (let (accum step)
    (labels ((encounter-first (x)
               (setf accum x
                     step #'encounter-rest))
             (encounter-rest (x)
               (setf accum (funcall aggregator accum x))))
      (setf step #'encounter-first)
      (mapcol (lambda (x) (funcall step x)) col)
      (unless (eq step #'encounter-rest)
        (error "col contains no elements.")))
    accum))

(defmethod aggregate* (col aggregator seed)
  (let ((accum seed))
    (mapcol (lambda (x)  (setf accum (funcall aggregator accum x)))
            col)
    accum))

(defmethod all (col predicate)
  (not (any* col (complement predicate))))

(defmethod any (col)
  (mapcol (lambda (_)
            (declare (ignore _))
            (return-from any t))
          col)
  nil)

(defmethod any* (col predicate)
  (mapcol (lambda (x)
            (when (funcall predicate x)
              (return-from any* t)))
          col)
  nil)

(defmethod eappend (col element)
  (labels ((recurse (col)
             (lazy-seq
               (if-let ((seq (col-seq col)))
                 (cons (seq-first seq) (recurse (seq-rest seq)))
                 (cons element nil)))))
    (recurse col)))

(defmethod batch (col size &key (element-type t) adjustable fill-pointer-p)
  (check-type size (integer 1))
  (let ((buf (make-array size :element-type element-type)))
    (labels ((copy-batch (col)
               (loop
                 :for i :below size
                 :for seq := (col-seq col) :then (col-seq (seq-rest seq))
                 :when (null seq)
                   :do (loop-finish)
                 :do (setf (aref buf i) (seq-first seq))
                 :finally (return (values i (seq-rest seq)))))
             (recurse (col)
               (lazy-seq
                 (multiple-value-bind (copied col) (copy-batch col)
                   (cond
                     ((zerop copied)
                      nil)
                     ((= copied size) ; Full batch
                      (cons
                       (make-array copied :element-type element-type :initial-contents buf :adjustable adjustable :fill-pointer (and fill-pointer-p t))
                       (recurse col)))
                     (t ; Partial batch
                      (cons
                       (replace (make-array copied :element-type element-type :adjustable adjustable :fill-pointer (and fill-pointer-p t)) buf)
                       nil)))))))
      (recurse col))))

(defmethod concat (first second)
  (labels ((yield-first (col)
             (lazy-seq
               (if-let ((seq (col-seq col)))
                 (cons (seq-first seq) (yield-first (seq-rest seq)))
                 second))))
    (yield-first first)))

(defmethod consume (col)
  (mapcol (lambda (_) (declare (ignore _))) col)
  (values))

(defmethod contains (col item &optional (test #'eql))
  (mapcol (lambda (x)
            (when (funcall test x item)
              (return-from contains t)))
          col))

(defmethod ecount (col)
  (let ((count 0))
    (mapcol (lambda (_)
              (declare (ignore _))
              (incf count))
            col)
    count))

(defmethod ecount* (col predicate)
  (let ((count 0))
    (mapcol (lambda (x)
              (when (funcall predicate x)
                (incf count)))
            col)
    count))

(defmethod default-if-empty (col &optional default)
  (if (any col)
      col
      (list default)))

(defmethod distinct (col &optional (test #'eql))
  (let ((known-elements ()))
    (labels ((recurse (col)
               (if-let ((seq (col-seq col)))
                 (let ((x (seq-first seq)))
                   (cond
                     ((member x known-elements :test test)
                      (recurse (seq-rest seq)))
                     (t
                      (push x known-elements)
                      (cons x (lazy-seq (recurse (seq-rest seq))))))))))
      (lazy-seq (recurse col)))))

(defmethod element-at (col index &optional default)
  (efirst (skip col index) default))

(defmethod evaluate (functions)
  (labels ((recurse (col)
             (when-let ((seq (col-seq col)))
               (cons (funcall (seq-first seq))
                     (lazy-seq (recurse (seq-rest seq)))))))
    (lazy-seq (recurse functions))))

(defmethod except (first second &optional (test #'eql))
  (let ((yielded ())
        (encountered-second ()))
    (labels ((search-encountered (elt1)
               (and (position elt1 encountered-second :test test) t))
             (search-e2 (elt1 col2)
               (if-let ((seq (col-seq col2)))
                 (let ((elt2 (seq-first seq)))
                   (pushnew elt2 encountered-second :test test)
                   (if (funcall test elt1 elt2)
                       (values t seq)
                       (search-e2 elt1 (seq-rest seq))))
                 (values nil nil)))
             (recurse (col1 col2)
               (when-let ((seq (col-seq col1)))
                 (let ((elt1 (seq-first seq)))
                   (if (or (search-encountered elt1)
                           (multiple-value-bind (foundp seq) (search-e2 elt1 col2)
                             (setf col2 seq)
                             foundp)
                           (member elt1 yielded :test test))
                       (recurse (seq-rest seq) col2)
                       (lazy-seq
                         (push elt1 yielded)
                         (cons elt1 (lazy-seq (recurse (seq-rest seq) col2)))))))))
      (lazy-seq (recurse first second)))))

(defmethod efirst (col &optional default)
  (mapcol (lambda (x) (return-from efirst x)) col)
  default)

(defmethod efirst* (col predicate &optional default)
  (mapcol (lambda (x)
            (when (funcall predicate x)
              (return-from efirst* x)))
          col)
  default)

(defstruct (grouping-col
            (:conc-name nil)
            (:constructor make-grouping-col (grouping-col-key grouping-col-seq))
            (:copier nil))
  (grouping-col-key (required-argument)
   :type t
   :read-only t)
  (grouping-col-seq (required-argument)
   :type t
   :read-only t))

(defmethod print-object ((object grouping-col) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~S ~S" (grouping-col-key object) (grouping-col-seq object))))

(defmethod col-seq ((col grouping-col))
  (grouping-col-seq col))

(defmethod group-adjacent (col key
                           &key
                             (test #'eql)
                             (selector #'identity)
                             (result-selector #'make-grouping-col))
  (labels ((recurse (col group-key group)
             (if-let ((seq (col-seq col)))
               (let* ((elt (seq-first seq))
                      (elt-key (funcall key elt)))
                 (if (funcall test group-key elt-key)
                     (recurse (seq-rest seq) group-key (cons (funcall selector elt) group))
                     (cons (funcall result-selector group-key group)
                           (lazy-seq (recurse (seq-rest seq) elt-key (list (funcall selector elt)))))))
               (list (funcall result-selector group-key group)))))
    (lazy-seq
      (when-let ((seq (col-seq col)))
        (let* ((elt (seq-first seq))
               (elt-key (funcall key elt)))
          (recurse (seq-rest seq) elt-key (list (funcall selector elt))))))))

(defmethod group-by (col key
                     &key
                       (test #'eql)
                       (selector #'identity)
                       (result-selector #'make-grouping-col))
  (lazy-seq
    (let ((groups ()))
      (labels ((form-groups (col)
                 (when-let ((seq (col-seq col)))
                   (let* ((elt (seq-first seq))
                          (elt-key (funcall key elt))
                          (group (find elt-key groups :key #'car :test test)))
                     (unless group
                       (setf group (cons elt-key nil))
                       (push group groups))
                     (push (funcall selector elt) (cdr group))
                     (form-groups (seq-rest seq))))))
        (form-groups col))
      (labels ((yield-groups (groups)
                 (when groups
                   (destructuring-bind (g-key . g-elts) (car groups)
                     (cons (funcall result-selector g-key g-elts)
                           (lazy-seq (yield-groups (cdr groups))))))))
        (yield-groups groups)))))

(defmethod intersect (first second &optional (test #'eql))
  (let ((yielded '())
        (encountered-second '()))
    (labels ((search-encountered (elt1)
               (and (position elt1 encountered-second :test test) t))
             (search-e2 (elt1 col2)
               (if-let ((seq (col-seq col2)))
                 (let ((elt2 (seq-first seq)))
                   (pushnew elt2 encountered-second :test test)
                   (if (funcall test elt1 elt2)
                       (values t seq)
                       (search-e2 elt1 (seq-rest seq))))
                 (values nil nil)))
             (recurse (col1 col2)
               (when-let ((seq (col-seq col1)))
                 (let ((elt1 (seq-first seq)))
                   (if (or (not (or (search-encountered elt1)
                                    (multiple-value-bind (foundp seq) (search-e2 elt1 col2)
                                      (setf col2 seq)
                                      foundp)))
                           (find elt1 yielded :test test))
                       (recurse (seq-rest seq) col2)
                       (lazy-seq
                         (push elt1 yielded)
                         (cons elt1 (recurse (seq-rest seq) col2))))))))
      (recurse first second))))

(defmethod elast (col &optional default)
  (efirst (ereverse col) default))

(defmethod elast* (col predicate &optional default)
  (mapcol (lambda (x)
            (when (funcall predicate x)
              (return-from elast* x)))
          (ereverse col))
  default)

(defstruct (ordered-col
            (:conc-name nil)
            (:constructor make-ordered-col (ordered-col-seq ordered-col-comparer))
            (:copier nil))
  (ordered-col-seq (required-argument)
   :type lazy-seq)
  (ordered-col-comparer (required-argument)
   :type (function (t t) integer)))

(defmethod col-seq ((obj ordered-col))
  (labels ((recurse (col)
             (when-let ((seq (col-seq col)))
               (cons (cdr (seq-first seq))
                     (lazy-seq (recurse (seq-rest seq)))))))
    (recurse (ordered-col-seq obj))))

(defun %order-by-impl (col key-selector comparer ordering)
  (make-ordered-col
   (lazy-seq
     (let* ((heap (to-vector col))
            (len (length heap))
            (keys (map 'vector key-selector heap)))
       (labels ((elt-less (a b)
                  (funcall ordering (funcall comparer (aref keys a) (aref keys b))))
                (min-child (left)
                  (if (= left (1- len))
                      left
                      (let ((right (1+ left)))
                        (if (elt-less left right)
                            left
                            right))))
                (percolate-down (parent)
                  (let ((left (1+ (ash parent 1))))
                    (when (< left len)
                      (let ((child (min-child left)))
                        (when (elt-less child parent)
                          (rotatef (aref heap parent) (aref heap child))
                          (rotatef (aref keys parent) (aref keys child))
                          (percolate-down child))))))
                (recurse ()
                  (when (> len 0)
                    (let ((elt (aref heap 0))
                          (key (aref keys 0)))
                      (decf len)
                      (shiftf (aref heap 0) (aref heap len) nil)
                      (shiftf (aref keys 0) (aref keys len) nil)
                      (percolate-down 0)
                      (cons (cons key elt) (lazy-seq (recurse)))))))
         ;; Heapify
         (percolate-down 0)

         ;; Start yielding elements
         (recurse))))
   comparer))

(defmethod order-by (col key-selector &optional (comparer #'-))
  (%order-by-impl col key-selector comparer #'minusp))

(defmethod order-by-descending (col key-selector &optional (comparer #'-))
  (%order-by-impl col key-selector comparer #'plusp))

(defmethod prepend (col element)
  (cons element col))

(defmethod ereverse (col)
  (lazy-seq
    (let ((stack ()))
      (mapcol (lambda (x) (push x stack)) col)
      stack)))

(defmethod run-length-encode (col &key (test #'eql) limit)
  (labels ((recurse (first-elt col)
             (loop
               :for count :from 1
               :for seq := (col-seq col) :then (col-seq (seq-rest seq))
               :do
                  (when (null seq)
                    (return (list (cons first-elt count))))

                  (let ((elt (seq-first seq)))
                    (unless (and (or (null limit)
                                     (< count limit))
                                 (funcall test first-elt elt))
                      (return
                        (cons (cons first-elt count)
                              (lazy-seq (recurse elt (seq-rest seq))))))))))
    (lazy-seq
      (when-let ((seq (col-seq col)))
        (recurse (seq-first seq) (seq-rest seq))))))

(defmethod select (col selector)
  (labels ((recurse (col)
             (when-let ((seq (col-seq col)))
               (cons (funcall selector (seq-first seq))
                     (lazy-seq (recurse (seq-rest seq)))))))
    (recurse col)))

(defmethod select* (col selector)
  (labels ((recurse (col i)
             (when-let ((seq (col-seq col)))
               (cons (funcall selector (seq-first seq) i)
                     (lazy-seq (recurse (seq-rest seq) (1+ i)))))))
    (recurse col 0)))

(defmethod select-many (col selector &optional (result-selector #'identity))
  (labels ((recurse (col)
             (when-let ((seq (col-seq col)))
               (splice-elts (funcall selector (seq-first seq))
                            (seq-rest seq))))
           (splice-elts (sub-col col)
             (if-let ((seq (col-seq sub-col)))
               (cons (funcall result-selector (seq-first seq))
                     (lazy-seq (splice-elts (seq-rest seq) col)))
               (recurse col))))
    (lazy-seq
      (recurse col))))

(defmethod select-many* (col selector &optional (result-selector #'identity))
  (labels ((recurse (col i)
             (when-let ((seq (col-seq col)))
               (splice-elts (funcall selector (seq-first seq) i)
                            (1+ i)
                            (seq-rest seq))))
           (splice-elts (sub-col i col)
             (if-let ((seq (col-seq sub-col)))
               (cons (funcall result-selector (seq-first seq))
                     (lazy-seq (splice-elts (seq-rest seq) i col)))
               (recurse col i))))
    (lazy-seq
      (recurse col 0))))

(defmethod single (col &optional default)
  (let ((found-value nil)
        (ret default))
    (mapcol (lambda (x)
              (when found-value
                (error "more than one element present in the col"))
              (setf ret x
                    found-value t))
            col)
    ret))

(defmethod single* (col predicate &optional default)
  (let ((found-value nil)
        (ret default))
    (mapcol (lambda (x)
              (when (funcall predicate x)
                (when found-value
                  (error "more than one element present in the col"))
                (setf ret x
                      found-value t)))
            col)
    ret))

(defmethod skip (col count)
  (if (<= count 0)
      col
      (lazy-seq
        (loop
          :for i :from 1
          :for seq := (col-seq col) :then (col-seq (seq-rest seq))
          :if (null seq)
            :return nil
          :if (= i count)
            :return (seq-rest seq)))))

(defmethod skip-last (col count)
  (if (<= count 0)
      col
      (lazy-seq
        (let* ((col col)
               (queue
                 (loop
                   :for i :below count
                   :for seq := (col-seq col) :then (col-seq (seq-rest seq))
                   :when  (null seq)
                     :do (loop-finish)
                   :collect (seq-first seq)
                   :finally (setf col (seq-rest seq))))
               (tail (last queue)))
          (labels ((recurse (col)
                     (when-let ((seq (col-seq col)))
                       (setf (cdr tail) (cons (seq-first seq) nil)
                             tail (cdr tail))
                       (cons (pop queue)
                             (lazy-seq (recurse (seq-rest col)))))))
            (recurse col))))))

(defmethod skip-until (col predicate)
  (labels ((recurse (col)
             (when-let ((seq (col-seq col)))
               (let ((x (seq-first seq)))
                 (if (funcall predicate x)
                     (cons x (seq-rest seq))
                     (recurse (seq-rest seq)))))))
    (lazy-seq (recurse col))))

(defmethod skip-while (col predicate)
  (labels ((recurse (col)
             (when-let ((seq (col-seq col)))
               (let ((x (seq-first seq)))
                 (if (funcall predicate x)
                     (recurse (seq-rest seq))
                     (cons x (seq-rest seq)))))))
    (lazy-seq (recurse col))))

(defmethod take (col count)
  (unless (<= count 0)
    (labels ((recurse (col i)
               (when-let ((seq (and (< i count)
                                    (col-seq col))))
                 (cons (seq-first seq)
                       (lazy-seq (recurse (seq-rest seq) (1+ i)))))))
      (lazy-seq (recurse col 0)))))

(defmethod take-every (col step)
  (check-type step (integer 1))
  (if (= step 1)
      col
      (labels ((recurse (col i)
                 (when-let ((seq (col-seq col)))
                   (if (= i 1)
                       (cons (seq-first seq)
                             (lazy-seq (recurse (seq-rest seq) step)))
                       (recurse (seq-rest seq) (1- i))))))
        (lazy-seq
          (when-let ((seq (col-seq col)))
            (cons (seq-first seq)
                  (lazy-seq (recurse (seq-rest seq) step))))))))

(defmethod take-last (col count)
  (check-type count (integer 0))
  (cond
    ((zerop count) nil)
    (t (ereverse (take (ereverse col) count)))))

(defmethod take-until (col predicate)
  (labels ((recurse (col)
             (when-let ((seq (col-seq col)))
               (let ((elt (seq-first seq)))
                 (unless (funcall predicate elt)
                   (cons elt (lazy-seq (recurse (seq-rest seq)))))))))
    (lazy-seq (recurse col))))

(defmethod take-while (col predicate)
  (labels ((recurse (col)
             (when-let ((seq (col-seq col)))
               (let ((elt (seq-first seq)))
                 (when (funcall predicate elt)
                   (cons elt (lazy-seq (recurse (seq-rest seq)))))))))
    (recurse col)))

(defun %then-by-impl (col key-selector comparer ordering)
  (select-many (group-adjacent (ordered-col-seq col) #'car
                               :test (let ((comparer (ordered-col-comparer col))) (lambda (a b) (zerop (funcall comparer a b))))
                               :selector #'cdr)
               (lambda (group)
                 (%order-by-impl group key-selector comparer ordering))))

(defmethod then-by (ordered-col key-selector &optional (comparer #'-))
  (%then-by-impl ordered-col key-selector comparer #'minusp))

(defmethod then-by-ascending (ordered-col key-selector &optional (comparer #'-))
  (%then-by-impl ordered-col key-selector comparer #'plusp))

(defmethod eunion (first second &optional (test #'eql))
  (distinct (concat first second) test))

(defmethod where (col predicate)
  (labels ((recurse (col)
             (when-let ((seq (col-seq col)))
               (let ((elt (seq-first seq)))
                 (if (funcall predicate elt)
                     (cons elt (lazy-seq (recurse (seq-rest seq))))
                     (recurse (seq-rest seq)))))))
    (lazy-seq (recurse col))))

(defmethod window (col size &key (element-type t) adjustable fill-pointer-p)
  (labels ((recurse (col prev-window)
             (when-let ((seq (col-seq col)))
               (let ((window (make-array size :element-type element-type :adjustable adjustable :fill-pointer (and fill-pointer-p t))))
                 (replace window prev-window :start2 1)
                 (setf (aref window (1- size)) (seq-first seq))
                 (cons window (lazy-seq (recurse (seq-rest seq) window)))))))
    (let ((buf (make-array size :element-type element-type :adjustable adjustable :fill-pointer (and fill-pointer-p t))))
      ;; Set up the very first window
      (labels ((first-window (col i)
                 (if (< i size)
                     (when-let ((seq (col-seq col)))
                       (setf (aref buf i) (seq-first seq))
                       (first-window (seq-rest col) (1+ i)))
                     (cons buf (lazy-seq (recurse col buf))))))
        (lazy-seq (first-window col 0))))))

(defmethod to-hash-table (col key &key (selector #'identity) (test #'eql))
  (let ((ret (make-hash-table :test test)))
    (mapcol (lambda (x) (setf (gethash (funcall key x) ret) (funcall selector x))) col)
    ret))

(defmethod to-list (col)
  (let ((res ()))
    (mapcol (lambda (x) (push x res)) col)
    (nreverse res)))

(defmethod to-vector (col &key (element-type t) adjustable fill-pointer-p)
  (let ((res ())
        (len 0))
    (mapcol (lambda (x)
              (push x res)
              (incf len))
            col)
    (make-array len
                :element-type element-type
                :initial-contents (nreverse res)
                :adjustable adjustable
                :fill-pointer (and fill-pointer-p t))))

