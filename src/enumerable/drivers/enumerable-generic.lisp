(in-package #:enumerable)

(defun %enumerable->seq (e)
  (if (compute-applicable-methods #'col-seq (list e))
      (col-seq e)
      (labels ((recurse (enumerator)
                 (when (move-next enumerator)
                   (cons (current enumerator)
                         (lazy-seq (recurse enumerator))))))
        (lazy-seq (recurse (get-enumerator e))))))

(defmacro %lazy-enum ((var enumerable) &body body)
  `(lazy-seq
     (let ((,var (get-enumerator ,enumerable)))
       ,@body)))

(defun %enumerator->lazy-seq (enumerator)
  (labels ((recurse ()
             (when (move-next enumerator)
               (cons (current enumerator)
                     (lazy-seq (recurse))))))
    (recurse)))

(defun %enumerator->lazy-seq* (enumerator selector)
  (labels ((recurse ()
             (when (move-next enumerator)
               (cons (funcall selector (current enumerator))
                     (lazy-seq (recurse))))))
    (recurse)))

(defun %enumerable->lazy-seq (enumerable)
  (if (typep enumerable 'clojure-seq:lazy-seq)
      enumerable
      (%enumerator->lazy-seq (get-enumerator enumerable))))

(defmethod aggregate (enumerable aggregator)
  (let (accum
        step)
    (labels ((encounter-first (x)
               (setf accum x
                     step #'encounter-rest))
             (encounter-rest (x)
               (setf accum (funcall aggregator accum x))))
      (setf step #'encounter-first)
      (map-enumerable (lambda (x) (funcall step x)) enumerable)
      (unless (eq step #'encounter-rest)
        (error "enumerable contains no elements.")))
    accum))

(defmethod aggregate* (enumerable aggregator seed)
  (let ((accum seed))
    (map-enumerable (lambda (x)
                      (setf accum (funcall aggregator accum x)))
                    enumerable)
    accum))

(defmethod all (enumerable predicate)
  (not (any* enumerable (complement predicate))))

(defmethod any (enumerable)
  (map-enumerable (lambda (_)
                    (declare (ignore _))
                    (return-from any t))
                  enumerable)
  nil)

(defmethod any* (enumerable predicate)
  (map-enumerable (lambda (x)
                    (when (funcall predicate x)
                      (return-from any* t)))
                  enumerable)
  nil)

(defmethod eappend (enumerable element)
  (%lazy-enum (enumerator enumerable)
    (labels ((recurse ()
               (if (move-next enumerator)
                   (lazy-seq (cons (current enumerator) (recurse)))
                   (cons element nil))))
      (recurse))))


(defmethod batch (enumerable size &key (element-type t) adjustable fill-pointer-p)
  (check-type size (integer 1))
  (%lazy-enum (enumerator enumerable)
    (let ((buf (make-array size :element-type element-type)))
      (labels ((copy-batch ()
                 (loop
                   :for i :below size
                   :while (move-next enumerator)
                   :do (setf (aref buf i) (current enumerator))
                   :finally (return i)))
               (recurse ()
                 (let ((copied (copy-batch)))
                   (cond
                     ((zerop copied)
                      nil)
                     ((= copied size) ; Full batch
                      (lazy-seq
                        (cons
                         (make-array copied :element-type element-type :initial-contents buf :adjustable adjustable :fill-pointer (and fill-pointer-p t))
                         (recurse))))
                     (t ; Partial batch
                      (cons
                       (replace (make-array copied :element-type element-type :adjustable adjustable :fill-pointer (and fill-pointer-p t)) buf)
                       nil))))))
        (recurse)))))

(defmethod concat (first second)
  (labels ((yield-first (enumerator)
             (if (move-next enumerator)
                 (cons (current enumerator)
                       (lazy-seq (yield-first enumerator)))
                 (%enumerable->lazy-seq second))))
    (lazy-seq (yield-first (get-enumerator first)))))

(defmethod consume (enumerable)
  (map-enumerable (lambda (_) (declare (ignore _))) enumerable)
  (values))

(defmethod contains (enumerable item &optional (test #'eql))
  (map-enumerable (lambda (x)
                    (when (funcall test x item)
                      (return-from contains t)))
                  enumerable))

(defmethod ecount (enumerable)
  (let ((count 0))
    (map-enumerable (lambda (_)
                      (declare (ignore _))
                      (incf count))
                    enumerable)
    count))

(defmethod ecount* (enumerable predicate)
  (let ((count 0))
    (map-enumerable (lambda (x)
                      (when (funcall predicate x)
                        (incf count)))
                    enumerable)
    count))

(defmethod default-if-empty (enumerable &optional default)
  (if (any enumerable)
      enumerable
      (list default)))

(defmethod distinct (enumerable &optional (test #'eql))
  (%lazy-enum (enumerator enumerable)
    (let ((known-elements ()))
      (labels ((recurse ()
                 (when (move-next enumerator)
                   (let ((x (current enumerator)))
                     (if (member x known-elements :test test)
                       (recurse)
                       (lazy-seq
                         (push x known-elements)
                         (cons x (recurse))))))))
        (recurse)))))

(defmethod element-at (enumerable index &optional default)
  (efirst (skip enumerable index) default))

(defmethod evaluate (functions)
  (%lazy-enum (enumerator functions)
    (%enumerator->lazy-seq* enumerator #'funcall)))

(defmethod except (first second &optional (test #'eql))
  (%lazy-enum (e1 first)
    (let ((e2 (get-enumerator second))
          (yielded ())
          (encountered-second ()))
      (labels ((search-encountered (elt1)
                 (and (position elt1 encountered-second :test test) t))
               (search-e2 (elt1)
                 (loop
                   :while (move-next e2)
                   :for elt2 := (current e2)
                   :do (pushnew elt2 encountered-second :test test)
                   :if (funcall test elt1 elt2)
                     :return t))
               (recurse ()
                 (when (move-next e1)
                   (let ((elt1 (current e1)))
                     (if (or (search-encountered elt1)
                             (search-e2 elt1)
                             (member elt1 yielded :test test))
                         (recurse)
                         (lazy-seq
                           (push elt1 yielded)
                           (cons elt1 (recurse))))))))
        (recurse)))))

(defmethod efirst (enumerable &optional default)
  (map-enumerable (lambda (x) (return-from efirst x)) enumerable)
  default)

(defmethod efirst* (enumerable predicate &optional default)
  (map-enumerable (lambda (x)
                    (when (funcall predicate x)
                      (return-from efirst* x)))
                  enumerable)
  default)

(defmethod group-adjacent (enumerable key
                           &key
                             (test #'eql)
                             (selector #'identity)
                             (result-selector #'make-grouping))
  (%lazy-enum (enumerator enumerable)
    (labels ((recurse (first-elt group-key)
               (lazy-seq
                 (loop
                   :with group := (list (funcall selector first-elt))
                   :while (move-next enumerator)
                   :for elt := (current enumerator)
                   :for elt-key := (funcall key elt)
                   :if (funcall test group-key elt-key)
                     :do (push (funcall selector elt) group)
                   :else
                     :return (cons (funcall result-selector group-key group)
                                   (recurse elt elt-key))
                   :finally
                      (return (list (funcall result-selector group-key group)))))))
      (when (move-next enumerator)
        (let* ((elt (current enumerator))
               (elt-key (funcall key elt)))
          (recurse elt elt-key))))))

(defmethod group-by (enumerable key
                     &key
                       (test #'eql)
                       (selector #'identity)
                       (result-selector #'make-grouping))
  (lazy-seq
    (let ((groups ()))
      (loop
        :with enumerator := (get-enumerator enumerable)
        :while (move-next enumerator)
        :for elt := (current enumerator)
        :for elt-key := (funcall key elt)
        :for group := (find elt-key groups :key #'car :test test)
        :for result-elt := (funcall selector elt)
        :if group
          :do (push result-elt (cdr group))
        :else
          :do (push (cons elt-key (cons result-elt nil)) groups))
      (labels ((recurse (groups)
                 (when groups
                   (lazy-seq
                     (destructuring-bind (g-key . g-elts) (car groups)
                       (cons (funcall result-selector g-key g-elts)
                             (recurse (cdr groups))))))))
        (recurse groups)))))

(defmethod intersect (first second &optional (test #'eql))
  (%lazy-enum (e1 first)
    (let ((e2 (get-enumerator second))
          (yielded '())
          (encountered-second '()))
      (labels ((search-encountered (elt1)
                 (and (position elt1 encountered-second :test test) t))
               (search-e2 (elt1)
                 (loop
                   :while (move-next e2)
                   :for elt2 := (current e2)
                   :do (pushnew elt2 encountered-second :test test)
                   :if (funcall test elt1 elt2)
                     :return t))
               (recurse ()
                 (when (move-next e1)
                   (let ((elt1 (current e1)))
                     (if (or (not (or (search-encountered elt1)
                                      (search-e2 elt1)))
                             (find elt1 yielded :test test))
                         (recurse)
                         (lazy-seq
                           (push elt1 yielded)
                           (cons elt1 (recurse))))))))
        (recurse)))))

(defmethod elast (enumerable &optional default)
  (efirst (ereverse enumerable) default))

(defmethod elast* (enumerable predicate &optional default)
  (map-enumerable (lambda (x)
                    (when (funcall predicate x)
                      (return-from elast* x)))
                  (ereverse enumerable))
  default)

(defstruct (%ordered-enumerable
            (:conc-name nil)
            (:constructor %make-ordered-enumerable (%ordered-enumerable-lazy-seq %ordered-enumerable-comparer))
            (:copier nil))
  (%ordered-enumerable-lazy-seq (required-argument)
   :type lazy-seq)
  (%ordered-enumerable-comparer (required-argument)
   :type (function (t t) integer)))

(defmethod get-enumerator ((enumerable %ordered-enumerable))
  (get-enumerator
   (labels ((recurse (rest)
              (when-let ((seq (col-seq rest)))
                (lazy-seq
                  (cons (cdr (seq-first seq))
                        (recurse (seq-rest seq)))))))
     (recurse (%ordered-enumerable-lazy-seq enumerable)))))

(defun %order-by-impl (enumerable key-selector comparer ordering)
  (%make-ordered-enumerable
   (lazy-seq
     (let* ((heap (to-vector enumerable))
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

(defmethod order-by (enumerable key-selector &optional (comparer #'-))
  (%order-by-impl enumerable key-selector comparer #'minusp))

(defmethod order-by-descending (enumerable key-selector &optional (comparer #'-))
  (%order-by-impl enumerable key-selector comparer #'plusp))

(defmethod prepend (enumerable element)
  (lazy-seq (cons element (%enumerable->seq enumerable))))

(defmethod ereverse (enumerable)
  (lazy-seq
    (let ((stack ()))
      (map-enumerable (lambda (x) (push x stack)) enumerable)
      stack)))

(defmethod run-length-encode (enumerable &key (test #'eql) limit)
  (%lazy-enum (enumerator enumerable)
    (when (move-next enumerator)
      (labels ((recurse (first-elt)
                 (loop
                   :for count :from 1
                   :while (move-next enumerator)
                   :for elt := (current enumerator)
                   :unless (and (or (null limit)
                                    (< count limit))
                                (funcall test first-elt elt))
                     :return (cons (cons first-elt count)
                                   (lazy-seq (recurse elt)))
                   :finally
                      (return (list (cons first-elt count))))))
        (let ((elt (current enumerator)))
          (recurse elt))))))

(defmethod select (enumerable selector)
  (%lazy-enum (enumerator enumerable)
    (%enumerator->lazy-seq* enumerator selector)))

(defmethod select* (enumerable selector)
  (%lazy-enum (enumerator enumerable)
    (let ((i 0))
      (%enumerator->lazy-seq* enumerator (lambda (x)
                                           (prog1 (funcall selector x i)
                                             (incf i)))))))

(defmethod select-many (enumerable selector &optional (result-selector #'identity))
  (%lazy-enum (enumerator enumerable)
    (labels ((recurse ()
               (when (move-next enumerator)
                 (let ((enumerator (get-enumerator (funcall selector (current enumerator)))))
                   (labels ((inner-recurse ()
                              (if (move-next enumerator)
                                  (cons (funcall result-selector (current enumerator))
                                        (lazy-seq (inner-recurse)))
                                  (recurse))))
                     (inner-recurse))))))
      (recurse))))

(defmethod select-many* (enumerable selector &optional (result-selector #'identity))
  (%lazy-enum (enumerator enumerable)
    (labels ((recurse (i)
               (when (move-next enumerator)
                 (let ((enumerator (get-enumerator (funcall selector (current enumerator) i))))
                   (labels ((inner-recurse ()
                              (if (move-next enumerator)
                                  (cons (funcall result-selector (current enumerator))
                                        (lazy-seq (inner-recurse)))
                                  (recurse (1+ i)))))
                     (inner-recurse))))))
      (recurse 0))))

(defmethod single (enumerable &optional default)
  (let ((found-value nil)
        (ret default))
    (map-enumerable (lambda (x)
                      (when found-value
                        (error "more than one element present in the enumerable"))
                      (setf ret x
                            found-value t))
                    enumerable)
    ret))

(defmethod single* (enumerable predicate &optional default)
  (let ((found-value nil)
        (ret default))
    (map-enumerable (lambda (x)
                      (when (funcall predicate x)
                        (when found-value
                          (error "more than one element present in the enumerable"))
                        (setf ret x
                              found-value t)))
                    enumerable)
    ret))

(defmethod skip (enumerable count)
  (if (<= count 0)
      enumerable
      (%lazy-enum (enumerator enumerable)
        (loop
          :for i :from 1
          :while (move-next enumerator)
          :if (= i count)
            :return (%enumerator->lazy-seq enumerator)))))

(defmethod skip-last (enumerable count)
  (if (<= count 0)
      enumerable
      (lazy-seq
        (let* ((enumerator (get-enumerator enumerable))
               (queue
                 (loop
                   :for i :below count
                   :while (move-next enumerator)
                   :collect (current enumerator)))
               (tail (last queue)))
          (labels ((recurse ()
                     (when (move-next enumerator)
                       (setf (cdr tail) (cons (current enumerator) nil)
                             tail (cdr tail))
                       (cons (pop queue)
                             (lazy-seq (recurse))))))
            (recurse))))))

(defmethod skip-until (enumerable predicate)
  (%lazy-enum (enumerator enumerable)
    (loop
      :while (move-next enumerator)
      :for x := (current enumerator)
      :when (funcall predicate x)
        :return (cons x (%enumerator->lazy-seq enumerator)))))

(defmethod skip-while (enumerable predicate)
  (%lazy-enum (enumerator enumerable)
    (loop :while (move-next enumerator)
          :for x := (current enumerator)
          :unless (funcall predicate x)
            :return (cons x (%enumerator->lazy-seq enumerator)))))

(defmethod take (enumerable count)
  (unless (<= count 0)
    (%lazy-enum (enumerator enumerable)
      (labels ((recurse (i)
                 (when (and (< i count)
                            (move-next enumerator))
                   (cons (current enumerator)
                         (lazy-seq (recurse (1+ i)))))))
        (recurse 0)))))

(defmethod take-every (enumerable step)
  (unless (and (integerp step)
               (plusp step))
    (error "step must be a positive integer, was ~A" step))
  (%lazy-enum (enumerator enumerable)
    (labels ((recurse ()
               (when (loop :repeat step
                           :always (move-next enumerator))
                 (lazy-seq
                   (cons (current enumerator)
                         (recurse))))))
      (when (move-next enumerator)
        (cons (current enumerator)
              (recurse))))))

(defmethod take-last (enumerable count)
  (when (minusp count)
    (error "count cannot be negative, was ~A" count))
  (cond
    ((zerop count) nil)
    (t (ereverse (take (ereverse enumerable) count)))))

(defmethod take-until (enumerable predicate)
  (%lazy-enum (enumerator enumerable)
    (labels ((recurse ()
               (when (move-next enumerator)
                 (let ((elt (current enumerator)))
                   (unless (funcall predicate elt)
                     (cons elt (lazy-seq (recurse))))))))
      (recurse))))

(defmethod take-while (enumerable predicate)
  (%lazy-enum (enumerator enumerable)
    (labels ((recurse ()
               (when (move-next enumerator)
                 (let ((elt (current enumerator)))
                   (when (funcall predicate elt)
                     (cons elt (lazy-seq (recurse))))))))
      (recurse))))

(defun %then-by-impl (enumerable key-selector comparer ordering)
  (select-many (group-adjacent (%ordered-enumerable-lazy-seq enumerable) #'car
                               :test (let ((comparer (%ordered-enumerable-comparer enumerable))) (lambda (a b) (zerop (funcall comparer a b))))
                               :selector #'cdr)
               (lambda (group)
                 (%order-by-impl group key-selector comparer ordering))))

(defmethod then-by (ordered-enumerable key-selector &optional (comparer #'-))
  (%then-by-impl ordered-enumerable key-selector comparer #'minusp))

(defmethod then-by-ascending (ordered-enumerable key-selector &optional (comparer #'-))
  (%then-by-impl ordered-enumerable key-selector comparer #'plusp))

(defmethod eunion (first second &optional (test #'eql))
  (distinct (concat first second) test))

(defmethod where (enumerable predicate)
  (%lazy-enum (enumerator enumerable)
    (labels ((recurse ()
               (when (move-next enumerator)
                 (let ((elt (current enumerator)))
                   (if (funcall predicate elt)
                       (cons elt (lazy-seq (recurse)))
                       (recurse))))))
      (recurse))))

(defmethod window (enumerable size &key (element-type t) adjustable fill-pointer-p)
  (%lazy-enum (enumerator enumerable)
    (let ((buf (make-array size :element-type element-type :adjustable adjustable :fill-pointer (and fill-pointer-p t))))
      (labels ((recurse (prev-window)
                 (when (move-next enumerator)
                   (let ((window (make-array size :element-type element-type :adjustable adjustable :fill-pointer (and fill-pointer-p t))))
                     (replace window prev-window :start2 1)
                     (setf (aref window (1- size)) (current enumerator))
                     (cons window (lazy-seq (recurse window)))))))
        (when (loop
                :for i :below size
                :always (move-next enumerator)
                :do (setf (aref buf i) (current enumerator)))
          (cons buf
                (lazy-seq (recurse buf))))))))

(defmethod to-hash-table (enumerable key &key (selector #'identity) (test #'eql))
  (let ((ret (make-hash-table :test test)))
    (map-enumerable (lambda (x) (setf (gethash (funcall key x) ret) (funcall selector x))) enumerable)
    ret))

(defmethod to-list (enumerable)
  (let ((res ()))
    (map-enumerable (lambda (x) (push x res)) enumerable)
    (nreverse res)))

(defmethod to-vector (enumerable &key (element-type t) adjustable fill-pointer-p)
  (let ((res ())
        (len 0))
    (map-enumerable (lambda (x)
                      (push x res)
                      (incf len))
                    enumerable)
    (make-array len
                :element-type element-type
                :initial-contents (nreverse res)
                :adjustable adjustable
                :fill-pointer (and fill-pointer-p t))))
