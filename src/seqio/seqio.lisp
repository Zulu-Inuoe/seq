(in-package #:com.inuoe.seqio)

(defgeneric aggregate (col aggregator)
  (:documentation
   "Applies a `aggregator' on `col'.
`aggregator' is a function of two arguments: The accumulated value, and the next element.
  It should return the new accumulated value.
Note: The first accumulated value is the first element of `col'.
      If `col' contains only one element, `aggregator' will not be invoked.
Signals an error if `col' is empty."))

(defgeneric aggregate* (col aggregator seed)
  (:documentation
   "Applies a `aggregator' on `col', initially with `seed'.
`aggregator' is a function of two arguments: The accumulated value, and the next element.
  It should return the new accumulated value.
`seed' is the initial accumulated value.
Note: If `col' contains no elements, `aggregator' will not be invoked, and
      `seed' will be returned."))

(defgeneric all (col predicate)
  (:documentation
   "Returns `t' if all elements in `col' satisfy `predicate'."))

(defgeneric any (col)
  (:documentation
   "Returns `t' if there are any elements in `col'. `nil' otherwise."))

(defgeneric any* (col predicate)
  (:documentation
   "Returns `t' if any element in `col' satisfies `predicate'. `nil' otherwise."))

(defgeneric eappend (col element)
  (:documentation
   "Appends `element' to the end of the `col'."))

(defgeneric batch (col size &key element-type adjustable fill-pointer-p)
  (:documentation
   "Generates a `col' with batched subsequences of `col' of size `size'.
Each subsequence is a fresh `vector' of size [1,size].
`element-type' - as `make-array'
`adjustable' - as `make-array'
`fill-pointer-p' - a generalized boolean. if true, the resulting vector shall
                   have a fill pointer initialized to the size of the window."))

(defun cartesian (fn &rest cols)
  (when cols
    (lazy-seq
      (labels ((concat (a b)
                 (if-let ((seq (col-seq a)))
                   (lazy-seq
                     (cons (seq-first seq)
                           (concat (seq-rest a) b)))
                   b))
               (recurse (cols cell args)
                 (if-let ((rest (rest cols)))
                   (labels ((inner-recurse (col)
                              (when-let ((seq (col-seq col)))
                                (setf (car cell) (seq-first seq))
                                (concat
                                 (lazy-seq (recurse rest (cdr cell) args))
                                 (lazy-seq (inner-recurse (seq-rest seq)))))))
                     (inner-recurse (first cols)))
                   (labels ((yielding-recurse (col)
                              (when-let ((seq (col-seq col)))
                                (setf (car cell) (seq-first seq))
                                (cons (apply fn args)
                                      (lazy-seq (yielding-recurse (seq-rest seq)))))))
                     (yielding-recurse (first cols))))))
        (let ((args ()))
          (dolist (_ cols)
            (declare (ignore _))
            (push nil args))
          (recurse cols args args))))))

(defgeneric concat (first second)
  (:documentation
   "Concatenates the `col's `first' and `second'."))

(defgeneric consume (col)
  (:documentation
   "Completely consumes the given sequence."))

(defgeneric contains (col item &optional test)
  (:documentation
   "Returns `t' if `col' contains `item', by applying `test'.
`test' defaults to `eql'."))

(defgeneric ecount (col)
  (:documentation
   "Count the number of elements in `col'."))

(defgeneric ecount* (col predicate)
  (:documentation
   "Count the number of elements in `col' that satisfy `predicate'"))

(defgeneric default-if-empty (col &optional default)
  (:documentation
   "Returns the elements of `col', or an col with `default' if it is empty."))

(defgeneric distinct (col &optional test)
  (:documentation
   "Returns distinct elements from `col' by using `test'.
`test' defaults to `eql'."))

(defgeneric element-at (col index &optional default)
  (:documentation
   "Returns the element in `col' at the specified `index', or `default' if such an element does not exist."))

(defun empty ()
  "Returns an empty `col'."
  nil)

(defgeneric evaluate (functions)
  (:documentation
   "Returns a sequence containing the values resulting from invoking each function in `functions'."))

(defgeneric except (first second &optional test)
  (:documentation
   "Produces the set difference between `first' and `second' by using `test'.
`test' defaults to `eql'"))

(defgeneric efirst (col &optional default)
  (:documentation
   "Return the first element in in `col', or `default' if no element is available."))

(defgeneric efirst* (col predicate &optional default)
  (:documentation
   "Returns the first element in `col' that satisfies `predicate', or `default' if no such element exists."))

(defgeneric group-adjacent (col key &key test selector result-selector)
  (:documentation
   "Groups adjacent elements in `col' by `key'.
`test' will be used to test keys for equality. defaults to `eql'
`selector' a function of one argument: each element
`result-selector' a function of two arguments: the group key, and a `col' of its members."))

(defgeneric group-by (col key &key test selector result-selector)
  (:documentation
   "Groups elements in `col' by `key'.
`test' will be used to test keys for equality. defaults to `eql'
`selector' a function of one argument: each element
`result-selector' a function of two arguments: the group key, and a `col' of its members."))

(defgeneric intersect (first second &optional test)
  (:documentation
   "Generates an col that is the set-intersection of `first' and `second'
`test' will be used to test for equality. defaults to `eql'"))

(defgeneric elast (col &optional default)
  (:documentation
   "Returns the last element in `col', or `default' if no element is available."))

(defgeneric elast* (col predicate &optional default)
  (:documentation
   "Returns the last element in `col' that satusfies `predicate', or `default' if no such element exists."))

(defgeneric order-by (col key-selector &optional comparer)
  (:documentation
   "Return the `col' in ascending sorted order.
`key-selector' will be used to determine value for each element.
`comparer' is a function receiving keys a and b,returning a negative value if a is less than b, 0 if equal, and positive if greater."))

(defgeneric order-by-descending (col key-selector &optional comparer)
  (:documentation
   "Return the `col' in descending sorted order.
`key-selector' will be used to determine value for each element.
`comparer' is a function receiving keys a and b,returning a negative value if a is less than b, 0 if equal, and positive if greater."))

(defgeneric pad (col width &optional padding)
  (:documentation
   "Pads `col' with `padding' if it contains less than `width' elements."))

(defgeneric pad* (col width &optional padding-selector)
  (:documentation
   "Pads `col' with if it contains less than `width' elements by calling `padding-selector' with the index of the element.

 `padding-selector' - A function designator, called with the index of the 'missing' element."))

(defgeneric prepend (col element)
  (:documentation
   "Adds `element' to the beginning of `col'."))

(defun range (&optional start count step)
  "Generates `count' integers starting at `range'."
  (let* ((step (or step 1))
         (start (or start 0))
         (start (- (+ start step) step)))
    (if count
        (labels ((recurse (i x)
                   (when (< i count)
                     (cons x (lazy-seq (recurse (1+ i) (+ x step)))))))
          (lazy-seq (recurse 0 start)))
        (labels ((recurse (x)
                   (cons x (lazy-seq (recurse (+ x step))))))
          (lazy-seq (recurse start))))))

(defun repeat (value count)
  "Generates an col that repeats `value' `count' times."
  (labels ((recurse (i)
             (when (< i count)
               (lazy-seq (cons value (recurse (1+ i)))))))
    (recurse 0)))

(defgeneric ereverse (col)
  (:documentation
   "Produce `col' in reversed order."))

(defun repeatedly (f &optional limit)
  (if limit
      (labels ((recurse (i)
                 (when (> i 0)
                   (lazy-seq (cons (funcall f) (recurse (1- i)))))))
        (recurse limit))
      (labels ((recurse ()
                 (lazy-seq (cons (funcall f) (recurse)))))
        (recurse))))

(defgeneric run-length-encode (col &key test limit)
  (:documentation
   "Run-length encode `col' as a collection of (elt . count) conses of successive elements.
 `test' defaults to `eql'
 `limit' when non-nil, serves as the inclusive upper-limit of `count'"))

(defgeneric scan (col transformer)
  (:documentation
   "As `aggregate', but lazily yields the collection (possibly empty) of each intermediate value."))

(defgeneric scan* (col transformer seed)
  (:documentation
   "As `aggregate*', but lazily yields the collection (including initial seed) of each intermediate values."))

(defgeneric select (col selector)
  (:documentation
   "Maps each element of `col' to a new `col' using `selector'.
`selector' is a function of one argument: the element."))

(defgeneric select* (col selector)
  (:documentation
   "Maps each element of `col' to a new `col' using `selector'.
`selector' is a function of two arguments: the element and its index."))

(defgeneric select-many (col selector &optional result-selector)
  (:documentation
   "Maps each element of `col' using `selector'. `selector' should produce a `col' for each element.
These sub-sequences are flattened, and each element of the resulting sequence is mapped by `result-selector'.
`selector' is a function of one argument: the element.
`result-selector' defaults to `identity'."))

(defgeneric select-many* (col selector &optional result-selector)
  (:documentation
   "Maps each element of `col' using `selector'. `selector' should produce a `col' for each element.
These sub-sequences are flattened, and each element of the resulting sequence is mapped by `result-selector'.
`selector' is a function of two arguments: the element and its index.
`result-selector' defaults to `identity'."))

(defgeneric single (col &optional default)
  (:documentation
   "Returns the only element of `col', or `default' if `col' is empty.
If `col' has more than one element, an error is signalled instead."))

(defgeneric single* (col predicate &optional default)
  (:documentation
   "Returns the only element of `col' that satisfies `predicate', or `default' if no such element exists.
If more than one element matches `predicate', an error is signalled instead."))

(defgeneric skip (col count)
  (:documentation
   "Skips the first `count' elements of `col'."))

(defgeneric skip-last (col count)
  (:documentation
   "Skips the last `count' elements of `col'."))

(defgeneric skip-until (col predicate)
  (:documentation
   "Skips elements in `col' to the first matching `predicate'"))

(defgeneric skip-while (col predicate)
  (:documentation
   "Skips elements in `col' while they match `predicate'."))

(defgeneric take (col count)
  (:documentation
   "Takes the first `count' elements in `col'."))

(defgeneric take-every (col step)
  (:documentation
   "Returns every nth element of a sequence."))

(defgeneric take-last (col count)
  (:documentation
   "Takes the last `count' elements in `col'."))

(defgeneric take-until (col predicate)
  (:documentation
   "Takes elements from `col' until they match `predicate'."))

(defgeneric take-while (col predicate)
  (:documentation
   "Takes elements from `col' while they match `predicate'."))

(defgeneric then-by (col key-selector &optional comparer)
  (:documentation
   "Performs a subsequent ordering on `col'Return the `col' in ascending sorted order.
`key-selector' will be used to determine value for each element.
`comparer' is a function receiving keys a and b,returning a negative value if a is less than b, 0 if equal, and positive if greater."))

(defun unfold (state generator
               &key predicate state-selector result-selector
               &aux
                 (predicate (or predicate (constantly t)))
                 (state-selector (or state-selector #'identity))
                 (result-selector (or result-selector #'identity)))
  "Produces a sequence by applying `generator' to `state'.
 `predicate' - applied to the result to control termination
 `result-selector' - applied to the result to yield it
 `state-selector' - applied to the result to determine the new state"
  (labels ((recurse (state)
             (let ((step (funcall generator state)))
               (when (funcall predicate step)
                 (cons (funcall result-selector step)
                       (lazy-seq (recurse (funcall state-selector step))))))))
    (lazy-seq (recurse state))))

(defgeneric eunion (first second &optional test)
  (:documentation
   "Produces the set union between `first' and `second' by using `test'.
`test' defaults to `eql'"))

(defgeneric where (col predicate)
  (:documentation
   "Filters `col' to elements that match `predicate'."))

(defgeneric window (col size &key element-type adjustable fill-pointer-p)
  (:documentation
   "Generates a `col' with sliding-window subsequences of `col' of size `size'.
The window is a constant size `size'.
Will yield no windows if `col' does not have at least `size' elements.
`element-type' - as `make-array'
`adjustable' - as `make-array'
`fill-pointer-p' - a generalized boolean. if true, the resulting vector shall
                   have a fill pointer initialized to the size of the window."))

(defgeneric to-hash-table (col key &key selector test)
  (:documentation
   "Creates a `hash-table' from the elements of `col', utilizing `key'  to generate keys for the elements.
`selector' - a function of one argument to transform each element
`test' - as `make-hash-table'"))

(defgeneric to-list (col)
  (:documentation
   "Creates a `list' from the elements of `col'."))

(defgeneric to-vector (col &key element-type adjustable fill-pointer-p)
  (:documentation
   "Creates a a `vector' from the elements in `col'.
`element-type' - as `make-array'
`adjustable' - as `make-array'
`fill-pointer-p' - a generalized boolean. if true, the resulting vector shall
                   have a fill pointer initialized to the size of the col."))
(defun zip (fn &rest cols)
  (lazy-seq
    (labels ((recurse (cols)
               (loop
                 :for cell :on cols
                 :for seq := (col-seq (car cell))
                 :if (null seq)
                   :do (return-from recurse nil)
                 :do (setf (car cell) seq))
               (cons
                (apply fn (mapcar #'seq-first cols))
                (lazy-seq
                  (loop
                    :for cell :on cols
                    :do (setf (car cell) (seq-rest (car cell))))
                  (recurse cols)))))
      (recurse (copy-list cols)))))