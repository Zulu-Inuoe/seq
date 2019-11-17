(in-package #:clojure-seq)

(defun %make-collapsed-displaced-vector (vec offset count)
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

(defgeneric col-seq (col)
  (:documentation
   "Returns a `seq' on the collection `col'")
  (:method ((col list))
    col)
  (:method ((col vector))
    (unless (zerop (length col))
      col)))

(defgeneric seq-first (seq)
  (:documentation
   "Returns the first element of `seq'")
  (:method ((col list))
    (car col))
  (:method ((col vector))
    (unless (zerop (length col))
      (aref col 0))))

(defgeneric seq-rest (seq)
  (:documentation
   "Returns the rest of the elements of `seq'")
  (:method ((col list))
    (cdr col))
  (:method ((col vector))
    (let ((len (length col)))
      (when (> len 1)
        (%make-collapsed-displaced-vector col 1 (1- len))))))
