(in-package #:clojure-seq)

(defgeneric col-seq (col)
  (:documentation
   "Returns a `seq' on the collection `col'")
  (:method ((col list))
    col))

(defgeneric seq-first (seq)
  (:documentation
   "Returns the first element of `seq'")
  (:method ((col list))
    (car col)))

(defgeneric seq-rest (seq)
  (:documentation
   "Returns the rest of the elements of `seq'")
  (:method ((col list))
    (cdr col)))
