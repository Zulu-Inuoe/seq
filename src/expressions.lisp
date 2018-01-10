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

(defgeneric all (enumerable predicate)
  (:documentation
   "Returns `t' if all elements in `enumerable' satisfy `predicate'."))

(defgeneric any (enumerable)
  (:documentation
   "Returns `t' if there are any elements in `enumerable'. `nil' otherwise."))

(defgeneric any* (enumerable predicate)
  (:documentation
   "Returns `t' if any element in `enumerable' satisfies `predicate'. `nil' otherwise."))

(defgeneric eappend (enumerable element)
  (:documentation
   "Appends `element' to the end of the `enumerable'."))

(defgeneric contains (enumerable item &optional test)
  (:documentation
   "Returns `t' if `enumerable' contains `item', by applying `test'.
`test' defaults to `eql'."))

(defgeneric concat (first second)
  (:documentation
   "Concatenates the `enumerable's `first' and `second'."))

(defgeneric ecount (enumerable)
  (:documentation
   "Count the number of elements in `enumerable'."))

(defgeneric ecount* (enumerable predicate)
  (:documentation
   "Count the number of elements in `enumerable' that satisfy `predicate'"))

(defgeneric default-if-empty (enumerable &optional default)
  (:documentation
   "Returns the elements of `enumerable', or an enumerable with `default' if it is empty."))

(defgeneric distinct (enumerable &optional test)
  (:documentation
   "Returns distinct elements from `enumerable' by using `test'.
`test' defaults to `eql'."))

(defgeneric element-at (enumerable index &optional default)
  (:documentation
   "Returns the element in `enumerable' at the specified `index', or `default' if such an element does not exist."))

(defun empty ()
  "Returns an empty `enumerable'."
  nil)

(defgeneric except (first second &optional test)
  (:documentation
   "Produces the set difference between `first' and `second' by using `test'.
`test' defaults to `eql'"))

(defgeneric efirst (enumerable &optional default)
  (:documentation
   "Return the first element in in `enumerable', or `default' if no element is available."))

(defgeneric efirst* (enumerable predicate &optional default)
  (:documentation
   "Returns the first element in `enumerable' that satisfies `predicate', or `default' if no such element exists."))

(defgeneric elast (enumerable &optional default)
  (:documentation
   "Returns the last element in `enumerable', or `default' if no element is available."))

(defgeneric elast* (enumerable predicate &optional default)
  (:documentation
   "Returns the last element in `enumerable' that satusfies `predicate', or `default' if no such element exists."))

(defgeneric prepend (enumerable element)
  (:documentation
   "Adds `element' to the beginning of `enumerable'."))

(defun range (start count)
  "Generates `count' integers starting at `count'."
  (with-enumerable
    (loop :for i :from start :by 1
          :repeat count
          :do (yield i))))

(defun repeat (value count)
  "Generates an enumerable that repeats `value' `count' times."
  (with-enumerable
    (loop :repeat count
          :do (yield value))))

(defgeneric select (enumerable selector)
  (:documentation
   "Maps each element of `enumerable' to a new `enumerable' using `selector'.
`selector' is a function of one argument: the element."))

(defgeneric select* (enumerable selector)
  (:documentation
   "Maps each element of `enumerable' to a new `enumerable' using `selector'.
`selector' is a function of two arguments: the element and its index."))

(defgeneric select-many (enumerable selector &optional result-selector)
  (:documentation
   "Maps each element of `enumerable' using `selector'. `selector' should produce an `enumerable' for each element.
These sub-sequences are flattened, and each element of the resulting sequence is mapped by `result-selector'.
`selector' is a function of one argument: the element.
`result-selector' defaults to `identity'."))

(defgeneric select-many* (enumerable selector &optional result-selector)
  (:documentation
   "Maps each element of `enumerable' using `selector'. `selector' should produce an `enumerable' for each element.
These sub-sequences are flattened, and each element of the resulting sequence is mapped by `result-selector'.
`selector' is a function of two arguments: the element and its index.
`result-selector' defaults to `identity'."))

(defgeneric skip (enumerable count)
  (:documentation
   "Skips the first `count' elements of `enumerable'."))

(defgeneric skip-while (enumerable predicate)
  (:documentation
   "Skips elements in `enumerable' while they match `predicate'."))

(defgeneric take (enumerable count)
  (:documentation
   "Takes the first `count' elements in `enumerable'."))

(defgeneric take-while (enumerable predicate)
  (:documentation
   "Takes elements from `enumerable' while they match `predicate'."))

(defgeneric where (enumerable predicate)
  (:documentation
   "Filters `enumerable' to elements that match `predicate'."))

(defgeneric to-list (enumerable)
  (:documentation
   "Creates a list from the elements of `enumerable'."))

(defgeneric to-vector (enumerable &key element-type adjustable fill-pointer-p)
  (:documentation
   "Creates a a vector from the elements in `enumerable'.
`element-type' - as `make-array'
`adjustable' - as `make-array'
`fill-pointer-p' - a generalized boolean. if true, the resulting vector shall
                   have a fill pointer initialized to the size of the sequence."))
