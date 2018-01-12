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

(defgeneric aggregate (enumerable aggregator)
  (:documentation
   "Applies a `aggregator' on `enumerable'.
`aggregator' is a function of two arguments: The accumulated value, and the next element.
  It should return the new accumulated value.
Note: The first accumulated value is the first element of `enumerable'.
      If `enumerable' contains only one element, `aggregator' will not be invoked.
Signals an error if `enumerable' is empty."))

(defgeneric aggregate* (enumerable aggregator seed)
  (:documentation
   "Applies a `aggregator' on `enumerable', initially with `seed'.
`aggregator' is a function of two arguments: The accumulated value, and the next element.
  It should return the new accumulated value.
`seed' is the initial accumulated value.
Note: If `enumerable' contains no elements, `aggregator' will not be invoked, and
      `seed' will be returned."))

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

(defgeneric concat (first second)
  (:documentation
   "Concatenates the `enumerable's `first' and `second'."))

(defgeneric consume (enumerable)
  (:documentation
   "Completely consumes the given sequence."))

(defgeneric contains (enumerable item &optional test)
  (:documentation
   "Returns `t' if `enumerable' contains `item', by applying `test'.
`test' defaults to `eql'."))

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

(defgeneric evaluate (functions)
  (:documentation
   "Returns a sequence containing the values resulting from invoking each function in `functions'."))

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

(defgeneric single (enumerable &optional default)
  (:documentation
   "Returns the only element of `enumerable', or `default' if `enumerable' is empty.
If `enumerable' has more than one element, an error is signalled instead."))

(defgeneric single* (enumerable predicate &optional default)
  (:documentation
   "Returns the only element of `enumerable' that satisfies `predicate', or `default' if no such element exists.
If more than one element matches `predicate', an error is signalled instead."))

(defgeneric skip (enumerable count)
  (:documentation
   "Skips the first `count' elements of `enumerable'."))

(defgeneric skip-last (enumerable count)
  (:documentation
   "Skips the last `count' elements of `enumerable'."))

(defgeneric skip-until (enumerable predicate)
  (:documentation
   "Skips elements in `enumerable' to the first matching `predicate'"))

(defgeneric skip-while (enumerable predicate)
  (:documentation
   "Skips elements in `enumerable' while they match `predicate'."))

(defgeneric take (enumerable count)
  (:documentation
   "Takes the first `count' elements in `enumerable'."))

(defgeneric take-every (enumerable step)
  (:documentation
   "Returns every nth element of a sequence."))

(defgeneric take-last (enumerable count)
  (:documentation
   "Takes the last `count' elements in `enumerable'."))

(defgeneric take-until (enumerable predicate)
  (:documentation
   "Takes elements from `enumerable' until they match `predicate'."))

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
