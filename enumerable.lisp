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

(defgeneric map-enumerable (fn enumerable)
  (:documentation
   "Apply `fn' to every element in `enumerable'."))

(defgeneric get-enumerator (enumerable)
  (:documentation
   "Create an `enumerator' for `enumerable'."))

(defgeneric current (enumerator)
  (:documentation
   "Retrieve the current element of `enumerator'."))

(defgeneric move-next (enumerator)
  (:documentation
   "Moves `enumerator' to the next element.
Returns `t' if `enumerator' is valid after it's moved, `nil' otherwise."))

(defgeneric any (enumerable)
  (:documentation
   "Returns `t' if there are any elements in `enumerable'. `nil' otherwise."))

(defgeneric any* (enumerable predicate)
  (:documentation
   "Returns `t' if any element in `enumerable' satisfies `predicate'. `nil' otherwise."))

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

(defgeneric select (enumerable selector)
  (:documentation
   "Maps each element of `enumerable' to a new `enumerable' using `selector'.
`selector' is a function of one argument: the element."))

(defgeneric select* (enumerable selector)
  (:documentation
   "Maps each element of `enumerable' to a new `enumerable' using `selector'.
`selector' is a function of two arguments: the element and its index."))

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

(defgeneric to-vector (enumerable)
  (:documentation
   "Creates a a vector from the elements in `enumerable'."))

(defmacro enumerable (&body body)
  (with-gensyms (cont)
    `(make-instance
      'continuation-enumerable
      :starter
      (lambda ()
        (let (,cont)
          (lambda ()
            (if ,cont
                (funcall ,cont)
                (cl-cont:with-call/cc
                  (labels ((yield (result)
                             (cl-cont:let/cc cc
                               (setf ,cont cc)
                               (values result t)))
                           (yield-break ()
                             (cl-cont:let/cc cc
                               (declare (ignore cc))
                               (setf ,cont (lambda () (values nil nil)))
                               (values nil nil))))
                    ,@body
                    (yield-break))))))))))

(defmacro lambdae (args &body body)
  `(lambda ,args
     (enumerable ,@body)))

(defmacro defenumerable (name args &body body)
  `(defun ,name ,args
     (enumerable ,@body)))
