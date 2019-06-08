(in-package #:enumerable)

(deftype enumerable ()
  "A type which can be enumerated."
  '(satisfies enumerablep))

(defgeneric map-enumerable (fn enumerable)
  (:documentation
   "Apply `fn' to every element in `enumerable'.")
  (:method (fn (enumerable null))
    (values)))

(defgeneric get-enumerator (enumerable)
  (:documentation
   "Create an `enumerator' for `enumerable'.")
  (:method ((enumerable null))
    nil))

(defgeneric current (enumerator)
  (:documentation
   "Retrieve the current element of `enumerator'.")
  (:method ((enumerator null))
    (values)))

(defgeneric move-next (enumerator)
  (:documentation
   "Moves `enumerator' to the next element.
Returns `t' if `enumerator' is valid after it's moved, `nil' otherwise.")
  (:method ((enumerator null))
    nil))
