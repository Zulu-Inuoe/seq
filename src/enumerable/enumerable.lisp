(in-package #:enumerable)

(deftype enumerable ()
  "A type which can be enumerated."
  '(satisfies enumerablep))

(defgeneric enumerablep (x)
  (:method (x)
    (and (compute-applicable-methods #'get-enumerator (list x)) t))
  (:method ((obj sequence)) t)
  (:method ((obj package)) t)
  (:method ((obj hash-table)) t)
  (:method ((obj stream)) t))

(defgeneric get-enumerator (enumerable)
  (:documentation
   "Create an `enumerator' for `enumerable'.")
  (:method ((enumerable null))
    nil))

(defgeneric map-enumerable (fn enumerable)
  (:documentation
   "Apply `fn' to every element in `enumerable'.")
  (:method (fn enumerable)
    (loop :with enumerator := (get-enumerator enumerable)
          :while (move-next enumerator)
          :for x := (current enumerator)
          :do (funcall fn x)))
  (:method (fn (enumerable null))
    (values)))

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
