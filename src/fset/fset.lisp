(in-package #:enumerable.fset)

(defstruct (%collection-enumerator
            (:conc-name nil)
            (:constructor %make-collection-enumerator (%collection-enumerator-iterator))
            (:copier nil))
  (%collection-enumerator-iterator (required-argument '%collection-enumerator-iterator)
   :type function
   :read-only t)
  (%collection-enumerator-current nil
   :type t))

(defmethod get-enumerator ((enumerable fset:collection))
  (%make-collection-enumerator (fset:iterator enumerable)))

(defmethod current ((enumerator %collection-enumerator))
  (%collection-enumerator-current enumerator))

(defmethod move-next ((enumerator %collection-enumerator))
  (multiple-value-bind (value more?)
      (funcall (%collection-enumerator-iterator enumerator) :get)
    (setf (%collection-enumerator-current enumerator) value)
    more?))

(defstruct (%map-enumerator
            (:conc-name nil)
            (:constructor %make-map-enumerator (%map-enumerator-iterator))
            (:copier nil))
  (%map-enumerator-iterator (required-argument '%map-enumerator-iterator)
   :type function
   :read-only t)
  (%map-enumerator-current nil
   :type t))

(defmethod get-enumerator ((enumerable fset:map))
  (%make-map-enumerator (fset:iterator enumerable)))

(defmethod current ((enumerator %map-enumerator))
  (%map-enumerator-current enumerator))

(defmethod move-next ((enumerator %map-enumerator))
  (multiple-value-bind (key value more?)
      (funcall (%map-enumerator-iterator enumerator) :get)
    (setf (%map-enumerator-current enumerator) (cons key value))
    more?))
