(in-package #:enumerable)

(defmethod get-enumerator ((enumerable package))
  (get-enumerator
   (let ((res ()))
     (do-symbols (s enumerable)
       (push s res))
     res)))
