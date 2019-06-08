(in-package #:enumerable)

(defmethod map-enumerable (fn (enumerable hash-table))
  (flet ((kv-fcall (k v)
           (funcall fn (cons k v))))
    (declare (dynamic-extent #'kv-fcall))
    (maphash #'kv-fcall enumerable)))

(defmethod get-enumerator ((enumerable hash-table))
  ;;with-hash-table-iterator has unspecified behavior outside of dynamic extent
  ;;so we can't just close it over
  (get-enumerator (hash-table-alist enumerable)))
