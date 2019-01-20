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

(defmethod map-enumerable (fn (enumerable hash-table))
  (flet ((kv-fcall (k v)
           (funcall fn (cons k v))))
    (declare (dynamic-extent #'kv-fcall))
    (maphash #'kv-fcall enumerable)))

(defmethod get-enumerator ((enumerable hash-table))
  ;;with-hash-table-iterator has unspecified behavior outside of dynamic extent
  ;;so we can't just close it over
  (get-enumerator (hash-table-alist enumerable)))
