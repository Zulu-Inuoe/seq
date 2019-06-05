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

(defmethod map-enumerable (fn (enumerable stream))
  (cond
    ((subtypep (stream-element-type enumerable) 'integer)
     (loop :for x := (read-byte enumerable nil)
           :while x
           :do (funcall fn x)))
    ((subtypep (stream-element-type enumerable) 'character)
     (loop :for x := (read-char enumerable nil)
           :while x
           :do (funcall fn x))))
  (values))

(defstruct (%stream-int-enumerator
            (:conc-name nil)
            (:constructor %make-stream-int-enumerator (%stream-int-enumerator-stream))
            (:copier nil))
  (%stream-int-enumerator-stream (required-argument '%stream-int-enumerator-stream)
   :type stream
   :read-only t)
  (%stream-int-enumerator-current nil
   :type (or null integer)))

(defmethod current ((enumerator %stream-int-enumerator))
  (%stream-int-enumerator-current enumerator))

(defmethod move-next ((enumerator %stream-int-enumerator))
  (and (setf (%stream-int-enumerator-current enumerator)
             (read-byte (%stream-int-enumerator-stream enumerator) nil nil))
       t))

(defstruct (%stream-char-enumerator
            (:conc-name nil)
            (:constructor %make-stream-char-enumerator (%stream-char-enumerator-stream))
            (:copier nil))
  (%stream-char-enumerator-stream (required-argument '%stream-char-enumerator-stream)
   :type stream
   :read-only t)
  (%stream-char-enumerator-current nil
   :type (or null character)))

(defmethod current ((enumerator %stream-char-enumerator))
  (%stream-char-enumerator-current enumerator))

(defmethod move-next ((enumerator %stream-char-enumerator))
  (and (setf (%stream-char-enumerator-current enumerator)
             (read-char (%stream-char-enumerator-stream enumerator) nil nil))
       t))

(defmethod get-enumerator ((enumerable stream))
  (cond
    ((subtypep (stream-element-type enumerable) 'integer)
     (%make-stream-int-enumerator enumerable))
    ((subtypep (stream-element-type enumerable) 'character)
     (%make-stream-char-enumerator enumerable))))
