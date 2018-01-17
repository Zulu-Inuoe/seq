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

(deftype enumerable ()
  "A type which can be enumerated."
  '(satisfies enumerablep))

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
