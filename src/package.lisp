;;;enumerable - enumerable implementation for CL, using cl-cont
;;;Written in 2018 by Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>
;;;
;;;To the extent possible under law, the author(s) have dedicated all copyright
;;;and related and neighboring rights to this software to the public domain
;;;worldwide. This software is distributed without any warranty.
;;;You should have received a copy of the CC0 Public Domain Dedication along
;;;with this software. If not, see
;;;<http://creativecommons.org/publicdomain/zero/1.0/>.

(defpackage #:enumerable
  (:use #:alexandria #:cl #:iterate)
  (:export
   ;;;enumerable
   #:enumerable
   #:lambdae
   #:defenumerable
   #:yield
   #:yield-break

   #:map-enumerable
   #:get-enumerator

   ;;;enumerator
   #:current
   #:move-next

   ;;;enumerable expressions
   #:all
   #:any
   #:any*
   #:eappend
   #:contains
   #:concat
   #:ecount
   #:ecount*
   #:default-if-empty
   #:distinct
   #:element-at
   #:empty
   #:except
   #:efirst
   #:efirst*
   #:elast
   #:elast*
   #:prepend
   #:range
   #:repeat
   #:select
   #:select*
   #:select-many
   #:select-many*
   #:skip
   #:skip-while
   #:take
   #:take-while
   #:where
   #:to-list
   #:to-vector))
