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

   #:map-enumerable
   #:get-enumerator

   ;;;enumerator
   #:current
   #:move-next

   ;;;enumerable expressions
   #:any
   #:any*
   #:contains
   #:ecount
   #:ecount*
   #:default-if-empty
   #:efirst
   #:efirst*
   #:elast
   #:elast*
   #:select
   #:select*
   #:skip
   #:skip-while
   #:take
   #:take-while
   #:where
   #:to-list
   #:to-vector))
