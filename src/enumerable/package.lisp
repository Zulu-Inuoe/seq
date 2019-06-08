(defpackage #:enumerable
  (:use
   #:alexandria
   #:cl
   #:clojure-seq)
  (:export
   ;;;enumerable
   #:enumerable
   #:enumerablep

   #:map-enumerable
   #:get-enumerator

   ;;;enumerator
   #:current
   #:move-next

   ;;;do-enumerable
   #:do-enumerable
   #:define-do-enumerable-expander

   ;;;grouping
   #:grouping
   #:grouping-key
   #:make-grouping

   ;;;enumerable expressions
   #:aggregate
   #:aggregate*
   #:all
   #:any
   #:any*
   #:eappend
   #:batch
   #:concat
   #:consume
   #:contains
   #:ecount
   #:ecount*
   #:default-if-empty
   #:distinct
   #:element-at
   #:evaluate
   #:empty
   #:except
   #:efirst
   #:efirst*
   #:group-by
   #:intersect
   #:elast
   #:elast*
   #:prepend
   #:range
   #:repeat
   #:ereverse
   #:select
   #:select*
   #:select-many
   #:select-many*
   #:single
   #:single*
   #:skip
   #:skip-last
   #:skip-until
   #:skip-while
   #:take
   #:take-every
   #:take-last
   #:take-until
   #:take-while
   #:eunion
   #:where
   #:window
   #:to-hash-table
   #:to-list
   #:to-vector))
