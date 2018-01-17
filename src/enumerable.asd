;;;enumerable - enumerable implementation for CL, using cl-cont
;;;Written in 2018 by Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>
;;;
;;;To the extent possible under law, the author(s) have dedicated all copyright
;;;and related and neighboring rights to this software to the public domain
;;;worldwide. This software is distributed without any warranty.
;;;You should have received a copy of the CC0 Public Domain Dedication along
;;;with this software. If not, see
;;;<http://creativecommons.org/publicdomain/zero/1.0/>.

(defsystem #:enumerable
  :version "0.0.0.0"
  :description "enumerable"
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "CC0 <http://creativecommons.org/publicdomain/zero/1.0/>"
  :serial t
  :components
  ((:file "package")
   (:file "enumerable")
   (:file "do-enumerable-expander")
   (:file "builtin-expanders")
   (:file "do-enumerable")
   (:file "with-enumerable")
   (:file "expressions")
   (:module "drivers"
    :components
    ((:file "enumerable-generic")
     (:file "enumerable-list")
     (:file "enumerable-vector")
     (:file "enumerable-sequence")
     (:file "enumerable-hash-table")
     (:file "enumerable-package")
     (:file "enumerable-continuation"))))
  :depends-on
  (#:alexandria
   #:cl-cont
   #:trivial-cltl2))
