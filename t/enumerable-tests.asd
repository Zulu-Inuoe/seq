;;;enumerable-tests - tests for the enumerable library
;;;Written in 2018 by Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>
;;;
;;;To the extent possible under law, the author(s) have dedicated all copyright
;;;and related and neighboring rights to this software to the public domain
;;;worldwide. This software is distributed without any warranty.
;;;You should have received a copy of the CC0 Public Domain Dedication along
;;;with this software. If not, see
;;;<http://creativecommons.org/publicdomain/zero/1.0/>.

(defsystem #:enumerable-tests
  :name "enumerable-tests"
  :version "0.0.0.0"
  :description "Tests for the enumerable library"
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "CC0 <http://creativecommons.org/publicdomain/zero/1.0/>"
  :serial t
  :components
  ((:file "package")
   (:file "enumerable-tests")
   (:file "do-enumerable")
   (:module "list"
    :components
    ((:file "package")
     (:file "list")))
   (:module "vector"
    :components
    ((:file "package")
     (:file "vector"))))
  :depends-on
  (#:enumerable
   #:fiveam))
