(defsystem #:enumerable
  :version "0.0.0.0"
  :description "enumerable"
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "CC0 <http://creativecommons.org/publicdomain/zero/1.0/>"
  :serial t
  :components
  ((:static-file "qlfile")
   (:file "package")
   (:file "enumerable")
   (:file "do-enumerable-expander")
   (:file "builtin-expanders")
   (:file "do-enumerable")
   (:file "grouping")
   (:file "expressions")
   (:module "drivers"
    :components
    ((:file "enumerable-seq")
     (:file "enumerable-generic")
     (:file "enumerable-list")
     (:file "enumerable-vector")
     (:file "enumerable-sequence")
     (:file "enumerable-stream")
     (:file "enumerable-hash-table")
     (:file "enumerable-package"))))
  :depends-on
  (#:alexandria
   #:clojure-seq
   #:introspect-environment))
