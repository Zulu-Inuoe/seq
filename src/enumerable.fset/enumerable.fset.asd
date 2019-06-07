(defsystem #:enumerable.fset
  :version "0.0.0.0"
  :description "FSet Enumerable integration"
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "CC0 <http://creativecommons.org/publicdomain/zero/1.0/>"
  :serial t
  :components
  ((:file "package")
   (:file "fset"))
  :depends-on
  (#:alexandria
   #:enumerable
   #:fset))
