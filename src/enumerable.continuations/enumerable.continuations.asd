(defsystem #:enumerable.continuations
  :version "0.0.0.0"
  :description "Continuations support for Enumerable"
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "CC0 <http://creativecommons.org/publicdomain/zero/1.0/>"
  :serial t
  :components
  ((:file "enumerable.continuations"))
  :depends-on
  (#:cl-cont
   #:enumerable))
