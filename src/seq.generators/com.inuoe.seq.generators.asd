(defsystem #:com.inuoe.seq.generators
  :version "0.0.0.0"
  :description "Continuations support for Enumerable"
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "CC0 <http://creativecommons.org/publicdomain/zero/1.0/>"
  :serial t
  :components
  ((:file "seq.generators"))
  :depends-on
  (#:alexandria
   #:cl-cont
   #:com.inuoe.seq))
