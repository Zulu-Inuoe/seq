(defsystem #:com.inuoe.seq.fset
  :version "0.0.0.0"
  :description "FSet Seq integration"
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "CC0 <http://creativecommons.org/publicdomain/zero/1.0/>"
  :serial t
  :components
  ((:file "seq.fset"))
  :depends-on
  (#:com.inuoe.seq
   #:fset))
