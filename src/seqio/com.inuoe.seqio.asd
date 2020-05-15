(defsystem #:com.inuoe.seqio
  :version "0.0.0.0"
  :description "Operations on seqs"
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "CC0 <http://creativecommons.org/publicdomain/zero/1.0/>"
  :serial t
  :components
  ((:file "package")
   (:file "seqio")
   (:file "generic")
   (:file "vector"))
  :depends-on
  (#:alexandria
   #:com.inuoe.seq))
