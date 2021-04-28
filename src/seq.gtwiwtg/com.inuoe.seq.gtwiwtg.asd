(defsystem #:com.inuoe.seq.gtwiwtg
  :version "0.0.0.0"
  :description "GTWIWTG Seq integration"
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "CC0 <http://creativecommons.org/publicdomain/zero/1.0/>"
  :serial t
  :components
  ((:file "seq.gtwiwtg"))
  :depends-on
  (#:com.inuoe.seq
   #:gtwiwtg))
