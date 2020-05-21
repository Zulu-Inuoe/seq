(defsystem #:com.inuoe.doseq-tests
  :version "0.0.0.0"
  :description "Tests for the doseq library"
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "CC0 <http://creativecommons.org/publicdomain/zero/1.0/>"
  :serial t
  :components
  ((:file "doseq-tests"))
  :depends-on
  (#:alexandria
   #:fiveam
   #:com.inuoe.seq
   #:com.inuoe.doseq))
