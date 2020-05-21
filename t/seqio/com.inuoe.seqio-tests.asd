(defsystem #:com.inuoe.seqio-tests
  :version "0.0.0.0"
  :description "Tests for the seqio library"
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "CC0 <http://creativecommons.org/publicdomain/zero/1.0/>"
  :serial t
  :components
  ((:file "seqio-tests")
   (:file "generic-tests")
   (:file "list-tests")
   (:file "vector-tests")
   (:file "hash-table-tests"))
  :depends-on
  (#:fiveam
   #:com.inuoe.seq
   #:com.inuoe.seqio
   #:com.inuoe.doseq))
