(defsystem #:com.inuoe.seq-tests
  :version "0.0.0.0"
  :description "Tests for the seq library"
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "CC0 <http://creativecommons.org/publicdomain/zero/1.0/>"
  :serial t
  :components
  ((:file "seq-tests"))
  :depends-on
  (#:alexandria
   #:fiveam
   #:com.inuoe.seq))
