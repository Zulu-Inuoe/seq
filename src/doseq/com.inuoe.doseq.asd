(defsystem #:com.inuoe.doseq
  :version "0.0.0.0"
  :description "Iteration macro for seqs"
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "CC0 <http://creativecommons.org/publicdomain/zero/1.0/>"
  :serial t
  :components
  ((:file "doseq"))
  :depends-on
  (#:alexandria
   #:introspect-environment
   #:com.inuoe.seq))
