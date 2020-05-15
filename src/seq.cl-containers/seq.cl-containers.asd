(defsystem #:com.inuoe.seq.cl-containers
  :version "0.0.0.0"
  :description "cl-containers seq integration"
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "CC0 <http://creativecommons.org/publicdomain/zero/1.0/>"
  :serial t
  :components
  ((:file "seq.cl-containers"))
  :depends-on
  (#:cl-containers
   #:com.inuoe.seq))
