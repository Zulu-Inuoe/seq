(defsystem #:com.inuoe.seq.cl-stream
  :version "0.0.0.0"
  :description "cl-stream Seq integration"
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "CC0 <http://creativecommons.org/publicdomain/zero/1.0/>"
  :serial t
  :components
  ((:file "seq.cl-stream"))
  :depends-on
  (#:alexandria
   #:cl-stream
   #:com.inuoe.seq))
