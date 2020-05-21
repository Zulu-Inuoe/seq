(defsystem #:com.inuoe.seq.fast-io
  :version "0.0.0.0"
  :description "fast-io seq integration"
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "CC0 <http://creativecommons.org/publicdomain/zero/1.0/>"
  :serial t
  :components
  ((:file "seq.fast-io"))
  :depends-on
  (#:alexandria
   #:fast-io
   #:com.inuoe.seq))
