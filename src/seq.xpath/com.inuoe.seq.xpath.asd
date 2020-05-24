(defsystem #:com.inuoe.seq.xpath
  :version "0.0.0.0"
  :description "xpath seq integration"
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "CC0 <http://creativecommons.org/publicdomain/zero/1.0/>"
  :serial t
  :components
  ((:file "seq.xpath"))
  :depends-on
  (#:xpath
   #:com.inuoe.seq))
