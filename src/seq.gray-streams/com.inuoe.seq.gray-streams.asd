(defsystem #:#:com.inuoe.seq.gray-streams
  :version "0.0.0.0"
  :description "Gray Stream Seq integration"
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "CC0 <http://creativecommons.org/publicdomain/zero/1.0/>"
  :serial t
  :components
  ((:file "seq.gray-streams"))
  :depends-on
  (#:alexandria
   #:com.inuoe.seq
   #:trivial-gray-streams))
