(defsystem #:com.inuoe.seq
  :version "0.0.0.0"
  :description "Clojure-style Sequences"
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "CC0 <http://creativecommons.org/publicdomain/zero/1.0/>"
  :serial t
  :components
  ((:file "seq"))
  :depends-on
  (#:alexandria))
