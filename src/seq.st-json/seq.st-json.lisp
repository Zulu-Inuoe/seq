(defpackage #:com.inuoe.seq.st-json
  (:use #:cl)
  (:import-from
   #:com.inuoe.seq
   #:col-seq))

(in-package #:com.inuoe.seq.st-json)

(defmethod col-seq ((obj st-json:jso))
  (st-json::jso-alist obj))
