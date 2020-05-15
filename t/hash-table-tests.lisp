(defpackage #:com.inuoe.seqio-tests.hash-table-tests
  (:use
   #:cl
   #:com.inuoe.seqio
   #:com.inuoe.seqio-tests)
  (:import-from
   #:fiveam
   #:def-suite
   #:in-suite
   #:test)
  (:export
   ;;; Test suites
   #:seqio.hash-table))

(in-package #:com.inuoe.seqio-tests)

(def-suite seqio.hash-table
  :description "Tests for seqio implementation for hash tables."
  :in seqio)

(in-suite seqio.hash-table)
