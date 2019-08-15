(defpackage #:enumerable-tests
  (:use #:alexandria #:cl #:enumerable)
  (:export
   #:eequal
   #:eequalp
   #:eset-equal

   ;;; Programmatic run-tests
   #:run-tests

   ;;; Test suites for base enumerable support
   #:enumerable
   #:enumerable.list
   #:enumerable.hash-table
   #:enumerable.vector

   ;;; Test suites for specializing the 'expressions'
   #:expressions
   #:expressions.generic
   #:expressions.list
   #:expressions.vector))
