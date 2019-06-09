(defpackage #:enumerable-tests
  (:use #:alexandria #:cl #:enumerable)
  (:export
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
