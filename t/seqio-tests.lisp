(defpackage #:com.inuoe.seqio-tests
  (:use
   #:alexandria
   #:cl
   #:com.inuoe.seqio)
  (:import-from
   #:com.inuoe.seqio
   #:to-list)
  (:import-from
   #:fiveam
   #:def-suite
   #:in-suite
   #:run!)
  (:export
   #:eequal
   #:eequalp
   #:eset-equal

   ;;; Programmatic run-tests
   #:run-tests

   ;;; Test suites for specializing the 'expressions'
   #:seqio
   #:seqio.generic
   #:seqio.vector
   #:seqio.hash-table))

(in-package #:com.inuoe.seqio-tests)

(def-suite seqio
  :description "Tests for the seqio library.")

(in-suite seqio)

(defun eequal (e1 e2)
  "As `equal', but forces arguments by `to-list'"
  (equal (to-list e1) (to-list e2)))

(defun eequalp (e1 e2)
  "As `equalp', but forces arguments by `to-list'"
  (equalp (to-list e1) (to-list e2)))

(defun eset-equal (e1 e2 &key (test #'eql) (key #'identity))
  "As `set-equal', but forces arguments by `to-list'"
  (set-equal (to-list e1) (to-list e2) :test test :key key))

(defun run-tests ()
  (run! 'seqio))

(defun main (&rest args)
  (declare (ignore args))
  (let ((result (run-tests)))
    (if result 0 -1)))
