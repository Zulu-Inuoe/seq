(defpackage #:com.inuoe.seqio-tests
  (:use #:cl)
  (:import-from
   #:alexandria
   #:set-equal)
  (:import-from
   #:com.inuoe.seqio
   #:to-list)
  (:import-from
   #:fiveam
   #:def-suite
   #:in-suite)
  (:export
   #:eequal
   #:eequalp
   #:eset-equal

   ;;; Programmatic run-tests
   #:run

   ;;; Test suites for specializing the 'expressions'
   #:seqio
   #:seqio.generic
   #:seqio.vector
   #:seqio.hash-table))

(in-package #:com.inuoe.seqio-tests)

(def-suite seqio
  :description "Tests for the seqio library.")

(in-suite seqio)

(defun run ()
  (fiveam:run! 'seqio))

(defun main (&rest args)
  (declare (ignore args))
  (let ((result (run)))
    (if result 0 -1)))

(defun eequal (e1 e2)
  "As `equal', but forces arguments by `to-list'"
  (equal (to-list e1) (to-list e2)))

(defun eequalp (e1 e2)
  "As `equalp', but forces arguments by `to-list'"
  (equalp (to-list e1) (to-list e2)))

(defun eset-equal (e1 e2 &key (test #'eql) (key #'identity))
  "As `set-equal', but forces arguments by `to-list'"
  (set-equal (to-list e1) (to-list e2) :test test :key key))

