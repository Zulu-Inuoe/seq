(defpackage #:com.inuoe.seqio-tests
  (:use #:cl)
  (:import-from
   #:alexandria
   #:set-equal)
  (:import-from
   #:fiveam
   #:def-suite
   #:in-suite
   #:test
   #:is)
  (:local-nicknames
   (#:seq #:com.inuoe.seq)
   (#:seqio #:com.inuoe.seqio))
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
  "As `equal', but forces arguments by `seqio:to-list'"
  (equal (seqio:to-list e1) (seqio:to-list e2)))

(defun eequalp (e1 e2)
  "As `equalp', but forces arguments by `seqio:to-list'"
  (equalp (seqio:to-list e1) (seqio:to-list e2)))

(defun eset-equal (e1 e2 &key (test #'eql) (key #'identity))
  "As `set-equal', but forces arguments by `seqio:to-list'"
  (set-equal (seqio:to-list e1) (seqio:to-list e2) :test test :key key))

(test range.generates-range
  (is (eequal '(0 1 2) (seqio:range 0 3))))

(test range.uses-start
  (is (eequal '(1 2 3) (seqio:range 1 3))))

(test range.uses-count
  (is (eequal '(1 2) (seqio:range 1 2))))

(test range.start-0-on-nil
  (is (eequal '(0 1) (seqio:range nil 2))))

(test range.allows-count-nil
  (is (eequal '(0 1 2 3 4) (seqio:take (seqio:range) 5))))

(test range.allows-step
  (is (eequal '(0 2 4 6 8) (seqio:take (seqio:range nil nil 2) 5))))

(test repeatedly.generates-to-limit
  (is (eequal '(1 2 3) (seqio:repeatedly (let ((i 0)) (lambda () (incf i))) 3))))

(test repeatedly.no-call-fn-until-forced
  (is (null (block nil (seqio:repeatedly (lambda () (return t))) nil))))

(test repeatedly.calls-on-consume
  (is (eq t (block nil (seq:col-seq (seqio:repeatedly (lambda () (return t)))) nil))))
