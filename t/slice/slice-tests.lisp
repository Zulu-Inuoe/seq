(defpackage #:com.inuoe.slice-tests
  (:use
   #:cl
   #:com.inuoe.slice)
  (:import-from
   #:alexandria)
  (:import-from
   #:fiveam
   #:def-suite
   #:finishes
   #:in-suite
   #:is
   #:test)
  (:export
   ;;; Test suites for specializing the 'expressions'
   #:slice
   ;;; Programmatic run-tests
   #:run
   #:main))

(in-package #:com.inuoe.slice-tests)

(def-suite slice
  :description "Tests for the slice library.")

(defun run ()
  (fiveam:run! 'slice))

(defun main (&rest args)
  (declare (ignore args))
  (let ((result (run)))
    (if result 0 -1)))

(in-suite slice)

(defun s->v (s)
  (let ((l (list)))
    (mapslice s (lambda (x) (push x l)))
    (coerce (nreverse l) 'vector)))

(defun slicev (slice &key start stop step)
  (s->v (slice slice :start start :stop stop :step step)))

(test default-slice-does-nothing
  (let ((vector #(0 1 2)))
    (is (eq vector (slice vector)))))

(test slice-length-works-on-vectors
  (is (= 3 (slice-length #(0 1 2)))))

(test saref-works-on-vectors
  (is (= 1 (saref #(0 1 2) 1))))

(test saref-doesnt-cheat
  (is (= 2 (saref #(2) 0))))

(test start-offsets
  (is (equalp #(2) (slicev #(0 1 2) :start 2))))

(test stop-limits
  (is (equalp #(0) (slicev #(0 1 2) :stop 1))))

(test negative-start-takes-from-end
  (is (equalp #(1 2) (slicev #(0 1 2) :start -2))))

(test negative-stop-counts-from-end
  (is (equalp #(0) (slicev #(0 1 2) :stop -2))))

(test out-of-bounds-start-ok
  (is (equalp #() (slicev #(0 1 2) :start 1827))))

(test out-of-bounds-start-negative-ok
  (is (equalp #(0 1 2) (slicev #(0 1 2) :start -1827))))

(test out-of-bounds-stop-ok
  (is (equalp #(0 1 2) (slicev #(0 1 2) :stop 1827))))

(test out-of-bounds-negative-stop-ok
  (is (equalp #() (slicev #(0 1 2) :stop -1827))))

(test start-and-stop-work-together
  (is (equalp #(1) (slicev #(0 1 2) :start 1 :stop 2))))

(test start-and-stop-can-go-past-eachother
  (is (equalp #() (slicev #(0 1 2) :start 29 :stop 0))))

(test step-works
  (is (equalp #(0 2) (slicev #(0 1 2) :step 2))))

(test reverse-step-goes-backwards
  (is (equalp #(2 1 0) (slicev #(0 1 2) :step -1))))

(test step-from-start
  (is (equalp #(1) (slicev #(0 1 2) :start 1 :step 2))))

(test step-respects-stop
  (is (equalp #(0) (slicev #(0 1 2) :stop 1 :step 2))))

(test negative-multi-step
  (is (equalp #(2 0) (slicev #(0 1 2) :step -2))))

(test compound-slice-step
  (is (equalp #(0 1 2) (slicev (slice #(0 1 2) :step -1) :step -1))))

(test compound-slice-start-and-stop
  (is (equalp #(1) (slicev (slice #(0 1 2) :start 1) :stop 1))))

(test compound-slice-negative-step-on-multi-step
  (is (equalp #(2 0) (slicev (slice #(0 1 2 3) :step 2) :step -1))))

(test compound-start-on-negative-stop
  (is (equalp #(0) (slicev (slice #(0 1 2 3) :stop -2 :step -1) :start 1))))