(defpackage #:com.inuoe.slice
  (:use #:cl)
  (:export
   #:slice
   #:slice-length
   #:saref
   #:mapslice))

(in-package #:com.inuoe.slice)

(defun %make-collapsed-displaced-vector (vec offset count)
  (labels ((recurse (vec offset count)
             (check-type vec vector)
             (multiple-value-bind (displaced-to displaced-offset) (array-displacement vec)
               (cond
                 (displaced-to
                  (recurse displaced-to (+ offset displaced-offset) count))
                 ((and (zerop offset) (= count (length vec)))
                  vec)
                 (t
                  (make-array count :element-type (array-element-type vec) :displaced-to vec :displaced-index-offset offset))))))
    (recurse vec offset count)))

(defstruct (%slice
            (:conc-name nil)
            (:constructor nil)
            (:copier nil)
            (:predicate nil))
  (%slice-start (required-argument)
   :type (integer 0))
  (%slice-stop (required-argument)
   :type (integer 0))
  (%slice-step (required-argument)
   :type (and integer (not (eql 0)))))

(deftype slice ()
  `(or %slice vector))

(defstruct (%vector-slice
            (:include %slice)
            (:conc-name nil)
            (:constructor %make-vector-slice (%vector-slice-vector %slice-start %slice-stop %slice-step))
            (:copier nil)
            (:predicate nil))
  (%vector-slice-vector (required-argument)
   :type vector))

(defstruct (%compound-slice
            (:include %slice)
            (:conc-name nil)
            (:constructor %make-compound-slice (%compound-slice-slice %slice-start %slice-stop %slice-step))
            (:copier nil)
            (:predicate nil))
  (%compound-slice-slice (required-argument)
   :type %slice))

(defun slice-length (slice)
  (etypecase slice
    (vector (length slice))
    (%slice (ceiling (- (%slice-stop slice) (%slice-start slice)) (abs (%slice-step slice))))))

(defun slice (slice &key start stop step)
  (check-type step (or null (and integer (not (eql 0)))))
  (flet ((%bind-start-stop-step (length start stop step)
           (let* ((stop (if stop
                            (max 0 (min length (if (minusp stop) (+ length stop) stop)))
                            length))
                  (start (if start
                             (max 0 (min stop (if (minusp start) (+ length start) start)))
                             0))
                  (step (if step
                            (progn
                              (check-type step (and integer (not (eql 0))))
                              step)
                            1)))
             (values start stop step))))
    (etypecase slice
      (vector
       (multiple-value-bind (start stop step)
           (%bind-start-stop-step (length slice) start stop step)
         (if (= step 1)
             (%make-collapsed-displaced-vector slice start (- stop start))
             (%make-vector-slice slice start stop step))))
      (%slice
       (multiple-value-bind (start stop step)
           (%bind-start-stop-step (slice-length slice) start stop step)
         ;; TODO - There are cases where we can collapse the slice but I can't think of them right now
         (%make-compound-slice slice start stop step))))))

(defun saref (slice index)
  (etypecase slice
    (vector
     (aref slice index))
    (%slice
     (let* ((start (%slice-start slice))
            (stop-1 (1- (%slice-stop slice)))
            (step (%slice-step slice))
            (sindex (+ (* index step)
                       (if (minusp step) stop-1 start))))
       (unless (<= start sindex stop-1)
         (error "Invalid index ~A for ~A, should be between ~A and ~A" index slice start stop-1))
       (etypecase slice
         (%vector-slice
          (aref (%vector-slice-vector slice) sindex))
         (%compound-slice
          (saref (%compound-slice-slice slice) sindex)))))))

(defun (setf saref) (value slice index)
  (etypecase slice
    (vector
     (setf (aref slice index) value))
    (%slice
     (let* ((start (%slice-start slice))
            (stop-1 (1- (%slice-stop slice)))
            (step (%slice-step slice))
            (sindex (+ (* index step)
                       (if (minusp step) stop-1 start))))
       (unless (<= start sindex stop-1)
         (error "Invalid index ~A for ~A, should be between ~A and ~A" index slice start stop-1))
       (etypecase slice
         (%vector-slice
          (setf (aref (%vector-slice-vector slice) sindex) value))
         (%compound-slice
          (setf (saref (%compound-slice-slice slice) sindex) value)))))))

(defun mapslice (slice fn)
  (etypecase slice
    (vector (map nil fn slice))
    (%slice
     (loop
       :with len := (slice-length slice)
       :for i :from 0 :below len
       :do (funcall fn (saref slice i)))))
  (values))

(defmethod print-object ((slice %slice) stream)
  (print-unreadable-object (slice stream)
    (format stream "SLICE #(")
    (let ((first t))
      (mapslice slice (lambda (elt)
                        (unless first
                          (format stream " "))
                        (format stream "~A" elt)
                        (setf first nil))))
    (format stream ")")))
