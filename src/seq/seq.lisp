(defpackage #:com.inuoe.seq
  (:use
   #:cl)
  (:import-from
   #:alexandria
   #:hash-table-alist
   #:if-let
   #:required-argument
   #:when-let)
  (:export
   #:col-seq
   #:mapcol
   #:mapcol*

   #:seq-first
   #:seq-rest

   #:lazy-seq
   #:lazy-seq-p
   #:make-lazy-seq))

(in-package #:com.inuoe.seq)

(defclass lazy-seq ()
  ((%lazy-seq-factory
    :initform (required-argument :factory)
    :type (or function symbol null)
    :initarg :factory)
   (%lazy-seq-value
    :initform nil
    :type t)))

(defun lazy-seq-p (x)
  (typep x 'lazy-seq))

(declaim (inline %lazy-seq-value))
(defun %lazy-seq-value (lazy-seq)
  (if-let ((factory (slot-value lazy-seq '%lazy-seq-factory)))
    (setf (slot-value lazy-seq '%lazy-seq-factory) nil
          (slot-value lazy-seq '%lazy-seq-value) (col-seq (funcall factory)))
    (slot-value lazy-seq '%lazy-seq-value)))

(defun make-lazy-seq (factory)
  (make-instance 'lazy-seq :factory factory))

(defmacro lazy-seq (&body body)
  `(make-lazy-seq (lambda () ,@body)))

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

(defgeneric col-seq (col)
  (:documentation
   "Returns a `seq' on the collection `col'")
  (:method ((col list))
    col)
  (:method ((col vector))
    (unless (zerop (length col))
      col))
  (:method ((col hash-table))
    ;;with-hash-table-iterator has unspecified behavior outside of dynamic extent
    ;;so we can't just close it over
    (hash-table-alist col))
  (:method  ((col lazy-seq))
    (%lazy-seq-value col))
  (:method ((col package))
    (let ((res ()))
      (do-symbols (s col)
        (push s res))
      res))
  (:method ((col stream))
    (let ((stream-type (stream-element-type col)))
      (cond
        ((subtypep stream-type 'integer)
         (labels ((recurse-bytes ()
                    (when-let ((val (read-byte col nil nil)))
                      (cons val (lazy-seq (recurse-bytes))))))
           (recurse-bytes)))
        ((subtypep stream-type 'character)
         (labels ((recurse-chars ()
                    (when-let ((val (read-char col nil nil)))
                      (cons val (lazy-seq (recurse-chars))))))
           (recurse-chars)))
        (t
         (error "Unsupported stream element type '~A'" stream-type))))))

(defun %mapcol-generic (col fn)
  (loop
    :for seq := (col-seq col) :then (col-seq (seq-rest seq))
    :while seq
    :do (funcall fn (seq-first seq)))
  (values))

(defgeneric mapcol (col fn)
  (:documentation
   "Eagerly apply `fn' to every element in `col'.")
  (:method (col fn)
    (%mapcol-generic col fn))
  (:method ((col null) fn)
    (values))
  (:method ((col list) fn)
    (%mapcol-generic col fn))
  (:method  ((col vector) fn)
    (loop :for x :across col
          :do (funcall fn x))
    (values))
  (:method ((col sequence) fn)
    (map nil fn col)
    (values))
  (:method  ((col hash-table) fn)
    (maphash (lambda (k v) (funcall fn (cons k v))) col)
    (values))
  (:method  ((col stream) fn)
    (cond
      ((subtypep (stream-element-type col) 'integer)
       (loop :for x := (read-byte col nil)
             :while x
             :do (funcall fn x)))
      ((subtypep (stream-element-type col) 'character)
       (loop :for x := (read-char col nil)
             :while x
             :do (funcall fn x))))
    (values))
  (:method ((col lazy-seq) fn)
    (%mapcol-generic col fn)))

(defun %mapcol*-generic (col fn)
  (mapcol col (let ((i 0)) (lambda (x) (funcall fn x i) (incf i)))))

(defgeneric mapcol* (col fn)
  (:documentation
   "Eagerly apply `fn' to every element in `col'.")
  (:method (col fn)
    (%mapcol*-generic col fn))
  (:method ((col null) fn)
    (values))
  (:method ((col list) fn)
    (%mapcol*-generic col fn))
  (:method  ((col vector) fn)
    (loop :for x :across col
          :for i :from 0
          :do (funcall fn x i))
    (values))
  (:method ((col sequence) fn)
    (map nil (let ((i 0)) (lambda (x) (funcall fn x i) (incf i))) col)
    (values))
  (:method  ((col hash-table) fn)
    (maphash (let ((i 0)) (lambda (k v) (funcall fn (cons k v) i) (incf i))) col)
    (values))
  (:method  ((col stream) fn)
    (cond
      ((subtypep (stream-element-type col) 'integer)
       (loop :for x := (read-byte col nil)
             :for i :from 0
             :while x
             :do (funcall fn x i)))
      ((subtypep (stream-element-type col) 'character)
       (loop :for x := (read-char col nil)
             :for i :from 0
             :while x
             :do (funcall fn x i))))
    (values))
  (:method ((col lazy-seq) fn)
    (%mapcol*-generic col fn)))

(defgeneric seq-first (seq)
  (:documentation
   "Returns the first element of `seq'")
  (:method (col)
    (seq-first (col-seq col)))
  (:method ((seq list))
    (car seq))
  (:method ((seq vector))
    (unless (zerop (length seq))
      (aref seq 0))))

(defgeneric seq-rest (seq)
  (:documentation
   "Returns the rest of the elements of `seq'")
  (:method (col)
    (seq-rest (col-seq col)))
  (:method ((seq list))
    (cdr seq))
  (:method ((seq vector))
    (let ((len (length seq)))
      (when (> len 1)
        (%make-collapsed-displaced-vector seq 1 (1- len))))))

(defmethod print-object ((object lazy-seq) stream)
  (print-unreadable-object (object stream)
    (format stream "LAZY-SEQ [")
    (if (null (slot-value object '%lazy-seq-factory))
      (loop
        :with seq := (%lazy-seq-value object)
        :with first := t
        :do
           (when (null seq)
             (loop-finish))
           (format stream "~:[ ~;~]~A" first (seq-first seq))
           (setf first nil)
           (let ((rest (seq-rest seq)))
             (cond
               ((and (lazy-seq-p rest) (slot-value rest '%lazy-seq-factory))
                ;; next sequence is lazy and not evaluated
                (format stream " . ")
                (format stream "~A" rest)
                (loop-finish))
               (t
                ;; next sequence is available so we can carry on
                (setf seq (col-seq rest))))))
      (format stream " ... "))
    (format stream "]")))
