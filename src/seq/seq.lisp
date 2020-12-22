(defpackage #:com.inuoe.seq
  (:use #:cl)
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

(declaim (ftype (function ((or symbol function)) (values lazy-seq &optional)) make-lazy-seq))
(defun make-lazy-seq (factory)
  (make-instance 'lazy-seq :factory factory))

(defmacro lazy-seq (&body body)
  `(make-lazy-seq (lambda () ,@body)))

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
    (if-let ((factory (slot-value col '%lazy-seq-factory)))
      (setf (slot-value col '%lazy-seq-factory) nil
            (slot-value col '%lazy-seq-value) (col-seq (funcall factory)))
      (slot-value col '%lazy-seq-value)))
  (:method ((col package))
    (let ((seq ()))
      (do-symbols (symbol col seq)
        (push symbol seq))))
  (:method ((col stream))
    (let ((type (stream-element-type col)))
      (cond
        ((subtypep type 'integer)
         (labels ((recurse-bytes ()
                    (when-let ((val (read-byte col nil)))
                      (cons val (lazy-seq (recurse-bytes))))))
           (recurse-bytes)))
        ((subtypep type 'character)
         (labels ((recurse-chars ()
                    (when-let ((val (read-char col nil)))
                      (cons val (lazy-seq (recurse-chars))))))
           (recurse-chars)))
        (t
         (error "Unsupported stream element type '~A'" type))))))

(defgeneric mapcol (col fn)
  (:documentation
   "Eagerly apply `fn' to every element in `col'.")
  (:method (col fn)
    (loop :for seq := (col-seq col) :then (col-seq (seq-rest seq))
          :while seq
          :do (funcall fn (seq-first seq)))
    (values))
  (:method ((col null) fn)
    (values))
  (:method ((col list) fn)
    (loop :for seq := (col-seq col) :then (col-seq (seq-rest seq))
          :while seq
          :do (funcall fn (seq-first seq)))
    (values))
  (:method ((col sequence) fn)
    (map nil fn col)
    (values))
  (:method  ((col hash-table) fn)
    (maphash (lambda (k v) (funcall fn (cons k v))) col)
    (values))
  (:method  ((col stream) fn)
    (loop :with reader := (let ((type (stream-element-type col)))
                            (cond
                              ((subtypep type 'integer)   #'read-byte)
                              ((subtypep type 'character) #'read-char)
                              (t (error "Unsupported stream element type '~A'" type))))
          :for x := (funcall reader col nil)
          :while x
          :do (funcall fn x))
    (values))
  (:method ((col lazy-seq) fn)
    (loop :for seq := (col-seq col) :then (col-seq (seq-rest seq))
          :while seq
          :do (funcall fn (seq-first seq)))
    (values)))

(defun mapcol* (col fn)
  (mapcol col (let ((i 0)) (lambda (x) (funcall fn x i) (incf i))))
  (values))

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
        (labels ((recurse (array offset count)
                   (multiple-value-bind (displaced-to displaced-offset) (array-displacement array)
                     (cond
                       (displaced-to
                        (recurse displaced-to (+ offset displaced-offset) count))
                       (t
                        (make-array count :element-type (array-element-type array) :displaced-to array :displaced-index-offset offset))))))
          (recurse seq 1 (1- len)))))))

(defmethod print-object ((object lazy-seq) stream)
  (print-unreadable-object (object stream)
    (format stream "LAZY-SEQ [")
    (if (null (slot-value object '%lazy-seq-factory))
      (loop
        :with seq := (slot-value object '%lazy-seq-value)
        :for first := t :then nil
        :while seq
        :do (format stream "~:[ ~;~]~A" first (seq-first seq))
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
