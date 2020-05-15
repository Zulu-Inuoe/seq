(defpackage #:com.inuoe.seq
  (:use
   #:cl)
  (:import-from
   #:alexandria
   #:hash-table-alist
   #:when-let)
  (:export
   #:col-seq
   #:mapcol

   #:seq-first
   #:seq-rest

   #:lazy-seq
   #:lazy-seq-p))

(in-package #:com.inuoe.seq)

(defstruct (lazy-seq
            (:conc-name nil)
            (:constructor %make-lazy-seq (%lazy-seq-factory))
            (:copier nil))
  (%lazy-seq-factory (required-argument)
   :type (or function symbol null))
  (%lazy-seq-value nil
   :type t))

(defmacro lazy-seq (&body body)
  `(%make-lazy-seq (lambda () (col-seq (progn ,@body)))))

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
  (:method  ((lazy-seq lazy-seq))
    (with-accessors ((%lazy-seq-factory %lazy-seq-factory)
                     (%lazy-seq-value %lazy-seq-value))
        lazy-seq
      (when %lazy-seq-factory
        (setf %lazy-seq-value (funcall %lazy-seq-factory)
              %lazy-seq-factory nil))
      %lazy-seq-value))
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

(defun %mapcol-generic (fn col)
  (loop
    :for seq := (col-seq col) :then (col-seq (seq-rest seq))
    :while seq
    :do (funcall fn (seq-first seq)))
  (values))

(defgeneric mapcol (fn col)
  (:documentation
   "Eagerly apply `fn' to every element in `col'.")
  (:method (fn col)
    (%mapcol-generic fn col))
  (:method (fn (col null))
    (values))
  (:method  (fn (col sequence))
    (map nil fn col)
    (values))
  (:method (fn (col list))
    (%mapcol-generic fn col))
  (:method  (fn (col vector))
    (loop :for x :across col
          :do (funcall fn x))
    (values))
  (:method (fn (col sequence))
    (map nil fn col)
    (values))
  (:method  (fn (col hash-table))
    (flet ((kv-fcall (k v)
             (funcall fn (cons k v))))
      (declare (dynamic-extent #'kv-fcall))
      (maphash #'kv-fcall col)))
  (:method  (fn (col stream))
    (cond
      ((subtypep (stream-element-type col) 'integer)
       (loop :for x := (read-byte col nil)
             :while x
             :do (funcall fn x)))
      ((subtypep (stream-element-type col) 'character)
       (loop :for x := (read-char col nil)
             :while x
             :do (funcall fn x))))
    (values)))

(defgeneric seq-first (seq)
  (:documentation
   "Returns the first element of `seq'")
  (:method ((col list))
    (car col))
  (:method ((col vector))
    (unless (zerop (length col))
      (aref col 0)))
  (:method ((lazy-seq lazy-seq))
    (seq-first (col-seq lazy-seq))))

(defgeneric seq-rest (seq)
  (:documentation
   "Returns the rest of the elements of `seq'")
  (:method ((col list))
    (cdr col))
  (:method ((col vector))
    (let ((len (length col)))
      (when (> len 1)
        (%make-collapsed-displaced-vector col 1 (1- len)))))
  (:method ((lazy-seq lazy-seq))
    (seq-rest (col-seq lazy-seq))))

(defmethod print-object ((object lazy-seq) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "[")
    (if (null (%lazy-seq-factory object))
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
               ((and (lazy-seq-p rest) (%lazy-seq-factory rest))
                ;; next sequence is lazy and not evaluated
                (format stream " . ")
                (format stream "~A" rest)
                (loop-finish))
               (t
                ;; next sequence is available so we can carry on
                (setf seq (col-seq rest))))))
      (format stream " ... "))
    (format stream "]")))
