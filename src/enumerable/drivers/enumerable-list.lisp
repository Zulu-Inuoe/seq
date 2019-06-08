(in-package #:enumerable)

(defmethod map-enumerable (fn (enumerable list))
  (mapc fn enumerable)
  (values))

(defstruct (%list-enumerator
            (:conc-name nil)
            (:constructor %make-list-enumerator (%list-enumerator-current))
            (:copier nil))
  (%list-enumerator-current (required-argument '%list-enumerator-current)
   :type list))

(defmethod get-enumerator ((enumerable list))
  (%make-list-enumerator (cons nil enumerable)))

(defmethod current ((enumerator %list-enumerator))
  (car (%list-enumerator-current enumerator)))

(defmethod move-next ((enumerator %list-enumerator))
  (and (setf (%list-enumerator-current enumerator) (cdr (%list-enumerator-current enumerator)))
       t))

(defmethod any* ((enumerable list) predicate)
  (member-if predicate enumerable))

(defmethod consume ((enumerable list))
  (values))

(defmethod contains ((enumerable list) item &optional (test #'eql))
  (member item enumerable :test test))

(defmethod element-at ((enumerable list) index &optional default)
  (if-let ((cell (nthcdr index enumerable)))
    (car cell)
    default))

(defmethod elast ((enumerable list) &optional default)
  (if enumerable
      (car (last enumerable))
      default))

(defmethod prepend ((enumerable list) element)
  (cons element enumerable))

(defmethod single ((enumerable list) &optional default)
  (cond
    ((cdr enumerable) (error "more than one element present in the enumerable"))
    (enumerable (car enumerable))
    (t default)))

(defmethod single* ((enumerable list) predicate &optional default)
  (let ((found-value nil)
        (ret default))
    (dolist (x enumerable ret)
      (when (funcall predicate x)
        (when found-value
          (error "more than one element present in the enumerable matches predicate"))
        (setf found-value t
              ret x)))))

(defmethod skip ((enumerable list) count)
  (if (<= count 0)
      enumerable
      (lazy-seq (nthcdr count enumerable))))

(defmethod skip-until ((enumerable list) predicate)
  (lazy-seq
    (let ((cell enumerable))
      (loop
        :while (and cell (not (funcall predicate (car cell))))
        :do (setf cell (cdr cell)))
      cell)))

(defmethod skip-while ((enumerable list) predicate)
  (lazy-seq
    (let ((cell enumerable))
      (loop
        :while (and cell (funcall predicate (car cell)))
        :do (setf cell (cdr cell)))
      cell)))

(defmethod take-last ((enumerable list) count)
  (lazy-seq (last enumerable count)))
