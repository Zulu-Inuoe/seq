(defpackage #:com.inuoe.seq.trivial-extensible-sequences
  (:use
   #:cl)
  (:import-from
   #:closer-mop)
  (:local-nicknames
   (#:seq #:com.inuoe.seq)
   (#:sequences #:org.shirakumo.trivial-extensible-sequences))
  (:export))

(in-package #:com.inuoe.seq.trivial-extensible-sequences)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let* ((lazy-seq-class (find-class 'seq:lazy-seq))
         (supers (c2mop:class-direct-superclasses lazy-seq-class))
         (new-supers (copy-list (remove-duplicates (append supers (list (find-class 'cl:sequence)))))))
    (reinitialize-instance lazy-seq-class :direct-superclasses new-supers)))

(defmethod sequences:length ((sequence seq:lazy-seq))
  (let ((count 0))
    (seq:mapcol sequence (lambda (elt)
                           (declare (ignore elt))
                           (incf count)))
    count))

(defmethod sequences:elt ((sequence seq:lazy-seq) index)
  (loop
    :for count :from 0
    :for seq := (seq:col-seq sequence) :then (seq:col-seq (seq:seq-rest seq))
    :while seq
    :if (= count index) :return (seq:seq-first seq)
    :finally (error "The index ~A is too large for a lazy-seq of length ~A" index count)))

(defmethod (setf sequences:elt) (value (sequence seq:lazy-seq) index)
  (loop
    :for count :from 0
    :for seq := (seq:col-seq sequence) :then (seq:col-seq (seq:seq-rest seq))
    :while seq
    :if (= count index) :return (setf (sequences:elt seq 0) value)
    :finally (error "The index ~A is too large for a lazy-seq of length ~A" index count)))

(defmethod sequences:adjust-sequence ((sequence seq:lazy-seq) length &key initial-contents initial-element)
  (declare (ignore initial-contents initial-element))
  (error "Unimplemented"))

(defmethod sequences:make-sequence-like ((sequence seq:lazy-seq) length &key initial-contents initial-element)
  (declare (ignore initial-contents initial-element))
  (error "Unimplemented"))

(defmethod sequences:make-sequence-iterator ((sequence seq:lazy-seq) &key start end from-end)
  (let* ((sequence (if (or (null start) (zerop start))
                       sequence
                       ;; Skip start elements
                       (loop
                         :with col := sequence
                         :for count :from 0
                         :when (= count start)
                           :return col
                         :do (let ((seq (seq:col-seq col)))
                               (unless seq (return nil))
                               (setf col (seq:seq-rest seq))))))
         (sequence (if (null from-end)
                       sequence
                       (cond
                         ((null end)
                          ;; Construct a reversed list and use that, instead
                          ;; tbd probably fails on setf business
                          (let ((res (list)))
                            (seq:mapcol sequence (lambda (elt)
                                                   (push elt res)))
                            res))
                         ((zerop end)
                          nil)
                         (t
                          ;; Construct a constrained reversed list
                          (prog ((count 0)
                                 (res (list)))
                             (seq:mapcol sequence
                                         (lambda (elt)
                                           (push elt res)
                                           (when (= (incf count) end)
                                             (return res))))
                             (return res)))))))

    (values (cons (seq:col-seq sequence) 0) end from-end

            ;; Iter - modify & return new state
            (lambda (#1=#:seq #2=#:state #3=#:from-end)
              (declare (ignore #1# #3#))
              (setf (car #2#) (seq:col-seq (seq:seq-rest (car #2#)))
                    (cdr #2#) (1+ (cdr #2#)))
              #2#)
            ;; endp - return true if at end
            (if end
                (lambda (#1# #2# #4=#:limit #3#)
                  (declare (ignore #1# #3#))
                  (destructuring-bind (state . count) #2#
                    (or (>= count #4#)
                        (null state))))
                (lambda (#1# #2# #4# #3#)
                  (declare (ignore #1# #4# #3#))
                  (null (car #2#))))
            ;; Current value
            (lambda (#1# #2#)
              (declare (ignore #1#))
              (seq:seq-first (car #2#)))
            ;; setf current value
            (lambda (#5=#:value #1# #2#)
              (declare (ignore #1#))
              (setf (sequences:elt (car #2#) 0) #5#))
            ;; index
            (lambda (#1# #2#)
              (declare (ignore #1#))
              (cdr #2#))
            ;; copy iterator
            (lambda (#1# #2#)
              (declare (ignore #1#))
              (copy-list #2#)))))
