;;;enumerable - enumerable implementation for CL, using cl-cont
;;;Written in 2018 by Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>
;;;
;;;To the extent possible under law, the author(s) have dedicated all copyright
;;;and related and neighboring rights to this software to the public domain
;;;worldwide. This software is distributed without any warranty.
;;;You should have received a copy of the CC0 Public Domain Dedication along
;;;with this software. If not, see
;;;<http://creativecommons.org/publicdomain/zero/1.0/>.

(in-package #:enumerable)

(defmethod map-enumerable (fn enumerable)
  ;;NOTE: default expander for do-enumerable uses map-enumerable,
  ;;so don't use it here, otherwise bad time recursion
  (loop :with enumerator := (get-enumerator enumerable)
        :while (move-next enumerator)
        :for x := (current enumerator)
        :do (funcall fn x))
  (values))

(defmethod aggregate (enumerable aggregator)
  (let ((enumerator (get-enumerator enumerable)))
    (unless (move-next enumerator)
      (error "enumerable contains no elements."))
    (loop
      :with accum := (current enumerator)
      :while (move-next enumerator)
      :do (setf accum (funcall aggregator accum (current enumerator)))
      :finally (return accum))))

(defmethod aggregate* (enumerable aggregator seed)
  (let ((accum seed))
    (do-enumerable (x enumerable accum)
      (setf accum (funcall aggregator accum x)))))

(defmethod all (enumerable predicate)
  (not (any* enumerable (complement predicate))))

(defmethod any (enumerable)
  (do-enumerable (x enumerable nil)
    (declare (ignore x))
    (return t)))

(defmethod any* (enumerable predicate)
  (do-enumerable (x enumerable)
    (when (funcall predicate x)
      (return t))))

(defmethod eappend (enumerable element)
  (with-enumerable
    (do-enumerable (x enumerable)
      (yield x))
    (yield element)))

(defmethod batch (enumerable size &key (element-type t) adjustable fill-pointer-p)
  (with-enumerable
    (loop
      :with buf := (make-array size :element-type element-type)
      :with enumerator := (get-enumerator enumerable)
      :do
         (loop
           :for i :below size
           :while (move-next enumerator)
           :do (setf (aref buf i) (current enumerator))
           :finally
              (cond
                ((zerop i)
                 (yield-break))
                ((= i size) ; Full batch
                 (yield (make-array i :element-type element-type :initial-contents buf :adjustable adjustable :fill-pointer (and fill-pointer-p t))))
                (t ; Partial batch
                 (yield (replace (make-array i :element-type element-type :adjustable adjustable :fill-pointer (and fill-pointer-p t)) buf))))))))

(defmethod concat (first second)
  (with-enumerable
    (do-enumerable (x first)
      (yield x))
    (do-enumerable (x second)
      (yield x))))

(defmethod consume (enumerable)
  (do-enumerable (elt enumerable)
    (declare (ignore elt)))
  (values))

(defmethod contains (enumerable item &optional (test #'eql))
  (any* enumerable (lambda (x) (funcall test x item))))

(defmethod ecount (enumerable)
  (let ((count 0))
    (do-enumerable (x enumerable)
      (declare (ignore x))
      (incf count))
    count))

(defmethod ecount* (enumerable predicate)
  (let ((count 0))
    (do-enumerable (x enumerable)
      (when (funcall predicate x)
        (incf count)))
    count))

(defmethod default-if-empty (enumerable &optional default)
  (if (any enumerable)
      enumerable
      (list default)))

(defmethod distinct (enumerable &optional (test #'eql))
  (with-enumerable
    (let ((known-elements ()))
      (do-enumerable (x enumerable)
        (unless (find x known-elements :test test)
          (yield x)
          (push x known-elements))))))

(defmethod element-at (enumerable index &optional default)
  (efirst (skip enumerable index) default))

(defmethod evaluate (functions)
  (with-enumerable
    (do-enumerable (fn functions)
      (yield (funcall fn)))))

(defmethod except (first second &optional (test #'eql))
  (with-enumerable
    (let ((e1 (get-enumerator first))
          (e2 (get-enumerator second))
          (yielded '())
          (encountered-second '()))
      (flet ((search-encountered (elt1)
               (and (position elt1 encountered-second :test test) t))
             (search-e2 (elt1)
               (loop
                 :while (move-next e2)
                 :for elt2 := (current e2)
                 :do (pushnew elt2 encountered-second :test test)
                 :if (funcall test elt1 elt2)
                   :return t)))
        (loop
          :while (move-next e1)
          :for elt1 := (current e1)
          :if (and (not (search-encountered elt1))
                   (not (search-e2 elt1))
                   (not (find elt1 yielded :test test)))
            :do (push elt1 yielded)
                (yield elt1))))))

(defmethod efirst (enumerable &optional default)
  (do-enumerable (x enumerable default)
    (return-from efirst x)))

(defmethod efirst* (enumerable predicate &optional default)
  (do-enumerable (x enumerable default)
    (when (funcall predicate x)
      (return-from efirst* x))))

(defmethod group-by (enumerable key
                     &key
                       (test #'eql)
                       (selector #'identity)
                       (result-selector #'make-grouping))
  (with-enumerable
    (loop
      :with groups := ()
      :with enumerator := (get-enumerator enumerable)
      :while (move-next enumerator)
      :for elt := (funcall selector (current enumerator))
      :for elt-key := (funcall key elt)
      :for group := (find elt-key groups :key #'car :test test)
      :if group
        :do (push elt (cdr group))
      :else
        :do (push (cons elt-key (cons elt nil)) groups)
      :finally
         (loop
           :for (group-key . elts) :in (nreverse groups)
           :do (yield (funcall result-selector group-key (nreverse elts)))))))

(defmethod intersect (first second &optional (test #'eql))
  (with-enumerable
    (let ((e1 (get-enumerator first))
          (e2 (get-enumerator second))
          (yielded '())
          (encountered-second '()))
      (flet ((search-encountered (elt1)
               (and (position elt1 encountered-second :test test) t))
             (search-e2 (elt1)
               (loop
                 :while (move-next e2)
                 :for elt2 := (current e2)
                 :do (pushnew elt2 encountered-second :test test)
                 :if (funcall test elt1 elt2)
                   :return t)))
        (loop
          :while (move-next e1)
          :for elt1 := (current e1)
          :if (and (or (search-encountered elt1)
                       (search-e2 elt1))
                   (not (find elt1 yielded :test test)))
            :do (push elt1 yielded)
                (yield elt1))))))

(defmethod elast (enumerable &optional default)
  (let ((last-res default))
    (do-enumerable (x enumerable last-res)
      (setf last-res x))))

(defmethod elast* (enumerable predicate &optional default)
  (let ((last-res default))
    (do-enumerable (x enumerable last-res)
      (when (funcall predicate x)
        (setf last-res x)))))

(defmethod prepend (enumerable element)
  (with-enumerable
    (yield element)
    (do-enumerable (x enumerable)
      (yield x))))

(defmethod ereverse (enumerable)
  (with-enumerable
    (let ((stack ()))
      (do-enumerable (x enumerable)
        (push x stack))
      (loop
        :while stack
        :for x := (pop stack)
        :do (yield x)))))

(defmethod select (enumerable selector)
  (with-enumerable
    (do-enumerable (x enumerable)
      (yield (funcall selector x)))))

(defmethod select* (enumerable selector)
  (with-enumerable
    (let ((i 0))
      (do-enumerable (x enumerable)
        (yield (funcall selector x i))
        (incf i)))))

(defmethod select-many (enumerable selector &optional (result-selector #'identity))
  (with-enumerable
    (do-enumerable (elt enumerable)
      (do-enumerable (sub-elt (funcall selector elt))
        (yield (funcall result-selector sub-elt))))))

(defmethod select-many* (enumerable selector &optional (result-selector #'identity))
  (with-enumerable
    (let ((i 0))
      (do-enumerable (elt enumerable)
        (do-enumerable (sub-elt (funcall selector elt i))
          (yield (funcall result-selector sub-elt)))
        (incf i)))))

(defmethod single (enumerable &optional default)
  (let ((found-value nil)
        (ret default))
    (do-enumerable (x enumerable ret)
      (when found-value
        (error "more than one element present in the enumerable"))
      (setf found-value t
            ret x))))

(defmethod single* (enumerable predicate &optional default)
  (let ((found-value nil)
        (ret default))
    (do-enumerable (x enumerable ret)
      (when (funcall predicate x)
        (when found-value
          (error "more than one element present in the enumerable matches predicate"))
        (setf found-value t
              ret x)))))

(defmethod skip (enumerable count)
  (with-enumerable
    (let ((enumerator (get-enumerator enumerable)))
      (unless (zerop count)
        (loop :repeat count
              :for valid := (move-next enumerator)
              :while valid
              :finally
                 (unless valid
                   (yield-break))))
      (loop :while (move-next enumerator)
            :do (yield (current enumerator))))))

(defmethod skip-last (enumerable count)
  (cond
    ((zerop count)
     enumerable)
    (t
     (with-enumerable
       (let* ((enumerator (get-enumerator enumerable))
              (queue
                (loop
                  :for i :below count
                  :while (move-next enumerator)
                  :collect (current enumerator))))
         (when (move-next enumerator)
           (loop
             :with tail := (last queue)
             :do (setf (cdr tail) (cons (current enumerator) nil)
                       tail (cdr tail))
                 (yield (pop queue))
             :while (move-next enumerator))))))))

(defmethod skip-until (enumerable predicate)
  (with-enumerable
    (let ((enumerator (get-enumerator enumerable)))
      (loop :while (move-next enumerator)
            :for x := (current enumerator)
            :if (funcall predicate x)
              :do (yield x)
                  (loop-finish))
      (loop :while (move-next enumerator)
            :for x := (current enumerator)
            :do (yield x)))))

(defmethod skip-while (enumerable predicate)
  (with-enumerable
    (let ((enumerator (get-enumerator enumerable)))
      (loop :while (move-next enumerator)
            :for x := (current enumerator)
            :while (funcall predicate x)
            :finally (yield x))
      (loop :while (move-next enumerator)
            :for x := (current enumerator)
            :do (yield x)))))

(defmethod take (enumerable count)
  (unless (<= count 0)
    (with-enumerable
      (loop
        :with enumerator := (get-enumerator enumerable)
        :for i :from 0 :below count
        :while (move-next enumerator)
        :do (yield (current enumerator))))))

(defmethod take-every (enumerable step)
  (unless (and (integerp step)
               (plusp step))
    (error "step must be a positive integer, was ~A" step))
  (with-enumerable
    (let ((i 0)
          (next-index 0))
      (do-enumerable (elt enumerable)
        (when (= i next-index)
          (yield elt)
          (incf next-index step))
        (incf i)))))

(defmethod take-last (enumerable count)
  (when (minusp count)
    (error "count cannot be negative, was ~A" count))
  (cond
    ((zerop count)
     nil)
    (t
     (let ((res (make-array count :fill-pointer t))
           (index 0)
           (filled nil))
       (do-enumerable (elt enumerable)
         (setf (aref res index) elt)
         (when (= (incf index) count)
           (setf index 0)
           (setf filled t)))

       (if filled
           ;;Organize the array by shifting things to the left by `index'
           (loop :repeat index
                 :do
                    (loop :for i :from 0 :below (1- count)
                          :do (rotatef (aref res i)
                                       (aref res (1+ i)))))
           (setf (fill-pointer res) index))
       res))))

(defmethod take-until (enumerable predicate)
  (with-enumerable
    (do-enumerable (elt enumerable)
      (if (funcall predicate elt)
          (yield-break)
          (yield elt)))))

(defmethod take-while (enumerable predicate)
  (with-enumerable
    (do-enumerable (x enumerable)
      (if (funcall predicate x)
          (yield x)
          (yield-break)))))

(defmethod eunion (first second &optional (test #'eql))
  (with-enumerable
    (let ((known-elements ()))
      (do-enumerable (x first)
        (unless (find x known-elements :test test)
          (yield x)
          (push x known-elements)))
      (do-enumerable (x second)
        (unless (find x known-elements :test test)
          (yield x)
          (push x known-elements))))))

(defmethod where (enumerable predicate)
  (with-enumerable
    (do-enumerable (x enumerable)
      (when (funcall predicate x)
        (yield x)))))

(defmethod window (enumerable size &key (element-type t) adjustable fill-pointer-p)
  (with-enumerable
    (let ((enumerator (get-enumerator enumerable))
          (buf (make-array size :element-type element-type :adjustable adjustable :fill-pointer (and fill-pointer-p t))))
      (loop
        :for i :below size
        :if (move-next enumerator)
          :do (setf (aref buf i) (current enumerator))
        :else
          :do (yield-break))
      (yield buf)

      (loop
        :while (move-next enumerator)
        :for prev-window := buf :then window
        :for window := (make-array size :element-type element-type :adjustable adjustable :fill-pointer (and fill-pointer-p t))
        :do (replace window prev-window :start2 1)
            (setf (aref window (1- size)) (current enumerator))
            (yield window)))))

(defmethod to-hash-table (enumerable key &key (selector #'identity) (test #'eql))
  (let ((ret (make-hash-table :test test)))
    (do-enumerable (x enumerable)
      (setf (gethash (funcall key x) ret) (funcall selector x)))
    ret))

(defmethod to-list (enumerable)
  (let ((res ()))
    (do-enumerable (x enumerable)
      (push x res))
    (nreverse res)))

(defmethod to-vector (enumerable &key (element-type t) adjustable fill-pointer-p)
  (let ((res ())
        (len 0))
    (do-enumerable (x enumerable)
      (push x res)
      (incf len))
    (make-array len
                :element-type element-type
                :initial-contents (nreverse res)
                :adjustable adjustable
                :fill-pointer (and fill-pointer-p t))))
