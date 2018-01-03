;;;Copyright (c) 2018 Wilfredo Velázquez-Rodríguez
;;;
;;;This software is provided 'as-is', without any express or implied
;;;warranty. In no event will the authors be held liable for any damages
;;;arising from the use of this software.
;;;
;;;Permission is granted to anyone to use this software for any purpose,
;;;including commercial applications, and to alter it and redistribute
;;;it freely, subject to the following restrictions:
;;;
;;;1. The origin of this software must not be misrepresented; you must not
;;;   claim that you wrote the original software. If you use this software
;;;   in a product, an acknowledgment in the product documentation would
;;;   be appreciated but is not required.
;;;
;;;2. Altered source versions must be plainly marked as such, and must not
;;;   be misrepresented as being the original software.
;;;
;;;3. This notice may not be removed or altered from any source distribution.

(in-package #:enumerable)

(defgeneric get-enumerator (enumerable))

(defgeneric current (enumerator))
(defgeneric move-next (enumerator))

(defmacro do-enumerable ((var enumerable &optional result)
                         &body body)
  (let ((enumerator-sym (gensym "ENUM")))
    `(loop :with ,enumerator-sym := (get-enumerator ,enumerable)
           :while (move-next ,enumerator-sym)
           :for ,var := (current ,enumerator-sym)
           :do
              (progn
                ,@body)
           :finally
              (return ,result))))

;;;list
(defmethod get-enumerator ((enumerable list))
  (cons nil enumerable))

(defmethod current ((enumerator cons))
  (car enumerator))

(defmethod move-next ((enumerator cons))
  (if (cdr enumerator)
      (progn
        (setf (car enumerator) (cadr enumerator))
        (setf (cdr enumerator) (cddr enumerator))
        t)
      nil))

;;;sequence
(defclass sequence-enumerator ()
  ((sequence
    :type sequence
    :initarg :sequence
    :initform (error "must specify sequence"))
   (index
    :type (integer 0 *)
    :initform 0)))

(defmethod get-enumerator ((enumerable sequence))
  (make-instance 'sequence-enumerator :sequence enumerable))

(defmethod current ((enumerator sequence-enumerator))
  (with-slots (sequence index)
      enumerator
    (if (and (> index 0)
             (<= index (length sequence)))
        (elt sequence (1- index))
        nil)))

(defmethod move-next ((enumerator sequence-enumerator))
  (with-slots (sequence index)
      enumerator
    (if (< index (length sequence))
        (and (incf index) t)
        nil)))

;;hash table
(defmethod get-enumerator ((enumerable hash-table))
  ;;with-hash-table-iterator has unspecified behavior outside of dynamic extent
  ;;so we can't just close it over
  (get-enumerator
   (let ((res ()))
     (maphash (lambda (k v) (push (cons k v) res)) enumerable)
     res)))

(defmacro defenumerable (name args &body body)
  (with-gensyms (cont)
    `(defun ,name ,args
       (make-instance
        'continuation-enumerable
        :starter
        (lambda ()
          (let (,cont)
            (lambda ()
              (if ,cont
                  (funcall ,cont)
                  (cl-cont:with-call/cc
                    (macrolet ((yield (&optional result)
                                 (with-gensyms (cc)
                                   `(cl-cont:let/cc ,cc
                                      (setf ,',cont ,cc)
                                      (values ,result t))))
                               (yield-break ()
                                 `(cl-cont:let/cc _
                                    (declare (ignore _))
                                    (setf ,',cont (lambda () (values nil nil)))
                                    (values nil nil))))
                      ,@body)
                    (cl-cont:let/cc _
                      (declare (ignore _))
                      (setf ,cont (lambda () (values nil nil)))
                      (values nil nil)))))))))))

;;; lazy
(defclass continuation-enumerable ()
  ((starter
    :type (function () continuation-enumerator)
    :initarg :starter
    :initform (error "must supply starter"))))

(defclass continuation-enumerator ()
  ((current
    :type t
    :initform nil)
   (continuation
    :type (function () (values t boolean))
    :initarg :continuation
    :initform (error "must supply continuation"))))

(defmethod get-enumerator ((enumerable continuation-enumerable))
  (with-slots (starter)
      enumerable
    (make-instance
     'continuation-enumerator
     :continuation (funcall starter))))

(defmethod current ((enumerator continuation-enumerator))
  (slot-value enumerator 'current))

(defmethod move-next ((enumerator continuation-enumerator))
  (with-slots (current continuation)
      enumerator
    (multiple-value-bind (val valid)
        (funcall continuation)
      (setf current val)
      valid)))

(defun any (enumerable &optional fn)
  (if fn
      (do-enumerable (x enumerable)
        (when (funcall fn x)
          (return t)))
      (do-enumerable (x enumerable)
        (return t))))

(defun contains (enumerable item &optional (test #'eql))
  (any enumerable (lambda (x) (funcall test x item))))

(defun ecount (enumerable &optional fn)
  (let ((count 0))
    (if fn
        (do-enumerable (x enumerable)
          (when (funcall fn x)
            (incf count)))
        (do-enumerable (x enumerable)
          (incf count)))
    count))

(defun default-if-empty (enumerable &optional default)
  (if (any enumerable)
      enumerable
      (list default)))

(defun efirst (enumerable &optional fn default)
  (if fn
      (do-enumerable (x enumerable)
        (when (funcall fn x)
          (return-from efirst x)))
      (do-enumerable (x enumerable)
        (return-from efirst x)))
  default)

(defun elast (enumerable &optional fn default)
  (let ((last-res default))
    (if fn
        (do-enumerable (x enumerable)
          (when (funcall fn x)
            (setf last-res x)))
        (do-enumerable (x enumerable)
          (setf last-res x)))
    last-res))

(defenumerable select (enumerable fn)
  (do-enumerable (x enumerable)
    (yield (funcall fn x))))

(defenumerable skip (enumerable count)
  (let ((enumerator (get-enumerator enumerable)))
    (unless (zerop count)
      (loop :repeat count
            :for valid := (move-next enumerator)
            :while valid
            :finally
               (unless valid
                 (yield-break))))
    (loop :while (move-next enumerator)
          :do (yield (current enumerator)))))

(defenumerable skip-while (enumerable fn)
  (let ((enumerator (get-enumerator enumerable)))
    (loop :with looped := nil
          :for valid := (move-next enumerator)
          :while (and valid (funcall fn (current enumerator)))
          :finally
             (when (and looped (not valid))
               (yield-break)))
    (loop :with continue := t
          :while continue
          :do
             (yield (current enumerator))
             (setf continue (move-next enumerator)))))

(defenumerable take (enumerable count)
  (loop
    :with enumerator := (get-enumerator enumerable)
    :repeat count
    :while (move-next enumerator)
    :do (yield (current enumerator))))

(defenumerable take-while (enumerable fn)
  (do-enumerable (x enumerable)
    (if (funcall fn x)
        (yield x)
        (yield-break))))

(defenumerable where (enumerable fn)
  (do-enumerable (x enumerable)
    (when (funcall fn x)
      (yield x))))

(defun to-list (enumerable)
  (loop :with enumerator := (get-enumerator enumerable)
        :while (move-next enumerator)
        :collect (current enumerator)))
