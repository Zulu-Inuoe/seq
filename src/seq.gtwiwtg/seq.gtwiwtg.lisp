(defpackage #:com.inuoe.seq.gtwiwtg
  (:use #:cl)
  (:local-nicknames
   (#:seq #:com.inuoe.seq))
  (:export
   #:to-generator))

(in-package #:com.inuoe.seq.gtwiwtg)

(defmethod seq:col-seq ((obj gtwiwtg::generator!))
  (when (gtwiwtg::has-next-p obj)
    (cons (gtwiwtg::next obj)
          (seq:lazy-seq obj))))

(defun to-generator (col)
  (let ((seq (seq:col-seq col)))
    (gtwiwtg:from-thunk-until (lambda ()
                                (prog1 (seq:seq-first seq)
                                  (setf seq (seq:col-seq (seq:seq-rest seq)))))
                              :until (lambda () (null seq)))))
