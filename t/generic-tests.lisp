(defpackage #:com.inuoe.seqio-tests.generic-tests
  (:use
   #:cl
   #:com.inuoe.seq
   #:com.inuoe.seqio
   #:com.inuoe.seqio-tests)
  (:import-from
   #:fiveam
   #:def-suite
   #:in-suite
   #:test
   #:is
   #:is-true
   #:is-false
   #:signals)
  (:export
   #:seqio.generic))

(in-package #:com.inuoe.seqio-tests.generic-tests)

(defstruct (%test-seq
            (:conc-name nil)
            (:constructor %make-seq (%test-seq-elements))
            (:copier nil)
            (:predicate nil))
  (%test-seq-elements nil :type list :read-only t))

(defmethod mapcol (fn (seq %test-seq))
  (mapc fn (%test-seq-elements seq))
  (values))

(defmethod col-seq ((col %test-seq))
  (%test-seq-elements col))

(defmethod seq-first ((col %test-seq))
  (car (%test-seq-elements col)))

(defmethod seq-rest ((col %test-seq))
  (cdr (%test-seq-elements col)))

(def-suite seqio.generic
  :description "Tests the generic implementations of seqio."
  :in seqio)

(in-suite seqio.generic)

(test generic.aggregate
  (is (equal 6 (aggregate (%make-seq '(0 2 4)) #'+)))
  (is (equal 0
                 (aggregate (%make-seq '(0)) (lambda (x y)
                                   (declare (ignore x y))
                                   (error "fail")))))
  (signals error
    (aggregate (%make-seq '()) #'+)))

(test generic.aggregate*
  (is (equal 6 (aggregate* (%make-seq '(0 2 4)) #'+ 0)))
  (is (equal 15 (aggregate* (%make-seq '(10)) #'+ 5)))
  (is (equal 5 (aggregate* (%make-seq '()) #'+ 5))))

(test generic.all
  (is-true (all (%make-seq '(0 2 4)) #'evenp))
  (is-false (all (%make-seq '(1 2 4)) #'evenp))
  (is-false (all (%make-seq '(0 1 4)) #'evenp))
  (is-false (all (%make-seq '(0 2 3)) #'evenp))
  (is-true (all (%make-seq '()) #'evenp))
  (is-true (all (%make-seq '(0)) #'evenp)))

(test generic.any
  (is-true (any (%make-seq '(0))))
  (is-false (any (%make-seq '()))))

(test generic.any*
  (is-true (any* (%make-seq '(0 2 4)) #'evenp))
  (is-true (any* (%make-seq '(1 2 4)) #'evenp))
  (is-true (any* (%make-seq '(0 3)) #'evenp))
  (is-false (any* (%make-seq '()) #'evenp))
  (is-true (any* (%make-seq '(0 1 nil 2)) #'null)))

(test generic.eappend
  (is (eequal '(0 1 2) (eappend (%make-seq '(0 1)) 2)))
  (is (eequal '(0) (eappend (%make-seq '()) 0))))

(test generic.batch
  (is (eequalp '(#(0) #(1) #(2) #(3)) (batch (%make-seq '(0 1 2 3)) 1)))
  (is (eequalp '(#(0 1) #(2 3)) (batch (%make-seq '(0 1 2 3)) 2)))
  (is (eequalp '(#(0 1 2) #(3)) (batch (%make-seq '(0 1 2 3)) 3)))
  (is (eequalp '(#(0 1 2 3)) (batch (%make-seq '(0 1 2 3)) 4))))

(test generic.concat
  (is (eequal '(0 1 2 3) (concat (%make-seq '(0 1)) (%make-seq '(2 3)))))
  (is (eequal '(0 1) (concat (%make-seq '(0 1)) (%make-seq '()))))
  (is (eequal '(2 3) (concat (%make-seq '()) (%make-seq '(2 3)))))
  (is (eequal '() (concat (%make-seq '()) (%make-seq '())))))

(test generic.consume
  ;; Need proper test for side-effects
  )

(test generic.contains
  (is-true (contains (%make-seq '(0 1 2)) 0))
  (is-true (contains (%make-seq '(0 1 2)) 1))
  (is-true (contains (%make-seq '(0 1 2)) 2))
  (is-true (contains (%make-seq '("Hello")) "Hello" #'string=))
  (signals type-error
    (contains (%make-seq '(0 1 2)) 0 #'string=))
  (is-false (contains (%make-seq '(0 1 2)) 5))
  (is-false (contains (%make-seq '("Hello")) 0))
  (is-true (contains (%make-seq '(5 1 nil 2)) nil)))

(test generic.ecount
  (is (= 0 (ecount (%make-seq '()))))
  (is (= 1 (ecount (%make-seq '(0)))))
  (is (= 2 (ecount (%make-seq '(0 0)))))
  (is (= 2 (ecount (%make-seq '(0 1))))))

(test generic.ecount*
  (is (= 0 (ecount* (%make-seq '()) #'evenp)))
  (is (= 1 (ecount* (%make-seq '(0)) #'evenp)))
  (is (= 2 (ecount* (%make-seq '(0 0)) #'evenp)))
  (is (= 1 (ecount* (%make-seq '(0 1)) #'evenp))))

(test generic.default-if-empty
  (is (eequal '(0) (default-if-empty (%make-seq '()) 0)))
  (is (eequal '(1) (default-if-empty (%make-seq '(1)) 0))))

(test generic.distinct
  (is (eset-equal '(0 1 2 3)
                      (distinct (%make-seq '(0 1 0 1 3 2)))))
  (is (= 4
             (length
              (to-list (distinct (%make-seq '(0 1 0 1 3 2))))))))

(test generic.element-at
  (is (= 0 (element-at (%make-seq '(0 5 7)) 0)))
  (is (= 5 (element-at (%make-seq '(0 5 7)) 1)))
  (is (= 7 (element-at (%make-seq '(0 5 7)) 2)))
  (is (eq :sentinel (element-at (%make-seq '(0 5 7)) 100 :sentinel)))
  (is (eq :sentinel (element-at (%make-seq '()) 0 :sentinel))))

(test generic.evaluate
  (is (eequal '(0 1 2)
                  (evaluate (%make-seq (list (lambda () 0) (lambda () 1) (lambda () 2))))))
  (is (eequal '(3 5 8)
                  (evaluate (%make-seq (list (lambda () 3) (lambda () 5) (lambda () 8))))))
  (is (eequal '()
                  (evaluate (%make-seq '())))))

(test generic.except
  (is (eequal '(1) (except (%make-seq '(1 2)) (%make-seq '(2)))))
  (is (eequal '() (except (%make-seq '(1 2)) (%make-seq '(1 2)))))
  (is (eequal '(1 2) (except (%make-seq '(1 2)) (%make-seq '()))))
  (is (eequal '() (except (%make-seq '()) (%make-seq '()))))
  (is (eequal '() (except (%make-seq '()) (%make-seq '(1 2))))))

(test generic.efirst
  (is (= 1 (efirst (%make-seq '(1 2)))))
  (is (= 5 (efirst (%make-seq '()) 5)))
  (is (= 0 (efirst (%make-seq '(0)) 5))))

(test generic.efirst*
  (is (= 5 (efirst* (%make-seq '(0 5 3 7)) #'oddp)))
  (is (= 0 (efirst* (%make-seq '(0 5 3 7)) #'evenp)))
  (is (eq nil (efirst* (%make-seq '(0 2 10 6)) #'oddp)))
  (is (eq :sentinel (efirst* (%make-seq '(0 2 10 6)) #'oddp :sentinel))))

(test generic.group-adjacent
  (is (eset-equal '((1 2) (0)) (group-adjacent '((0 . 1) (0 . 2) (1 . 0)) #'car :selector #'cdr)
                  :test #'eset-equal
                  :key #'to-list)))

(test generic.group-by
  (is
   (eset-equal
    '(("A" 2 1) ("C" 3) ("D" 4))
    (group-by
     '(("A" 2) ("A" 1) ("C" 3) ("D" 4)) #'car
     :test #'string=
     :result-selector (lambda (k e) (cons k (to-list e)))
     :selector #'cadr)
    :test (lambda (a b)
            (and (string= (car a) (car b))
                 (eset-equal (cdr a) (cdr b)))))))

(test generic.intersect
  (is (eset-equal '(1) (intersect (%make-seq '(1 2 3)) (%make-seq '(1)))))
  (is (eset-equal '(1 2 3) (intersect (%make-seq '(1 2 3)) (%make-seq '(1 2 3)))))
  (is (eset-equal '(3) (intersect (%make-seq '(1 2 3)) (%make-seq '(3)))))
  (is (eset-equal '() (intersect (%make-seq '(1 2 3)) (%make-seq '()))))
  (is (eset-equal '(1 2 3) (intersect (%make-seq '(1 2 3)) (%make-seq '(3 2 1)))))
  (is (eset-equal '() (intersect (%make-seq '()) (%make-seq '(1 2 3))))))

(test generic.elast
  (is (= 3 (elast (%make-seq '(1 2 3)))))
  (is (eq nil (elast (%make-seq '()))))
  (is (eq :sentinel (elast (%make-seq '()) :sentinel)))
  (is (= 0 (elast (%make-seq '(0)) :sentinel))))

(test generic.elast*
  (is (= 3 (elast* (%make-seq '(1 2 3)) #'identity)))
  (is (= 2 (elast* (%make-seq '(1 2 3)) #'evenp)))
  (is (eq nil (elast* (%make-seq '()) #'evenp)))
  (is (eq :sentinel (elast* (%make-seq '(1)) #'evenp :sentinel)))
  (is (eq :sentinel (elast* (%make-seq '()) #'evenp :sentinel))))

(test generic.order-by.sort-empty
  (is (eequal () (order-by () #'identity))))

(test generic.order-by.sort-numbers-identity
  (is (eequal '(1 2 3) (order-by '(3 2 1) #'identity))))

(test generic.order-by.sort-characters-code
  (is (eequal (list (code-char 1) (code-char 2) (code-char 3)) (order-by (list (code-char 2) (code-char 3) (code-char 1)) #'char-code))))

(test generic.order-by.sort-strings-length-minus
  (is (eequal '("short" "medium" "longlong") (order-by '("longlong" "short" "medium") #'identity (lambda (a b) (- (length a) (length b)))))))

(test generic.prepend
  (is (eequal '(0 1 2) (prepend (%make-seq '(1 2)) 0)))
  (is (eequal '(0) (prepend (%make-seq '()) 0))))

(test generic.ereverse
  (is (eequal '(4 3 2 1) (ereverse (%make-seq '(1 2 3 4)))))
  (is (eequal '(1) (ereverse (%make-seq '(1))))))

(test generic.run-length-encode.basic-encoding
  (is (eequal '((nil . 10)) (run-length-encode (make-list 10))))
  (is (eequal '((0 . 1) (1 . 1) (2 . 1)) (run-length-encode '(0 1 2)))))

(test generic.run-length-encode.limit-elements
  (is (eequal '((nil . 5) (nil . 5)) (run-length-encode (make-list 10) :limit 5))))

(test generic.select
  (is (eequal '(1 2 3) (select (%make-seq '(0 1 2)) #'1+)))
  (is (eequal '(1) (select (%make-seq '(0)) #'1+)))
  (is (eequal '() (select (%make-seq '()) #'1+))))

(test generic.select*
  (is (eequal '((0 . 0) (1 . 1) (2 . 2)) (select* (%make-seq '(0 1 2)) #'cons)))
  (is (eequal '((a . 0) (b . 1) (c . 2)) (select* (%make-seq '(a b c)) #'cons)))
  (is (eequal '() (select* (%make-seq '()) #'cons))))

(test generic.select-many
  (is (eequal '(1 2 2) (select-many (%make-seq '(0 1 2)) (lambda (v) (repeat v v)))))
  (is (eequal '(2 3 3) (select-many (%make-seq '(0 1 2)) (lambda (v) (repeat v v)) #'1+)))
  (is (eequal '() (select-many (%make-seq '()) (lambda (v) (repeat v v))))))

(test generic.select-many*
  (is (eequal '(1 2 2 2 2 2) (select-many* (%make-seq '(0 1 2 2)) #'repeat)))
  (is (eequal '(2 3 3 3 3 3) (select-many* (%make-seq '(0 1 2 2)) #'repeat #'1+)))
  (is (eequal '() (select-many* (%make-seq '()) #'repeat))))

(test generic.single
  (is (equal 0 (single (%make-seq '(0)))))
  (is (equal 0 (single (%make-seq '(0)) :sentinel)))
  (is (equal nil (single (%make-seq '()))))
  (is (equal :sentinel (single (%make-seq '()) :sentinel)))
  (signals error
    (single (%make-seq '(0 1))))
  (signals error
    (single (%make-seq '(0 1)) :sentinel)))

(test generic.single*
  (is (equal 0 (single* (%make-seq '(0)) #'evenp)))
  (is (equal 0 (single* (%make-seq '(0)) #'evenp :sentinel)))
  (is (equal 0 (single* (%make-seq '(-5 -1 0 5 1)) #'evenp)))
  (is (equal 0 (single* (%make-seq '(-5 -1 0 5 1)) #'evenp :sentinel)))
  (is (equal nil (single* (%make-seq '()) #'evenp)))
  (is (equal :sentinel (single* (%make-seq '()) #'evenp :sentinel)))
  (signals error
    (single* (%make-seq '(0 1 2)) #'evenp))
  (signals error
    (single* (%make-seq '(0 1 2)) #'evenp :sentinel)))

(test generic.skip
  (is (eequal '(0 1 2 3) (skip (%make-seq '(0 1 2 3)) 0)))
  (is (eequal '(2 3) (skip (%make-seq '(0 1 2 3)) 2)))
  (is (eequal '() (skip (%make-seq '(0 1 2 3)) 4)))
  (is (eequal '() (skip (%make-seq '(0 1 2 3)) 2121)))
  (is (eequal '(0 1 2 3) (skip (%make-seq '(0 1 2 3)) -1))))

(test generic.skip-last
  (is (eequal '(0 1 2 3) (skip-last (%make-seq '(0 1 2 3)) 0)))
  (is (eequal '(0 1 2) (skip-last (%make-seq '(0 1 2 3)) 1)))
  (is (eequal '(0 1) (skip-last (%make-seq '(0 1 2 3)) 2)))
  (is (eequal '() (skip-last (%make-seq '(0 1 2 3)) 4)))
  (is (eequal '() (skip-last (%make-seq '(0 1 2 3)) 2121))))

(test generic.skip-until
  (is (eequal '(0 1 2 3) (skip-until (%make-seq '(0 1 2 3)) #'evenp)))
  (is (eequal '(1 2 3) (skip-until (%make-seq '(0 1 2 3)) #'oddp)))
  (is (eequal '() (skip-until (%make-seq '(0 2 4 8)) #'oddp))))

(test generic.skip-while
  (is (eequal '(1 2 3) (skip-while (%make-seq '(0 1 2 3)) #'evenp)))
  (is (eequal '(0 1 2 3) (skip-while (%make-seq '(0 1 2 3)) #'oddp)))
  (is (eequal '(0 2 4 8) (skip-while (%make-seq '(0 2 4 8)) #'oddp))))

(test generic.take
  (is (eequal '() (take (%make-seq '(0 1 2 3)) 0)))
  (is (eequal '(0 1) (take (%make-seq '(0 1 2 3)) 2)))
  (is (eequal '(0 1 2 3) (take (%make-seq '(0 1 2 3)) 4)))
  (is (eequal '(0 1 2 3) (take (%make-seq '(0 1 2 3)) 41201))))

(test generic.take-every
  (signals error
    (to-list (take-every (%make-seq '(0 1 2 3)) 0)))
  (is (eequal '(0 1 2 3) (take-every (%make-seq '(0 1 2 3)) 1)))
  (is (eequal '(0 2) (take-every (%make-seq '(0 1 2 3)) 2)))
  (is (eequal '(0 3) (take-every (%make-seq '(0 1 2 3)) 3)))
  (is (eequal '(0) (take-every (%make-seq '(0 1 2 3)) 41201))))

(test generic.take-last
  (signals error
    (to-list (take-last (%make-seq '(0 1 2 3)) -12)))
  (is (eequal '() (take-last (%make-seq '(0 1 2 3)) 0)))
  (is (eequal '(2 3) (take-last (%make-seq '(0 1 2 3)) 2)))
  (is (eequal '(0 1 2 3) (take-last (%make-seq '(0 1 2 3)) 4)))
  (is (eequal '(0 1 2 3) (take-last (%make-seq '(0 1 2 3)) 1212))))

(test generic.take-until
  (is (eequal '() (take-until (%make-seq '(0 1 2 3)) #'evenp)))
  (is (eequal '(0) (take-until (%make-seq '(0 1 2 3)) #'oddp)))
  (is (eequal '(0 2 4 8) (take-until (%make-seq '(0 2 4 8)) #'oddp))))

(test generic.take-while
  (is (eequal '(0) (take-while (%make-seq '(0 1 2 3)) #'evenp)))
  (is (eequal '() (take-while (%make-seq '(0 1 2 3)) #'oddp)))
  (is (eequal '() (take-while (%make-seq '(0 2 4 8)) #'oddp)))
  (is (eequal '(0 2 4 8) (take-while (%make-seq '(0 2 4 8)) #'evenp))))

(test generic.then-by.car-then-cdr
  (is (equal '((0 . 1) (0 . 2) (1 . 0) (2 . 0))
             (to-list (then-by (order-by '((1 . 0) (0 . 2) (0 . 1) (2 . 0)) #'car) #'cdr)))))

(test generic.eunion
  (is (eset-equal '(0 1 2 3)
                      (eunion (%make-seq '(0 1 2 3)) (%make-seq '(0 1 2 3)))))
  (is (eset-equal '(0 1 2 3)
                      (eunion (%make-seq '()) (%make-seq '(0 1 2 2 3)))))
  (is (eset-equal '(0 1 2 3)
                      (eunion (%make-seq '(0 1 1 2 3)) (%make-seq '()))))
  (is (eset-equal '(0 1 2 3)
                      (eunion (%make-seq '(0 1 1)) (%make-seq '(3 2 3)))))
  (is (= 1
             (length (to-list (eunion (%make-seq '("foo" "FOO")) (%make-seq '("foo" "FOO")) #'string-equal))))))

(test generic.where
  (is (eequal '(0 2) (where (%make-seq '(0 1 2 3)) #'evenp)))
  (is (eequal '(1 3) (where (%make-seq '(0 1 2 3)) #'oddp)))
  (is (eequal '() (where (%make-seq '(0 2 4 8)) #'oddp)))
  (is (eequal '(0 2 4 8) (where (%make-seq '(0 2 4 8)) #'evenp))))

(test generic.window
  (is (eequalp '(#(0) #(1) #(2) #(3)) (window (%make-seq '(0 1 2 3)) 1)))
  (is (eequalp '(#(0 1) #(1 2) #(2 3)) (window (%make-seq '(0 1 2 3)) 2)))
  (is (eequalp '(#(0 1 2) #(1 2 3)) (window (%make-seq '(0 1 2 3)) 3)))
  (is (eequalp '(#(0 1 2 3)) (window (%make-seq '(0 1 2 3)) 4)))
  (is (eequalp '() (window (%make-seq '(0 1 2 3)) 5))))

(test generic.to-hash-table
  (is (eset-equal
           '((1 . 1) (2 . 2) (3 . 3))
           (to-hash-table '(1 2 3) #'identity)
           :test #'equal))
  (is (eset-equal
           '()
           (to-hash-table '() #'identity)
           :test #'equal))
  (is (eset-equal
           '((2 . 1) (3 . 2))
           (to-hash-table '(1 2) #'1+)
           :test #'equal))
  (is (eset-equal
           '((1 . 2) (2 . 3))
           (to-hash-table '(1 2) #'identity :selector #'1+)
           :test #'equal)))

(test generic.to-list
  (is (eequal '(1 2 3) (%make-seq '(1 2 3))))
  (is (eequal '() (%make-seq '())))
  (is (eequal '(0) (%make-seq '(0)))))

(test generic.to-vector
  (is (equalp #(1 2 3) (to-vector (%make-seq '(1 2 3)))))
  (is (equalp #() (to-vector (%make-seq '()))))
  (is (equalp #(0) (to-vector (%make-seq '(0))))))
