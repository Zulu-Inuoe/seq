(in-package #:enumerable-tests)

(defclass %test-enumerable ()
  ((%enumerable
     :initform nil
     :initarg :enumerable
     :reader %test-enumerable-enumerable))
  (:documentation
   "Wrapper for an enumerable in order to dispatch on the non-specialized implementations."))

(defmethod get-enumerator ((enumerable %test-enumerable))
  (get-enumerator (%test-enumerable-enumerable enumerable)))

(defun %make-e (enumerable)
  (make-instance '%test-enumerable :enumerable enumerable))

(5am:def-suite expressions.generic
  :description "Tests the generic implementations of enumerable expressions."
  :in expressions)

(5am:in-suite expressions.generic)

(5am:test generic.aggregate
  (5am:is (equal 6 (aggregate (%make-e '(0 2 4)) #'+)))
  (5am:is (equal 0
                 (aggregate (%make-e '(0)) (lambda (x y)
                                   (declare (ignore x y))
                                   (error "fail")))))
  (5am:signals error
    (aggregate (%make-e '()) #'+)))

(5am:test generic.aggregate*
  (5am:is (equal 6 (aggregate* (%make-e '(0 2 4)) #'+ 0)))
  (5am:is (equal 15 (aggregate* (%make-e '(10)) #'+ 5)))
  (5am:is (equal 5 (aggregate* (%make-e '()) #'+ 5))))

(5am:test generic.all
  (5am:is-true (all (%make-e '(0 2 4)) #'evenp))
  (5am:is-false (all (%make-e '(1 2 4)) #'evenp))
  (5am:is-false (all (%make-e '(0 1 4)) #'evenp))
  (5am:is-false (all (%make-e '(0 2 3)) #'evenp))
  (5am:is-true (all (%make-e '()) #'evenp))
  (5am:is-true (all (%make-e '(0)) #'evenp)))

(5am:test generic.any
  (5am:is-true (any (%make-e '(0))))
  (5am:is-false (any (%make-e '()))))

(5am:test generic.any*
  (5am:is-true (any* (%make-e '(0 2 4)) #'evenp))
  (5am:is-true (any* (%make-e '(1 2 4)) #'evenp))
  (5am:is-true (any* (%make-e '(0 3)) #'evenp))
  (5am:is-false (any* (%make-e '()) #'evenp))
  (5am:is-true (any* (%make-e '(0 1 nil 2)) #'null)))

(5am:test generic.eappend
  (5am:is (equal '(0 1 2) (to-list (eappend (%make-e '(0 1)) 2))))
  (5am:is (equal '(0) (to-list (eappend (%make-e '()) 0)))))

(5am:test generic.batch
  (5am:is (equalp '(#(0) #(1) #(2) #(3)) (to-list (batch (%make-e '(0 1 2 3)) 1))))
  (5am:is (equalp '(#(0 1) #(2 3)) (to-list (batch (%make-e '(0 1 2 3)) 2))))
  (5am:is (equalp '(#(0 1 2) #(3)) (to-list (batch (%make-e '(0 1 2 3)) 3))))
  (5am:is (equalp '(#(0 1 2 3)) (to-list (batch (%make-e '(0 1 2 3)) 4)))))

(5am:test generic.concat
  (5am:is (equal '(0 1 2 3) (to-list (concat (%make-e '(0 1)) (%make-e '(2 3))))))
  (5am:is (equal '(0 1) (to-list (concat (%make-e '(0 1)) (%make-e '())))))
  (5am:is (equal '(2 3) (to-list (concat (%make-e '()) (%make-e '(2 3))))))
  (5am:is (equal '() (to-list (concat (%make-e '()) (%make-e '()))))))

(5am:test generic.consume
  ;; Need proper test for side-effects
  )

(5am:test generic.contains
  (5am:is-true (contains (%make-e '(0 1 2)) 0))
  (5am:is-true (contains (%make-e '(0 1 2)) 1))
  (5am:is-true (contains (%make-e '(0 1 2)) 2))
  (5am:is-true (contains (%make-e '("Hello")) "Hello" #'string=))
  (5am:signals type-error
    (contains (%make-e '(0 1 2)) 0 #'string=))
  (5am:is-false (contains (%make-e '(0 1 2)) 5))
  (5am:is-false (contains (%make-e '("Hello")) 0))
  (5am:is-true (contains (%make-e '(5 1 nil 2)) nil)))

(5am:test generic.ecount
  (5am:is (= 0 (ecount (%make-e '()))))
  (5am:is (= 1 (ecount (%make-e '(0)))))
  (5am:is (= 2 (ecount (%make-e '(0 0)))))
  (5am:is (= 2 (ecount (%make-e '(0 1))))))

(5am:test generic.ecount*
  (5am:is (= 0 (ecount* (%make-e '()) #'evenp)))
  (5am:is (= 1 (ecount* (%make-e '(0)) #'evenp)))
  (5am:is (= 2 (ecount* (%make-e '(0 0)) #'evenp)))
  (5am:is (= 1 (ecount* (%make-e '(0 1)) #'evenp))))

(5am:test generic.default-if-empty
  (5am:is (equal '(0) (to-list (default-if-empty (%make-e '()) 0))))
  (5am:is (equal '(1) (to-list (default-if-empty (%make-e '(1)) 0)))))

(5am:test generic.distinct
  (5am:is (set-equal '(0 1 2 3)
                     (to-list (distinct (%make-e '(0 1 0 1 3 2))))))
  (5am:is (= 4
             (length
              (to-list (distinct (%make-e '(0 1 0 1 3 2))))))))

(5am:test generic.element-at
  (5am:is (= 0 (element-at (%make-e '(0 5 7)) 0)))
  (5am:is (= 5 (element-at (%make-e '(0 5 7)) 1)))
  (5am:is (= 7 (element-at (%make-e '(0 5 7)) 2)))
  (5am:is (eq :sentinel (element-at (%make-e '(0 5 7)) 100 :sentinel)))
  (5am:is (eq :sentinel (element-at (%make-e '()) 0 :sentinel))))

(5am:test generic.evaluate
  (5am:is (equal '(0 1 2)
                 (to-list (evaluate (%make-e (list (lambda () 0) (lambda () 1) (lambda () 2)))))))
  (5am:is (equal '(3 5 8)
                 (to-list (evaluate (%make-e (list (lambda () 3) (lambda () 5) (lambda () 8)))))))
  (5am:is (equal '()
                 (to-list (evaluate (%make-e '()))))))

(5am:test generic.except
  (5am:is (equal '(1) (to-list (except (%make-e '(1 2)) (%make-e '(2))))))
  (5am:is (equal '() (to-list (except (%make-e '(1 2)) (%make-e '(1 2))))))
  (5am:is (equal '(1 2) (to-list (except (%make-e '(1 2)) (%make-e '())))))
  (5am:is (equal '() (to-list (except (%make-e '()) (%make-e '())))))
  (5am:is (equal '() (to-list (except (%make-e '()) (%make-e '(1 2)))))))

(5am:test generic.efirst
  (5am:is (= 1 (efirst (%make-e '(1 2)))))
  (5am:is (= 5 (efirst (%make-e '()) 5)))
  (5am:is (= 0 (efirst (%make-e '(0)) 5))))

(5am:test generic.efirst*
  (5am:is (= 5 (efirst* (%make-e '(0 5 3 7)) #'oddp)))
  (5am:is (= 0 (efirst* (%make-e '(0 5 3 7)) #'evenp)))
  (5am:is (eq nil (efirst* (%make-e '(0 2 10 6)) #'oddp)))
  (5am:is (eq :sentinel (efirst* (%make-e '(0 2 10 6)) #'oddp :sentinel))))

(5am:test generic.group-by
  (5am:is
   (set-equal
    '(("A" 2 1) ("C" 3) ("D" 4))
    (to-list
     (group-by
      '(("A" 2) ("A" 1) ("C" 3) ("D" 4)) #'car
      :test #'string=
      :result-selector (lambda (k e) (cons k (to-list e)))
      :selector #'cadr))
    :test (lambda (a b)
            (and (string= (car a) (car b))
                 (set-equal (cdr a) (cdr b)))))))

(5am:test generic.intersect
  (5am:is (set-equal '(1) (to-list (intersect (%make-e '(1 2 3)) (%make-e '(1))))))
  (5am:is (set-equal '(1 2 3) (to-list (intersect (%make-e '(1 2 3)) (%make-e '(1 2 3))))))
  (5am:is (set-equal '(3) (to-list (intersect (%make-e '(1 2 3)) (%make-e '(3))))))
  (5am:is (set-equal '() (to-list (intersect (%make-e '(1 2 3)) (%make-e '())))))
  (5am:is (set-equal '(1 2 3) (to-list (intersect (%make-e '(1 2 3)) (%make-e '(3 2 1))))))
  (5am:is (set-equal '() (to-list (intersect (%make-e '()) (%make-e '(1 2 3)))))))

(5am:test generic.elast
  (5am:is (= 3 (elast (%make-e '(1 2 3)))))
  (5am:is (eq nil (elast (%make-e '()))))
  (5am:is (eq :sentinel (elast (%make-e '()) :sentinel)))
  (5am:is (= 0 (elast (%make-e '(0)) :sentinel))))

(5am:test generic.elast*
  (5am:is (= 3 (elast* (%make-e '(1 2 3)) #'identity)))
  (5am:is (= 2 (elast* (%make-e '(1 2 3)) #'evenp)))
  (5am:is (eq nil (elast* (%make-e '()) #'evenp)))
  (5am:is (eq :sentinel (elast* (%make-e '(1)) #'evenp :sentinel)))
  (5am:is (eq :sentinel (elast* (%make-e '()) #'evenp :sentinel))))

(5am:test generic.order-by.sort-empty
  (5am:is (equal () (to-list (order-by () #'identity)))))

(5am:test generic.order-by.sort-numbers-identity
  (5am:is (equal '(1 2 3) (to-list (order-by '(3 2 1) #'identity)))))

(5am:test generic.order-by.sort-characters-code
  (5am:is (equal '#.(list (code-char 1) (code-char 2) (code-char 3)) (to-list (order-by '#.(list (code-char 2) (code-char 3) (code-char 1)) #'char-code)))))

(5am:test generic.order-by.sort-strings-length-minus
  (5am:is (equal '("short" "medium" "longlong") (to-list (order-by '("longlong" "short" "medium") #'identity (lambda (a b) (- (length a) (length b))))))))

(5am:test generic.prepend
  (5am:is (equal '(0 1 2) (to-list (prepend (%make-e '(1 2)) 0))))
  (5am:is (equal '(0) (to-list (prepend (%make-e '()) 0)))))

(5am:test generic.select
  (5am:is (equal '(1 2 3) (to-list (select (%make-e '(0 1 2)) #'1+))))
  (5am:is (equal '(1) (to-list (select (%make-e '(0)) #'1+))))
  (5am:is (equal '() (to-list (select (%make-e '()) #'1+)))))

(5am:test generic.select*
  (5am:is (equal '((0 . 0) (1 . 1) (2 . 2)) (to-list (select* (%make-e '(0 1 2)) #'cons))))
  (5am:is (equal '((a . 0) (b . 1) (c . 2)) (to-list (select* (%make-e '(a b c)) #'cons))))
  (5am:is (equal '() (to-list (select* (%make-e '()) #'cons)))))

(5am:test generic.select-many
  (5am:is (equal '(1 2 2) (to-list (select-many (%make-e '(0 1 2)) (lambda (v) (repeat v v))))))
  (5am:is (equal '(2 3 3) (to-list (select-many (%make-e '(0 1 2)) (lambda (v) (repeat v v)) #'1+))))
  (5am:is (equal '() (to-list (select-many (%make-e '()) (lambda (v) (repeat v v)))))))

(5am:test generic.select-many*
  (5am:is (equal '(1 2 2 2 2 2) (to-list (select-many* (%make-e '(0 1 2 2)) #'repeat))))
  (5am:is (equal '(2 3 3 3 3 3) (to-list (select-many* (%make-e '(0 1 2 2)) #'repeat #'1+))))
  (5am:is (equal '() (to-list (select-many* (%make-e '()) #'repeat)))))

(5am:test generic.single
  (5am:is (equal 0 (single (%make-e '(0)))))
  (5am:is (equal 0 (single (%make-e '(0)) :sentinel)))
  (5am:is (equal nil (single (%make-e '()))))
  (5am:is (equal :sentinel (single (%make-e '()) :sentinel)))
  (5am:signals error
    (single (%make-e '(0 1))))
  (5am:signals error
    (single (%make-e '(0 1)) :sentinel)))

(5am:test generic.single*
  (5am:is (equal 0 (single* (%make-e '(0)) #'evenp)))
  (5am:is (equal 0 (single* (%make-e '(0)) #'evenp :sentinel)))
  (5am:is (equal 0 (single* (%make-e '(-5 -1 0 5 1)) #'evenp)))
  (5am:is (equal 0 (single* (%make-e '(-5 -1 0 5 1)) #'evenp :sentinel)))
  (5am:is (equal nil (single* (%make-e '()) #'evenp)))
  (5am:is (equal :sentinel (single* (%make-e '()) #'evenp :sentinel)))
  (5am:signals error
    (single* (%make-e '(0 1 2)) #'evenp))
  (5am:signals error
    (single* (%make-e '(0 1 2)) #'evenp :sentinel)))

(5am:test generic.skip
  (5am:is (equal '(0 1 2 3) (to-list (skip (%make-e '(0 1 2 3)) 0))))
  (5am:is (equal '(2 3) (to-list (skip (%make-e '(0 1 2 3)) 2))))
  (5am:is (equal '() (to-list (skip (%make-e '(0 1 2 3)) 4))))
  (5am:is (equal '() (to-list (skip (%make-e '(0 1 2 3)) 2121))))
  (5am:is (equal '(0 1 2 3) (to-list (skip (%make-e '(0 1 2 3)) -1)))))

(5am:test generic.skip-last
  (5am:is (equal '(0 1 2 3) (to-list (skip-last (%make-e '(0 1 2 3)) 0))))
  (5am:is (equal '(0 1 2) (to-list (skip-last (%make-e '(0 1 2 3)) 1))))
  (5am:is (equal '(0 1) (to-list (skip-last (%make-e '(0 1 2 3)) 2))))
  (5am:is (equal '() (to-list (skip-last (%make-e '(0 1 2 3)) 4))))
  (5am:is (equal '() (to-list (skip-last (%make-e '(0 1 2 3)) 2121)))))

(5am:test generic.skip-until
  (5am:is (equal '(0 1 2 3) (to-list (skip-until (%make-e '(0 1 2 3)) #'evenp))))
  (5am:is (equal '(1 2 3) (to-list (skip-until (%make-e '(0 1 2 3)) #'oddp))))
  (5am:is (equal '() (to-list (skip-until (%make-e '(0 2 4 8)) #'oddp)))))

(5am:test generic.skip-while
  (5am:is (equal '(1 2 3) (to-list (skip-while (%make-e '(0 1 2 3)) #'evenp))))
  (5am:is (equal '(0 1 2 3) (to-list (skip-while (%make-e '(0 1 2 3)) #'oddp))))
  (5am:is (equal '(0 2 4 8) (to-list (skip-while (%make-e '(0 2 4 8)) #'oddp)))))

(5am:test generic.take
  (5am:is (equal '() (to-list (take (%make-e '(0 1 2 3)) 0))))
  (5am:is (equal '(0 1) (to-list (take (%make-e '(0 1 2 3)) 2))))
  (5am:is (equal '(0 1 2 3) (to-list (take (%make-e '(0 1 2 3)) 4))))
  (5am:is (equal '(0 1 2 3) (to-list (take (%make-e '(0 1 2 3)) 41201)))))

(5am:test generic.take-every
  (5am:signals error
    (to-list (take-every (%make-e '(0 1 2 3)) 0)))
  (5am:is (equal '(0 1 2 3) (to-list (take-every (%make-e '(0 1 2 3)) 1))))
  (5am:is (equal '(0 2) (to-list (take-every (%make-e '(0 1 2 3)) 2))))
  (5am:is (equal '(0 3) (to-list (take-every (%make-e '(0 1 2 3)) 3))))
  (5am:is (equal '(0) (to-list (take-every (%make-e '(0 1 2 3)) 41201)))))

(5am:test generic.take-last
  (5am:signals error
    (to-list (take-last (%make-e '(0 1 2 3)) -12)))
  (5am:is (equal '() (to-list (take-last (%make-e '(0 1 2 3)) 0))))
  (5am:is (equal '(2 3) (to-list (take-last (%make-e '(0 1 2 3)) 2))))
  (5am:is (equal '(0 1 2 3) (to-list (take-last (%make-e '(0 1 2 3)) 4))))
  (5am:is (equal '(0 1 2 3) (to-list (take-last (%make-e '(0 1 2 3)) 1212)))))

(5am:test generic.take-until
  (5am:is (equal '() (to-list (take-until (%make-e '(0 1 2 3)) #'evenp))))
  (5am:is (equal '(0) (to-list (take-until (%make-e '(0 1 2 3)) #'oddp))))
  (5am:is (equal '(0 2 4 8) (to-list (take-until (%make-e '(0 2 4 8)) #'oddp)))))

(5am:test generic.take-while
  (5am:is (equal '(0) (to-list (take-while (%make-e '(0 1 2 3)) #'evenp))))
  (5am:is (equal '() (to-list (take-while (%make-e '(0 1 2 3)) #'oddp))))
  (5am:is (equal '() (to-list (take-while (%make-e '(0 2 4 8)) #'oddp))))
  (5am:is (equal '(0 2 4 8) (to-list (take-while (%make-e '(0 2 4 8)) #'evenp)))))

(5am:test generic.eunion
  (5am:is (set-equal '(0 1 2 3)
                     (to-list (eunion (%make-e '(0 1 2 3)) (%make-e '(0 1 2 3))))))
  (5am:is (set-equal '(0 1 2 3)
                     (to-list (eunion (%make-e '()) (%make-e '(0 1 2 2 3))))))
  (5am:is (set-equal '(0 1 2 3)
                     (to-list (eunion (%make-e '(0 1 1 2 3)) (%make-e '())))))
  (5am:is (set-equal '(0 1 2 3)
                     (to-list (eunion (%make-e '(0 1 1)) (%make-e '(3 2 3))))))
  (5am:is (= 1
             (length (to-list (eunion (%make-e '("foo" "FOO")) (%make-e '("foo" "FOO")) #'string-equal))))))

(5am:test generic.where
  (5am:is (equal '(0 2) (to-list (where (%make-e '(0 1 2 3)) #'evenp))))
  (5am:is (equal '(1 3) (to-list (where (%make-e '(0 1 2 3)) #'oddp))))
  (5am:is (equal '() (to-list (where (%make-e '(0 2 4 8)) #'oddp))))
  (5am:is (equal '(0 2 4 8) (to-list (where (%make-e '(0 2 4 8)) #'evenp)))))

(5am:test generic.window
  (5am:is (equalp '(#(0) #(1) #(2) #(3)) (to-list (window (%make-e '(0 1 2 3)) 1))))
  (5am:is (equalp '(#(0 1) #(1 2) #(2 3)) (to-list (window (%make-e '(0 1 2 3)) 2))))
  (5am:is (equalp '(#(0 1 2) #(1 2 3)) (to-list (window (%make-e '(0 1 2 3)) 3))))
  (5am:is (equalp '(#(0 1 2 3)) (to-list (window (%make-e '(0 1 2 3)) 4))))
  (5am:is (equalp '() (to-list (window (%make-e '(0 1 2 3)) 5)))))

(5am:test generic.to-hash-table
  (5am:is (set-equal
           '((1 . 1) (2 . 2) (3 . 3))
           (to-list (to-hash-table '(1 2 3) #'identity))
           :test #'equal))
  (5am:is (set-equal
           '()
           (to-list (to-hash-table '() #'identity))
           :test #'equal))
  (5am:is (set-equal
           '((2 . 1) (3 . 2))
           (to-list (to-hash-table '(1 2) #'1+))
           :test #'equal))
  (5am:is (set-equal
           '((1 . 2) (2 . 3))
           (to-list (to-hash-table '(1 2) #'identity :selector #'1+))
           :test #'equal)))

(5am:test generic.to-list
  (5am:is (equal '(1 2 3) (to-list (%make-e '(1 2 3)))))
  (5am:is (equal '() (to-list (%make-e '()))))
  (5am:is (equal '(0) (to-list (%make-e '(0))))))

(5am:test generic.to-vector
  (5am:is (equalp #(1 2 3) (to-vector (%make-e '(1 2 3)))))
  (5am:is (equalp #() (to-vector (%make-e '()))))
  (5am:is (equalp #(0) (to-vector (%make-e '(0))))))
