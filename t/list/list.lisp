(in-package #:enumerable-tests.list)

(5am:def-suite enumerable.list
  :description "Tests for enumerable interface implementation for lists."
  :in enumerable)

(5am:in-suite enumerable.list)

(5am:test list.get-enumerator
  (5am:finishes
    (let ((enumerator (get-enumerator '(0 1 2 3))))
      (5am:is (move-next enumerator))
      (5am:is (= 0 (current enumerator)))
      (5am:is (move-next enumerator))
      (5am:is (= 1 (current enumerator)))
      (5am:is (move-next enumerator))
      (5am:is (= 2 (current enumerator)))
      (5am:is (move-next enumerator))
      (5am:is (= 3 (current enumerator)))
      (5am:is-false (move-next enumerator)))
    (let ((enumerator (get-enumerator '())))
      (5am:is-false (move-next enumerator))
      (5am:is-false (move-next enumerator)))))

(5am:def-suite expressions.list
  :description "Tests the enumerable expressions with lists."
  :in expressions)

(5am:in-suite expressions.list)

(5am:test list.aggregate
  (5am:is (equal 6 (aggregate '(0 2 4) #'+)))
  (5am:is (equal 0
                 (aggregate '(0) (lambda (x y)
                                   (declare (ignore x y))
                                   (error "fail")))))
  (5am:signals error
    (aggregate '() #'+)))

(5am:test list.aggregate*
  (5am:is (equal 6 (aggregate* '(0 2 4) #'+ 0)))
  (5am:is (equal 15 (aggregate* '(10) #'+ 5)))
  (5am:is (equal 5 (aggregate* '() #'+ 5))))

(5am:test list.all
  (5am:is (all '(0 2 4) #'evenp))
  (5am:is-false (all '(1 2 4) #'evenp))
  (5am:is-false (all '(0 1 4) #'evenp))
  (5am:is-false (all '(0 2 3) #'evenp))
  (5am:is (all '() #'evenp))
  (5am:is (all '(0) #'evenp)))

(5am:test list.any
  (5am:is (any '(0)))
  (5am:is-false (any '())))

(5am:test list.any*
  (5am:is (any* '(0 2 4) #'evenp))
  (5am:is (any* '(1 2 4) #'evenp))
  (5am:is (any* '(0 3) #'evenp))
  (5am:is-false (any* '() #'evenp))
  (5am:is (any* '(0 1 nil 2) #'null)))

(5am:test list.eappend
  (5am:is (eequal '(0 1 2) (eappend '(0 1) 2)))
  (5am:is (eequal '(0) (eappend '() 0))))

(5am:test list.concat
  (5am:is (eequal '(0 1 2 3) (concat '(0 1) '(2 3))))
  (5am:is (eequal '(0 1) (concat '(0 1) '())))
  (5am:is (eequal '(2 3) (concat '() '(2 3))))
  (5am:is (eequal '() (concat '() '()))))

(5am:test list.consume
  ;;No observable behavior
  )

(5am:test list.contains
  (5am:is (contains '(0 1 2) 0))
  (5am:is (contains '(0 1 2) 1))
  (5am:is (contains '(0 1 2) 2))
  (5am:is (contains '("Hello") "Hello" #'string=))
  (5am:signals type-error
    (contains '(0 1 2) 0 #'string=))
  (5am:is-false (contains '(0 1 2) 5))
  (5am:is-false (contains '("Hello") 0))
  (5am:is (contains '(5 1 nil 2) nil)))

(5am:test list.ecount
  (5am:is (= 0 (ecount '())))
  (5am:is (= 1 (ecount '(0))))
  (5am:is (= 2 (ecount '(0 0))))
  (5am:is (= 2 (ecount '(0 1)))))

(5am:test list.ecount*
  (5am:is (= 0 (ecount* '() #'evenp)))
  (5am:is (= 1 (ecount* '(0) #'evenp)))
  (5am:is (= 2 (ecount* '(0 0) #'evenp)))
  (5am:is (= 1 (ecount* '(0 1) #'evenp))))

(5am:test list.default-if-empty
  (5am:is (eequal '(0) (default-if-empty '() 0)))
  (5am:is (eequal '(1) (default-if-empty '(1) 0))))

(5am:test list.distinct
  (5am:is (eset-equal '(0 1 2 3) (distinct '(0 1 0 1 3 2))))
  (5am:is (= 4
             (length
              (to-list (distinct '(0 1 0 1 3 2)))))))

(5am:test list.element-at
  (5am:is (= 0 (element-at '(0 5 7) 0)))
  (5am:is (= 5 (element-at '(0 5 7) 1)))
  (5am:is (= 7 (element-at '(0 5 7) 2)))
  (5am:is (eq :sentinel (element-at '(0 5 7) 100 :sentinel)))
  (5am:is (eq :sentinel (element-at '() 0 :sentinel))))

(5am:test list.evaluate
  (5am:is (eequal '(0 1 2)
                 (evaluate (list (lambda () 0) (lambda () 1) (lambda () 2)))))
  (5am:is (eequal '(3 5 8)
                 (evaluate (list (lambda () 3) (lambda () 5) (lambda () 8)))))
  (5am:is (eequal '()
                 (evaluate '()))))

(5am:test list.except
  (5am:is (eequal '(1) (except '(1 2) '(2))))
  (5am:is (eequal '() (except '(1 2) '(1 2))))
  (5am:is (eequal '(1 2) (except '(1 2) '())))
  (5am:is (eequal '() (except '() '())))
  (5am:is (eequal '() (except '() '(1 2)))))

(5am:test list.efirst
  (5am:is (= 1 (efirst '(1 2))))
  (5am:is (= 5 (efirst '() 5)))
  (5am:is (= 0 (efirst '(0) 5))))

(5am:test list.efirst*
  (5am:is (= 5 (efirst* '(0 5 3 7) #'oddp)))
  (5am:is (= 0 (efirst* '(0 5 3 7) #'evenp)))
  (5am:is (eq nil (efirst* '(0 2 10 6) #'oddp)))
  (5am:is (eq :sentinel (efirst* '(0 2 10 6) #'oddp :sentinel))))

(5am:test list.elast
  (5am:is (= 3 (elast '(1 2 3))))
  (5am:is (eq nil (elast '())))
  (5am:is (eq :sentinel (elast '() :sentinel)))
  (5am:is (= 0 (elast '(0) :sentinel))))

(5am:test list.elast*
  (5am:is (= 3 (elast* '(1 2 3) #'identity)))
  (5am:is (= 2 (elast* '(1 2 3) #'evenp)))
  (5am:is (eq nil (elast* '() #'evenp)))
  (5am:is (eq :sentinel (elast* '(1) #'evenp :sentinel)))
  (5am:is (eq :sentinel (elast* '() #'evenp :sentinel))))

(5am:test list.prepend
  (5am:is (eequal '(0 1 2) (prepend '(1 2) 0)))
  (5am:is (eequal '(0) (prepend '() 0))))

(5am:test list.select
  (5am:is (eequal '(1 2 3) (select '(0 1 2) #'1+)))
  (5am:is (eequal '(1) (select '(0) #'1+)))
  (5am:is (eequal '() (select '() #'1+))))

(5am:test list.select*
  (5am:is (eequal '((0 . 0) (1 . 1) (2 . 2)) (select* '(0 1 2) #'cons)))
  (5am:is (eequal '((a . 0) (b . 1) (c . 2)) (select* '(a b c) #'cons)))
  (5am:is (eequal '() (select* '() #'cons))))

(5am:test list.select-many
  (5am:is (eequal '(1 2 2) (select-many '(0 1 2) (lambda (v) (repeat v v)))))
  (5am:is (eequal '(2 3 3) (select-many '(0 1 2) (lambda (v) (repeat v v)) #'1+)))
  (5am:is (eequal '() (select-many '() (lambda (v) (repeat v v))))))

(5am:test list.select-many*
  (5am:is (eequal '(1 2 2 2 2 2) (select-many* '(0 1 2 2) #'repeat)))
  (5am:is (eequal '(2 3 3 3 3 3) (select-many* '(0 1 2 2) #'repeat #'1+)))
  (5am:is (eequal '() (select-many* '() #'repeat))))

(5am:test list.single
  (5am:is (equal 0 (single '(0))))
  (5am:is (equal 0 (single '(0) :sentinel)))
  (5am:is (equal nil (single '())))
  (5am:is (equal :sentinel (single '() :sentinel)))
  (5am:signals error
    (single '(0 1)))
  (5am:signals error
    (single '(0 1) :sentinel)))

(5am:test list.single*
  (5am:is (equal 0 (single* '(0) #'evenp)))
  (5am:is (equal 0 (single* '(0) #'evenp :sentinel)))
  (5am:is (equal 0 (single* '(-5 -1 0 5 1) #'evenp)))
  (5am:is (equal 0 (single* '(-5 -1 0 5 1) #'evenp :sentinel)))
  (5am:is (equal nil (single* '() #'evenp)))
  (5am:is (equal :sentinel (single* '() #'evenp :sentinel)))
  (5am:signals error
    (single* '(0 1 2) #'evenp))
  (5am:signals error
    (single* '(0 1 2) #'evenp :sentinel)))

(5am:test list.skip
  (5am:is (eequal '(0 1 2 3) (skip '(0 1 2 3) 0)))
  (5am:is (eequal '(2 3) (skip '(0 1 2 3) 2)))
  (5am:is (eequal '() (skip '(0 1 2 3) 4)))
  (5am:is (eequal '() (skip '(0 1 2 3) 2121))))

(5am:test list.skip-last
  (5am:is (eequal '(0 1 2 3) (skip-last '(0 1 2 3) 0)))
  (5am:is (eequal '(0 1 2) (skip-last '(0 1 2 3) 1)))
  (5am:is (eequal '(0 1) (skip-last '(0 1 2 3) 2)))
  (5am:is (eequal '() (skip-last '(0 1 2 3) 4)))
  (5am:is (eequal '() (skip-last '(0 1 2 3) 2121))))

(5am:test list.skip-until
  (5am:is (eequal '(0 1 2 3) (skip-until '(0 1 2 3) #'evenp)))
  (5am:is (eequal '(1 2 3) (skip-until '(0 1 2 3) #'oddp)))
  (5am:is (eequal '() (skip-until '(0 2 4 8) #'oddp))))

(5am:test list.skip-while
  (5am:is (eequal '(1 2 3) (skip-while '(0 1 2 3) #'evenp)))
  (5am:is (eequal '(0 1 2 3) (skip-while '(0 1 2 3) #'oddp)))
  (5am:is (eequal '(0 2 4 8) (skip-while '(0 2 4 8) #'oddp))))

(5am:test list.take
  (5am:is (eequal '() (take '(0 1 2 3) 0)))
  (5am:is (eequal '(0 1) (take '(0 1 2 3) 2)))
  (5am:is (eequal '(0 1 2 3) (take '(0 1 2 3) 4)))
  (5am:is (equal '(0 1 2 3) (take '(0 1 2 3) 41201))))

(5am:test list.take-every
  (5am:signals error
    (to-list (take-every '(0 1 2 3) 0)))
  (5am:is (eequal '(0 1 2 3) (take-every '(0 1 2 3) 1)))
  (5am:is (eequal '(0 2) (take-every '(0 1 2 3) 2)))
  (5am:is (eequal '(0 3) (take-every '(0 1 2 3) 3)))
  (5am:is (eequal '(0) (take-every '(0 1 2 3) 41201))))

(5am:test list.take-last
  (5am:signals error
    (to-list (take-last '(0 1 2 3) -12)))
  (5am:is (eequal '() (take-last '(0 1 2 3) 0)))
  (5am:is (eequal '(2 3) (take-last '(0 1 2 3) 2)))
  (5am:is (eequal '(0 1 2 3) (take-last '(0 1 2 3) 4)))
  (5am:is (eequal '(0 1 2 3) (take-last '(0 1 2 3) 1212))))

(5am:test list.take-until
  (5am:is (eequal '() (take-until '(0 1 2 3) #'evenp)))
  (5am:is (eequal '(0) (take-until '(0 1 2 3) #'oddp)))
  (5am:is (eequal '(0 2 4 8) (take-until '(0 2 4 8) #'oddp))))

(5am:test list.take-while
  (5am:is (eequal '(0) (take-while '(0 1 2 3) #'evenp)))
  (5am:is (eequal '() (take-while '(0 1 2 3) #'oddp)))
  (5am:is (eequal '() (take-while '(0 2 4 8) #'oddp)))
  (5am:is (eequal '(0 2 4 8) (take-while '(0 2 4 8) #'evenp))))

(5am:test list.where
  (5am:is (eequal '(0 2) (where '(0 1 2 3) #'evenp)))
  (5am:is (eequal '(1 3) (where '(0 1 2 3) #'oddp)))
  (5am:is (eequal '() (where '(0 2 4 8) #'oddp)))
  (5am:is (eequal '(0 2 4 8) (where '(0 2 4 8) #'evenp))))

(5am:test list.to-list
  (5am:is (equal '(1 2 3) (to-list '(1 2 3))))
  (5am:is (equal '() (to-list '())))
  (5am:is (equal '(0) (to-list '(0)))))

(5am:test list.to-vector
  (5am:is (equalp #(1 2 3) (to-vector '(1 2 3))))
  (5am:is (equalp #() (to-vector '())))
  (5am:is (equalp #(0) (to-vector '(0)))))
