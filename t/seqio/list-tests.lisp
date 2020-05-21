(defpackage #:com.inuoe.seqio-tests.list-tests
  (:use
   #:alexandria
   #:cl
   #:com.inuoe.seqio
   #:com.inuoe.seqio-tests)
  (:import-from
   #:fiveam
   #:def-suite
   #:in-suite
   #:test
   #:is
   #:is-false
   #:signals)
  (:export
   ;;; Test suites
   #:seqio.list))

(in-package #:com.inuoe.seqio-tests.list-tests)

(def-suite seqio.list
  :description "Tests seqio with lists."
  :in seqio)

(in-suite seqio.list)

(test list.aggregate
  (is (equal 6 (aggregate '(0 2 4) #'+)))
  (is (equal 0
                 (aggregate '(0) (lambda (x y)
                                   (declare (ignore x y))
                                   (error "fail")))))
  (signals error
    (aggregate '() #'+)))

(test list.aggregate*
  (is (equal 6 (aggregate* '(0 2 4) #'+ 0)))
  (is (equal 15 (aggregate* '(10) #'+ 5)))
  (is (equal 5 (aggregate* '() #'+ 5))))

(test list.all
  (is (all '(0 2 4) #'evenp))
  (is-false (all '(1 2 4) #'evenp))
  (is-false (all '(0 1 4) #'evenp))
  (is-false (all '(0 2 3) #'evenp))
  (is (all '() #'evenp))
  (is (all '(0) #'evenp)))

(test list.any
  (is (any '(0)))
  (is-false (any '())))

(test list.any*
  (is (any* '(0 2 4) #'evenp))
  (is (any* '(1 2 4) #'evenp))
  (is (any* '(0 3) #'evenp))
  (is-false (any* '() #'evenp))
  (is (any* '(0 1 nil 2) #'null)))

(test list.eappend
  (is (eequal '(0 1 2) (eappend '(0 1) 2)))
  (is (eequal '(0) (eappend '() 0))))

(test list.concat
  (is (eequal '(0 1 2 3) (concat '(0 1) '(2 3))))
  (is (eequal '(0 1) (concat '(0 1) '())))
  (is (eequal '(2 3) (concat '() '(2 3))))
  (is (eequal '() (concat '() '()))))

(test list.consume
  ;;No observable behavior
  )

(test list.contains
  (is (contains '(0 1 2) 0))
  (is (contains '(0 1 2) 1))
  (is (contains '(0 1 2) 2))
  (is (contains '("Hello") "Hello" #'string=))
  (signals type-error
    (contains '(0 1 2) 0 #'string=))
  (is-false (contains '(0 1 2) 5))
  (is-false (contains '("Hello") 0))
  (is (contains '(5 1 nil 2) nil)))

(test list.ecount
  (is (= 0 (ecount '())))
  (is (= 1 (ecount '(0))))
  (is (= 2 (ecount '(0 0))))
  (is (= 2 (ecount '(0 1)))))

(test list.ecount*
  (is (= 0 (ecount* '() #'evenp)))
  (is (= 1 (ecount* '(0) #'evenp)))
  (is (= 2 (ecount* '(0 0) #'evenp)))
  (is (= 1 (ecount* '(0 1) #'evenp))))

(test list.default-if-empty
  (is (eequal '(0) (default-if-empty '() 0)))
  (is (eequal '(1) (default-if-empty '(1) 0))))

(test list.distinct
  (is (eset-equal '(0 1 2 3) (distinct '(0 1 0 1 3 2))))
  (is (= 4
             (length
              (to-list (distinct '(0 1 0 1 3 2)))))))

(test list.element-at
  (is (= 0 (element-at '(0 5 7) 0)))
  (is (= 5 (element-at '(0 5 7) 1)))
  (is (= 7 (element-at '(0 5 7) 2)))
  (is (eq :sentinel (element-at '(0 5 7) 100 :sentinel)))
  (is (eq :sentinel (element-at '() 0 :sentinel))))

(test list.evaluate
  (is (eequal '(0 1 2)
                 (evaluate (list (lambda () 0) (lambda () 1) (lambda () 2)))))
  (is (eequal '(3 5 8)
                 (evaluate (list (lambda () 3) (lambda () 5) (lambda () 8)))))
  (is (eequal '()
                 (evaluate '()))))

(test list.except
  (is (eequal '(1) (except '(1 2) '(2))))
  (is (eequal '() (except '(1 2) '(1 2))))
  (is (eequal '(1 2) (except '(1 2) '())))
  (is (eequal '() (except '() '())))
  (is (eequal '() (except '() '(1 2)))))

(test list.efirst
  (is (= 1 (efirst '(1 2))))
  (is (= 5 (efirst '() 5)))
  (is (= 0 (efirst '(0) 5))))

(test list.efirst*
  (is (= 5 (efirst* '(0 5 3 7) #'oddp)))
  (is (= 0 (efirst* '(0 5 3 7) #'evenp)))
  (is (eq nil (efirst* '(0 2 10 6) #'oddp)))
  (is (eq :sentinel (efirst* '(0 2 10 6) #'oddp :sentinel))))

(test list.elast
  (is (= 3 (elast '(1 2 3))))
  (is (eq nil (elast '())))
  (is (eq :sentinel (elast '() :sentinel)))
  (is (= 0 (elast '(0) :sentinel))))

(test list.elast*
  (is (= 3 (elast* '(1 2 3) #'identity)))
  (is (= 2 (elast* '(1 2 3) #'evenp)))
  (is (eq nil (elast* '() #'evenp)))
  (is (eq :sentinel (elast* '(1) #'evenp :sentinel)))
  (is (eq :sentinel (elast* '() #'evenp :sentinel))))

(test list.prepend
  (is (eequal '(0 1 2) (prepend '(1 2) 0)))
  (is (eequal '(0) (prepend '() 0))))

(test list.select
  (is (eequal '(1 2 3) (select '(0 1 2) #'1+)))
  (is (eequal '(1) (select '(0) #'1+)))
  (is (eequal '() (select '() #'1+))))

(test list.select*
  (is (eequal '((0 . 0) (1 . 1) (2 . 2)) (select* '(0 1 2) #'cons)))
  (is (eequal '((a . 0) (b . 1) (c . 2)) (select* '(a b c) #'cons)))
  (is (eequal '() (select* '() #'cons))))

(test list.select-many
  (is (eequal '(1 2 2) (select-many '(0 1 2) (lambda (v) (repeat v v)))))
  (is (eequal '(2 3 3) (select-many '(0 1 2) (lambda (v) (repeat v v)) #'1+)))
  (is (eequal '() (select-many '() (lambda (v) (repeat v v))))))

(test list.select-many*
  (is (eequal '(1 2 2 2 2 2) (select-many* '(0 1 2 2) #'repeat)))
  (is (eequal '(2 3 3 3 3 3) (select-many* '(0 1 2 2) #'repeat #'1+)))
  (is (eequal '() (select-many* '() #'repeat))))

(test list.single
  (is (equal 0 (single '(0))))
  (is (equal 0 (single '(0) :sentinel)))
  (is (equal nil (single '())))
  (is (equal :sentinel (single '() :sentinel)))
  (signals error
    (single '(0 1)))
  (signals error
    (single '(0 1) :sentinel)))

(test list.single*
  (is (equal 0 (single* '(0) #'evenp)))
  (is (equal 0 (single* '(0) #'evenp :sentinel)))
  (is (equal 0 (single* '(-5 -1 0 5 1) #'evenp)))
  (is (equal 0 (single* '(-5 -1 0 5 1) #'evenp :sentinel)))
  (is (equal nil (single* '() #'evenp)))
  (is (equal :sentinel (single* '() #'evenp :sentinel)))
  (signals error
    (single* '(0 1 2) #'evenp))
  (signals error
    (single* '(0 1 2) #'evenp :sentinel)))

(test list.skip
  (is (eequal '(0 1 2 3) (skip '(0 1 2 3) 0)))
  (is (eequal '(2 3) (skip '(0 1 2 3) 2)))
  (is (eequal '() (skip '(0 1 2 3) 4)))
  (is (eequal '() (skip '(0 1 2 3) 2121))))

(test list.skip-last
  (is (eequal '(0 1 2 3) (skip-last '(0 1 2 3) 0)))
  (is (eequal '(0 1 2) (skip-last '(0 1 2 3) 1)))
  (is (eequal '(0 1) (skip-last '(0 1 2 3) 2)))
  (is (eequal '() (skip-last '(0 1 2 3) 4)))
  (is (eequal '() (skip-last '(0 1 2 3) 2121))))

(test list.skip-until
  (is (eequal '(0 1 2 3) (skip-until '(0 1 2 3) #'evenp)))
  (is (eequal '(1 2 3) (skip-until '(0 1 2 3) #'oddp)))
  (is (eequal '() (skip-until '(0 2 4 8) #'oddp))))

(test list.skip-while
  (is (eequal '(1 2 3) (skip-while '(0 1 2 3) #'evenp)))
  (is (eequal '(0 1 2 3) (skip-while '(0 1 2 3) #'oddp)))
  (is (eequal '(0 2 4 8) (skip-while '(0 2 4 8) #'oddp))))

(test list.take
  (is (eequal '() (take '(0 1 2 3) 0)))
  (is (eequal '(0 1) (take '(0 1 2 3) 2)))
  (is (eequal '(0 1 2 3) (take '(0 1 2 3) 4)))
  (is (eequal '(0 1 2 3) (take '(0 1 2 3) 41201))))

(test list.take-every
  (signals error
    (to-list (take-every '(0 1 2 3) 0)))
  (is (eequal '(0 1 2 3) (take-every '(0 1 2 3) 1)))
  (is (eequal '(0 2) (take-every '(0 1 2 3) 2)))
  (is (eequal '(0 3) (take-every '(0 1 2 3) 3)))
  (is (eequal '(0) (take-every '(0 1 2 3) 41201))))

(test list.take-last
  (signals error
    (to-list (take-last '(0 1 2 3) -12)))
  (is (eequal '() (take-last '(0 1 2 3) 0)))
  (is (eequal '(2 3) (take-last '(0 1 2 3) 2)))
  (is (eequal '(0 1 2 3) (take-last '(0 1 2 3) 4)))
  (is (eequal '(0 1 2 3) (take-last '(0 1 2 3) 1212))))

(test list.take-until
  (is (eequal '() (take-until '(0 1 2 3) #'evenp)))
  (is (eequal '(0) (take-until '(0 1 2 3) #'oddp)))
  (is (eequal '(0 2 4 8) (take-until '(0 2 4 8) #'oddp))))

(test list.take-while
  (is (eequal '(0) (take-while '(0 1 2 3) #'evenp)))
  (is (eequal '() (take-while '(0 1 2 3) #'oddp)))
  (is (eequal '() (take-while '(0 2 4 8) #'oddp)))
  (is (eequal '(0 2 4 8) (take-while '(0 2 4 8) #'evenp))))

(test list.where
  (is (eequal '(0 2) (where '(0 1 2 3) #'evenp)))
  (is (eequal '(1 3) (where '(0 1 2 3) #'oddp)))
  (is (eequal '() (where '(0 2 4 8) #'oddp)))
  (is (eequal '(0 2 4 8) (where '(0 2 4 8) #'evenp))))

(test list.to-list
  (is (equal '(1 2 3) (to-list '(1 2 3))))
  (is (equal '() (to-list '())))
  (is (equal '(0) (to-list '(0)))))

(test list.to-vector
  (is (equalp #(1 2 3) (to-vector '(1 2 3))))
  (is (equalp #() (to-vector '())))
  (is (equalp #(0) (to-vector '(0)))))
