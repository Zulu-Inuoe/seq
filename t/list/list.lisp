;;;enumerable-tests - tests for the enumerable library
;;;Written in 2018 by Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>
;;;
;;;To the extent possible under law, the author(s) have dedicated all copyright
;;;and related and neighboring rights to this software to the public domain
;;;worldwide. This software is distributed without any warranty.
;;;You should have received a copy of the CC0 Public Domain Dedication along
;;;with this software. If not, see
;;;<http://creativecommons.org/publicdomain/zero/1.0/>.

(in-package #:enumerable-tests.list)

(5am:def-suite expressions.list
  :description "Tests the enumerable expressions with lists."
  :in expressions)

(5am:in-suite expressions.list)

(5am:test aggregate
  (5am:is (equal (aggregate '(0 2 4) #'+) 6))
  (5am:is (equal (aggregate '(0) (lambda (x y)
                                   (declare (ignore x y))
                                   (error "fail")))
                 0))
  (5am:signals error
    (aggregate '() #'+)))

(5am:test aggregate*
  (5am:is (equal (aggregate* '(0 2 4) #'+ 0) 6))
  (5am:is (equal (aggregate* '(10) #'+ 5) 15))
  (5am:is (equal (aggregate* '() #'+ 5) 5)))

(5am:test all
  (5am:is (all '(0 2 4) #'evenp))
  (5am:is (not (all '(1 2 4) #'evenp)))
  (5am:is (not (all '(0 1 4) #'evenp)))
  (5am:is (not (all '(0 2 3) #'evenp)))
  (5am:is (all '() #'evenp))
  (5am:is (all '(0) #'evenp)))

(5am:test any
  (5am:is (any '(0)))
  (5am:is (not (any '()))))

(5am:test any*
  (5am:is (any* '(0 2 4) #'evenp))
  (5am:is (any* '(1 2 4) #'evenp))
  (5am:is (any* '(0 3) #'evenp))
  (5am:is (not (any* '() #'evenp)))
  (5am:is (any* '(0 1 nil 2) #'null)))

(5am:test eappend
  (5am:is (equal (to-list (eappend '(0 1) 2)) '(0 1 2)))
  (5am:is (equal (to-list (eappend '() 0)) '(0))))

(5am:test concat
  (5am:is (equal (to-list (concat '(0 1) '(2 3))) '(0 1 2 3)))
  (5am:is (equal (to-list (concat '(0 1) '())) '(0 1)))
  (5am:is (equal (to-list (concat '() '(2 3))) '(2 3)))
  (5am:is (equal (to-list (concat '() '())) '())))

(5am:test consume
  ;;No observable behavior
  )

(5am:test contains
  (5am:is (contains '(0 1 2) 0))
  (5am:is (contains '(0 1 2) 1))
  (5am:is (contains '(0 1 2) 2))
  (5am:is (contains '("Hello") "Hello" #'string=))
  (5am:signals type-error
    (contains '(0 1 2) 0 #'string=))
  (5am:is (not (contains '(0 1 2) 5)))
  (5am:is (not (contains '("Hello") 0)))
  (5am:is (contains '(5 1 nil 2) nil)))

(5am:test ecount
  (5am:is (= (ecount '()) 0))
  (5am:is (= (ecount '(0)) 1))
  (5am:is (= (ecount '(0 0)) 2))
  (5am:is (= (ecount '(0 1)) 2)))

(5am:test ecount*
  (5am:is (= (ecount* '() #'evenp) 0))
  (5am:is (= (ecount* '(0) #'evenp) 1))
  (5am:is (= (ecount* '(0 0) #'evenp) 2))
  (5am:is (= (ecount* '(0 1) #'evenp) 1)))

(5am:test default-if-empty
  (5am:is (equal (to-list (default-if-empty '() 0)) '(0)))
  (5am:is (equal (to-list (default-if-empty '(1) 0)) '(1))))

(5am:test distinct
  (5am:is (set-equal (to-list (distinct '(0 1 0 1 3 2)))
                     '(0 1 2 3)))
  (5am:is (= (length
              (to-list (distinct '(0 1 0 1 3 2))))
             4)))

(5am:test element-at
  (5am:is (= (element-at '(0 5 7) 0) 0))
  (5am:is (= (element-at '(0 5 7) 1) 5))
  (5am:is (= (element-at '(0 5 7) 2) 7))
  (5am:is (eq (element-at '(0 5 7) 100 :sentinel) :sentinel))
  (5am:is (eq (element-at '() 0 :sentinel) :sentinel)))

(5am:test evaluate
  (5am:is (equal (to-list (evaluate (list (lambda () 0) (lambda () 1) (lambda () 2))))
                 '(0 1 2)))
  (5am:is (equal (to-list (evaluate (list (lambda () 3) (lambda () 5) (lambda () 8))))
                 '(3 5 8)))
  (5am:is (equal (to-list (evaluate '()))
                 '())))

(5am:test except
  (5am:is (equal (to-list (except '(1 2) '(2))) '(1)))
  (5am:is (equal (to-list (except '(1 2) '(1 2))) '()))
  (5am:is (equal (to-list (except '(1 2) '())) '(1 2)))
  (5am:is (equal (to-list (except '() '())) '()))
  (5am:is (equal (to-list (except '() '(1 2))) '())))

(5am:test efirst
  (5am:is (= (efirst '(1 2)) 1))
  (5am:is (= (efirst '() 5) 5))
  (5am:is (= (efirst '(0) 5) 0)))

(5am:test efirst*
  (5am:is (= (efirst* '(0 5 3 7) #'oddp) 5))
  (5am:is (= (efirst* '(0 5 3 7) #'evenp) 0))
  (5am:is (eq (efirst* '(0 2 10 6) #'oddp) nil))
  (5am:is (eq (efirst* '(0 2 10 6) #'oddp :sentinel) :sentinel)))

(5am:test elast
  (5am:is (= (elast '(1 2 3)) 3))
  (5am:is (eq (elast '()) nil))
  (5am:is (eq (elast '() :sentinel) :sentinel))
  (5am:is (= (elast '(0) :sentinel) 0)))

(5am:test elast*
  (5am:is (= (elast* '(1 2 3) #'identity) 3))
  (5am:is (= (elast* '(1 2 3) #'evenp) 2))
  (5am:is (eq (elast* '() #'evenp) nil))
  (5am:is (eq (elast* '(1) #'evenp :sentinel) :sentinel))
  (5am:is (eq (elast* '() #'evenp :sentinel) :sentinel)))

(5am:test prepend
  (5am:is (equal (to-list (prepend '(1 2) 0)) '(0 1 2)))
  (5am:is (equal (to-list (prepend '() 0)) '(0))))

(5am:test select
  (5am:is (equal (to-list (select '(0 1 2) #'1+)) '(1 2 3)))
  (5am:is (equal (to-list (select '(0) #'1+)) '(1)))
  (5am:is (equal (to-list (select '() #'1+)) '())))

(5am:test select*
  (5am:is (equal (to-list (select* '(0 1 2) #'cons)) '((0 . 0) (1 . 1) (2 . 2))))
  (5am:is (equal (to-list (select* '(a b c) #'cons)) '((a . 0) (b . 1) (c . 2))))
  (5am:is (equal (to-list (select* '() #'cons)) '())))

(5am:test select-many
  (5am:is (equal (to-list (select-many '(0 1 2) (lambda (v) (repeat v v)))) '(1 2 2)))
  (5am:is (equal (to-list (select-many '(0 1 2) (lambda (v) (repeat v v)) #'1+)) '(2 3 3)))
  (5am:is (equal (to-list (select-many '() (lambda (v) (repeat v v)))) '())))

(5am:test select-many*
  (5am:is (equal (to-list (select-many* '(0 1 2 2) #'repeat)) '(1 2 2 2 2 2)))
  (5am:is (equal (to-list (select-many* '(0 1 2 2) #'repeat #'1+)) '(2 3 3 3 3 3)))
  (5am:is (equal (to-list (select-many* '() #'repeat)) '())))

(5am:test single
  (5am:is (equal (single '(0)) 0))
  (5am:is (equal (single '(0) :sentinel) 0))
  (5am:is (equal (single '()) nil))
  (5am:is (equal (single '() :sentinel) :sentinel))
  (5am:signals error
    (single '(0 1)))
  (5am:signals error
    (single '(0 1) :sentinel)))

(5am:test single*
  (5am:is (equal (single* '(0) #'evenp) 0))
  (5am:is (equal (single* '(0) #'evenp :sentinel) 0))
  (5am:is (equal (single* '(-5 -1 0 5 1) #'evenp) 0))
  (5am:is (equal (single* '(-5 -1 0 5 1) #'evenp :sentinel) 0))
  (5am:is (equal (single* '() #'evenp) nil))
  (5am:is (equal (single* '() #'evenp :sentinel) :sentinel))
  (5am:signals error
    (single* '(0 1 2) #'evenp))
  (5am:signals error
    (single* '(0 1 2) #'evenp :sentinel)))

(5am:test skip
  (5am:is (equal (to-list (skip '(0 1 2 3) 0)) '(0 1 2 3)))
  (5am:is (equal (to-list (skip '(0 1 2 3) 2)) '(2 3)))
  (5am:is (equal (to-list (skip '(0 1 2 3) 4)) '()))
  (5am:is (equal (to-list (skip '(0 1 2 3) 2121)) '())))

(5am:test skip-last
  (5am:is (equal (to-list (skip-last '(0 1 2 3) 0)) '(0 1 2 3)))
  (5am:is (equal (to-list (skip-last '(0 1 2 3) 2)) '(0 1)))
  (5am:is (equal (to-list (skip-last '(0 1 2 3) 4)) '()))
  (5am:is (equal (to-list (skip-last '(0 1 2 3) 2121)) '())))

(5am:test skip-until
  (5am:is (equal (to-list (skip-until '(0 1 2 3) #'evenp)) '(0 1 2 3)))
  (5am:is (equal (to-list (skip-until '(0 1 2 3) #'oddp)) '(1 2 3)))
  (5am:is (equal (to-list (skip-until '(0 2 4 8) #'oddp)) '())))

(5am:test skip-while
  (5am:is (equal (to-list (skip-while '(0 1 2 3) #'evenp)) '(1 2 3)))
  (5am:is (equal (to-list (skip-while '(0 1 2 3) #'oddp)) '(0 1 2 3)))
  (5am:is (equal (to-list (skip-while '(0 2 4 8) #'oddp)) '(0 2 4 8))))

(5am:test take
  (5am:is (equal (to-list (take '(0 1 2 3) 0)) '()))
  (5am:is (equal (to-list (take '(0 1 2 3) 2)) '(0 1)))
  (5am:is (equal (to-list (take '(0 1 2 3) 4)) '(0 1 2 3)))
  (5am:is (equal (to-list (take '(0 1 2 3) 41201)) '(0 1 2 3))))

(5am:test take-every
  (5am:signals error
    (to-list (take-every '(0 1 2 3) 0)))
  (5am:is (equal (to-list (take-every '(0 1 2 3) 1)) '(0 1 2 3)))
  (5am:is (equal (to-list (take-every '(0 1 2 3) 2)) '(0 2)))
  (5am:is (equal (to-list (take-every '(0 1 2 3) 3)) '(0 3)))
  (5am:is (equal (to-list (take-every '(0 1 2 3) 41201)) '(0))))

(5am:test take-last
  (5am:signals error
    (to-list (take-last '(0 1 2 3) -12)))
  (5am:is (equal (to-list (take-last '(0 1 2 3) 0)) '()))
  (5am:is (equal (to-list (take-last '(0 1 2 3) 2)) '(2 3)))
  (5am:is (equal (to-list (take-last '(0 1 2 3) 4)) '(0 1 2 3)))
  (5am:is (equal (to-list (take-last '(0 1 2 3) 1212)) '(0 1 2 3))))

(5am:test take-until
  (5am:is (equal (to-list (take-until '(0 1 2 3) #'evenp)) '()))
  (5am:is (equal (to-list (take-until '(0 1 2 3) #'oddp)) '(0)))
  (5am:is (equal (to-list (take-until '(0 2 4 8) #'oddp)) '(0 2 4 8))))

(5am:test take-while
  (5am:is (equal (to-list (take-while '(0 1 2 3) #'evenp)) '(0)))
  (5am:is (equal (to-list (take-while '(0 1 2 3) #'oddp)) '()))
  (5am:is (equal (to-list (take-while '(0 2 4 8) #'oddp)) '()))
  (5am:is (equal (to-list (take-while '(0 2 4 8) #'evenp)) '(0 2 4 8))))

(5am:test where
  (5am:is (equal (to-list (where '(0 1 2 3) #'evenp)) '(0 2)))
  (5am:is (equal (to-list (where '(0 1 2 3) #'oddp)) '(1 3)))
  (5am:is (equal (to-list (where '(0 2 4 8) #'oddp)) '()))
  (5am:is (equal (to-list (where '(0 2 4 8) #'evenp)) '(0 2 4 8))))

(5am:test to-list
  (5am:is (equal (to-list '(1 2 3)) '(1 2 3)))
  (5am:is (equal (to-list '()) '()))
  (5am:is (equal (to-list '(0)) '(0))))

(5am:test to-vector
  (5am:is (equalp (to-vector '(1 2 3)) #(1 2 3)))
  (5am:is (equalp (to-vector '()) #()))
  (5am:is (equalp (to-vector '(0)) #(0))))
