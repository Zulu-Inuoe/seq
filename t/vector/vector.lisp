;;;enumerable-tests - tests for the enumerable library
;;;Written in 2018 by Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>
;;;
;;;To the extent possible under law, the author(s) have dedicated all copyright
;;;and related and neighboring rights to this software to the public domain
;;;worldwide. This software is distributed without any warranty.
;;;You should have received a copy of the CC0 Public Domain Dedication along
;;;with this software. If not, see
;;;<http://creativecommons.org/publicdomain/zero/1.0/>.

(in-package #:enumerable-tests.vector)

(5am:def-suite enumerable.vector
  :description "Tests for enumerable interface implementation for vectors."
  :in enumerable)

(5am:in-suite enumerable.vector)

(5am:test vector.get-enumerator
  (5am:finishes
    (let ((enumerator (get-enumerator #(0 1 2 3))))
      (5am:is (move-next enumerator))
      (5am:is (= 0 (current enumerator)))
      (5am:is (move-next enumerator))
      (5am:is (= 1 (current enumerator)))
      (5am:is (move-next enumerator))
      (5am:is (= 2 (current enumerator)))
      (5am:is (move-next enumerator))
      (5am:is (= 3 (current enumerator)))
      (5am:is-false (move-next enumerator)))
    (let ((enumerator (get-enumerator #())))
      (5am:is-false (move-next enumerator))
      (5am:is-false (move-next enumerator)))))

(5am:def-suite expressions.vector
  :description "Tests the enumerable expressions with vectors."
  :in expressions)

(5am:in-suite expressions.vector)

(5am:test vector.aggregate
  (5am:is (equal 6 (aggregate #(0 2 4) #'+)))
  (5am:is (equal 0
                 (aggregate #(0) (lambda (x y)
                                   (declare (ignore x y))
                                   (error "fail")))))
  (5am:signals error
    (aggregate #() #'+)))

(5am:test vector.aggregate*
  (5am:is (equal 6 (aggregate* #(0 2 4) #'+ 0)))
  (5am:is (equal 15 (aggregate* #(10) #'+ 5)))
  (5am:is (equal 5 (aggregate* #() #'+ 5))))

(5am:test vector.all
  (5am:is-true (all #(0 2 4) #'evenp))
  (5am:is-false (all #(1 2 4) #'evenp))
  (5am:is-false (all #(0 1 4) #'evenp))
  (5am:is-false (all #(0 2 3) #'evenp))
  (5am:is-true (all #() #'evenp))
  (5am:is-true (all #(0) #'evenp)))

(5am:test vector.any
  (5am:is-true (any #(0)))
  (5am:is-false (any #())))

(5am:test vector.any*
  (5am:is-true (any* #(0 2 4) #'evenp))
  (5am:is-true (any* #(1 2 4) #'evenp))
  (5am:is-true (any* #(0 3) #'evenp))
  (5am:is-false (any* #() #'evenp))
  (5am:is-true (any* #(0 1 nil 2) #'null)))

(5am:test vector.eappend
  (5am:is (equal '(0 1 2) (to-list (eappend #(0 1) 2))))
  (5am:is (equal '(0) (to-list (eappend #() 0)))))

(5am:test vector.batch
  (5am:is (equalp '(#(0) #(1) #(2) #(3)) (to-list (batch #(0 1 2 3) 1))))
  (5am:is (equalp '(#(0 1) #(2 3)) (to-list (batch #(0 1 2 3) 2))))
  (5am:is (equalp '(#(0 1 2) #(3)) (to-list (batch #(0 1 2 3) 3))))
  (5am:is (equalp '(#(0 1 2 3)) (to-list (batch #(0 1 2 3) 4)))))

(5am:test vector.concat
  (5am:is (equal '(0 1 2 3) (to-list (concat #(0 1) #(2 3)))))
  (5am:is (equal '(0 1) (to-list (concat #(0 1) #()))))
  (5am:is (equal '(2 3) (to-list (concat #() #(2 3)))))
  (5am:is (equal '() (to-list (concat #() #())))))

(5am:test vector.consume
  ;;No observable behavior
  )

(5am:test vector.contains
  (5am:is-true (contains #(0 1 2) 0))
  (5am:is-true (contains #(0 1 2) 1))
  (5am:is-true (contains #(0 1 2) 2))
  (5am:is-true (contains #("Hello") "Hello" #'string=))
  (5am:signals type-error
    (contains #(0 1 2) 0 #'string=))
  (5am:is-false (contains #(0 1 2) 5))
  (5am:is-false (contains #("Hello") 0))
  (5am:is-true (contains #(5 1 nil 2) nil)))

(5am:test vector.ecount
  (5am:is (= 0 (ecount #())))
  (5am:is (= 1 (ecount #(0))))
  (5am:is (= 2 (ecount #(0 0))))
  (5am:is (= 2 (ecount #(0 1)))))

(5am:test vector.ecount*
  (5am:is (= 0 (ecount* #() #'evenp)))
  (5am:is (= 1 (ecount* #(0) #'evenp)))
  (5am:is (= 2 (ecount* #(0 0) #'evenp)))
  (5am:is (= 1 (ecount* #(0 1) #'evenp))))

(5am:test vector.default-if-empty
  (5am:is (equal '(0) (to-list (default-if-empty #() 0))))
  (5am:is (equal '(1) (to-list (default-if-empty #(1) 0)))))

(5am:test vector.distinct
  (5am:is (set-equal '(0 1 2 3)
                     (to-list (distinct #(0 1 0 1 3 2)))))
  (5am:is (= 4
             (length
              (to-list (distinct #(0 1 0 1 3 2)))))))

(5am:test vector.element-at
  (5am:is (= 0 (element-at #(0 5 7) 0)))
  (5am:is (= 5 (element-at #(0 5 7) 1)))
  (5am:is (= 7 (element-at #(0 5 7) 2)))
  (5am:is (eq :sentinel (element-at #(0 5 7) 100 :sentinel)))
  (5am:is (eq :sentinel (element-at #() 0 :sentinel))))

(5am:test vector.evaluate
  (5am:is (equal '(0 1 2)
                 (to-list (evaluate (vector (lambda () 0) (lambda () 1) (lambda () 2))))))
  (5am:is (equal '(3 5 8)
                 (to-list (evaluate (vector (lambda () 3) (lambda () 5) (lambda () 8))))))
  (5am:is (equal '()
                 (to-list (evaluate '())))))

(5am:test vector.except
  (5am:is (equal '(1) (to-list (except #(1 2) #(2)))))
  (5am:is (equal '() (to-list (except #(1 2) #(1 2)))))
  (5am:is (equal '(1 2) (to-list (except #(1 2) #()))))
  (5am:is (equal '() (to-list (except #() #()))))
  (5am:is (equal '() (to-list (except #() #(1 2))))))

(5am:test vector.efirst
  (5am:is (= 1 (efirst #(1 2))))
  (5am:is (= 5 (efirst #() 5)))
  (5am:is (= 0 (efirst #(0) 5))))

(5am:test vector.efirst*
  (5am:is (= 5 (efirst* #(0 5 3 7) #'oddp)))
  (5am:is (= 0 (efirst* #(0 5 3 7) #'evenp)))
  (5am:is (eq nil (efirst* #(0 2 10 6) #'oddp)))
  (5am:is (eq :sentinel (efirst* #(0 2 10 6) #'oddp :sentinel))))

(5am:test vector.elast
  (5am:is (= 3 (elast #(1 2 3))))
  (5am:is (eq nil (elast #())))
  (5am:is (eq :sentinel (elast #() :sentinel)))
  (5am:is (= 0 (elast #(0) :sentinel))))

(5am:test vector.elast*
  (5am:is (= 3 (elast* #(1 2 3) #'identity)))
  (5am:is (= 2 (elast* #(1 2 3) #'evenp)))
  (5am:is (eq nil (elast* #() #'evenp)))
  (5am:is (eq :sentinel (elast* #(1) #'evenp :sentinel)))
  (5am:is (eq :sentinel (elast* #() #'evenp :sentinel))))

(5am:test vector.prepend
  (5am:is (equal '(0 1 2) (to-list (prepend #(1 2) 0))))
  (5am:is (equal '(0) (to-list (prepend #() 0)))))

(5am:test vector.reverse
  (5am:is (equal '(4 3 2 1) (to-list (reverse #(1 2 3 4)))))
  (5am:is (equal '(1) (to-list (reverse #(1))))))

(5am:test vector.select
  (5am:is (equal '(1 2 3) (to-list (select #(0 1 2) #'1+))))
  (5am:is (equal '(1) (to-list (select #(0) #'1+))))
  (5am:is (equal '() (to-list (select #() #'1+)))))

(5am:test vector.select*
  (5am:is (equal '((0 . 0) (1 . 1) (2 . 2)) (to-list (select* #(0 1 2) #'cons))))
  (5am:is (equal '((a . 0) (b . 1) (c . 2)) (to-list (select* #(a b c) #'cons))))
  (5am:is (equal '() (to-list (select* #() #'cons)))))

(5am:test vector.select-many
  (5am:is (equal '(1 2 2) (to-list (select-many #(0 1 2) (lambda (v) (repeat v v))))))
  (5am:is (equal '(2 3 3) (to-list (select-many #(0 1 2) (lambda (v) (repeat v v)) #'1+))))
  (5am:is (equal '() (to-list (select-many #() (lambda (v) (repeat v v)))))))

(5am:test vector.select-many*
  (5am:is (equal '(1 2 2 2 2 2) (to-list (select-many* #(0 1 2 2) #'repeat))))
  (5am:is (equal '(2 3 3 3 3 3) (to-list (select-many* #(0 1 2 2) #'repeat #'1+))))
  (5am:is (equal '() (to-list (select-many* #() #'repeat)))))

(5am:test vector.single
  (5am:is (equal 0 (single #(0))))
  (5am:is (equal 0 (single #(0) :sentinel)))
  (5am:is (equal nil (single #())))
  (5am:is (equal :sentinel (single #() :sentinel)))
  (5am:signals error
    (single #(0 1)))
  (5am:signals error
    (single #(0 1) :sentinel)))

(5am:test vector.single*
  (5am:is (equal 0 (single* #(0) #'evenp)))
  (5am:is (equal 0 (single* #(0) #'evenp :sentinel)))
  (5am:is (equal 0 (single* #(-5 -1 0 5 1) #'evenp)))
  (5am:is (equal 0 (single* #(-5 -1 0 5 1) #'evenp :sentinel)))
  (5am:is (equal nil (single* #() #'evenp)))
  (5am:is (equal :sentinel (single* #() #'evenp :sentinel)))
  (5am:signals error
    (single* #(0 1 2) #'evenp))
  (5am:signals error
    (single* #(0 1 2) #'evenp :sentinel)))

(5am:test vector.skip
  (5am:is (equal '(0 1 2 3) (to-list (skip #(0 1 2 3) 0))))
  (5am:is (equal '(2 3) (to-list (skip #(0 1 2 3) 2))))
  (5am:is (equal '() (to-list (skip #(0 1 2 3) 4))))
  (5am:is (equal '() (to-list (skip #(0 1 2 3) 2121))))
  (5am:is (equal '(0 1 2 3) (to-list (skip #(0 1 2 3) -1)))))

(5am:test vector.skip-last
  (5am:is (equal '(0 1 2 3) (to-list (skip-last #(0 1 2 3) 0))))
  (5am:is (equal '(0 1) (to-list (skip-last #(0 1 2 3) 2))))
  (5am:is (equal '() (to-list (skip-last #(0 1 2 3) 4))))
  (5am:is (equal '() (to-list (skip-last #(0 1 2 3) 2121)))))

(5am:test vector.skip-until
  (5am:is (equal '(0 1 2 3) (to-list (skip-until #(0 1 2 3) #'evenp))))
  (5am:is (equal '(1 2 3) (to-list (skip-until #(0 1 2 3) #'oddp))))
  (5am:is (equal '() (to-list (skip-until #(0 2 4 8) #'oddp)))))

(5am:test vector.skip-while
  (5am:is (equal '(1 2 3) (to-list (skip-while #(0 1 2 3) #'evenp))))
  (5am:is (equal '(0 1 2 3) (to-list (skip-while #(0 1 2 3) #'oddp))))
  (5am:is (equal '(0 2 4 8) (to-list (skip-while #(0 2 4 8) #'oddp)))))

(5am:test vector.take
  (5am:is (equal '() (to-list (take #(0 1 2 3) 0))))
  (5am:is (equal '(0 1) (to-list (take #(0 1 2 3) 2))))
  (5am:is (equal '(0 1 2 3) (to-list (take #(0 1 2 3) 4))))
  (5am:is (equal '(0 1 2 3) (to-list (take #(0 1 2 3) 41201)))))

(5am:test vector.take-every
  (5am:signals error
    (to-list (take-every #(0 1 2 3) 0)))
  (5am:is (equal '(0 1 2 3) (to-list (take-every #(0 1 2 3) 1))))
  (5am:is (equal '(0 2) (to-list (take-every #(0 1 2 3) 2))))
  (5am:is (equal '(0 3) (to-list (take-every #(0 1 2 3) 3))))
  (5am:is (equal '(0) (to-list (take-every #(0 1 2 3) 41201)))))

(5am:test vector.take-last
  (5am:signals error
    (to-list (take-last #(0 1 2 3) -12)))
  (5am:is (equal '() (to-list (take-last #(0 1 2 3) 0))))
  (5am:is (equal '(2 3) (to-list (take-last #(0 1 2 3) 2))))
  (5am:is (equal '(0 1 2 3) (to-list (take-last #(0 1 2 3) 4))))
  (5am:is (equal '(0 1 2 3) (to-list (take-last #(0 1 2 3) 1212)))))

(5am:test vector.take-until
  (5am:is (equal '() (to-list (take-until #(0 1 2 3) #'evenp))))
  (5am:is (equal '(0) (to-list (take-until #(0 1 2 3) #'oddp))))
  (5am:is (equal '(0 2 4 8) (to-list (take-until #(0 2 4 8) #'oddp)))))

(5am:test vector.take-while
  (5am:is (equal '(0) (to-list (take-while #(0 1 2 3) #'evenp))))
  (5am:is (equal '() (to-list (take-while #(0 1 2 3) #'oddp))))
  (5am:is (equal '() (to-list (take-while #(0 2 4 8) #'oddp))))
  (5am:is (equal '(0 2 4 8) (to-list (take-while #(0 2 4 8) #'evenp)))))

(5am:test vector.where
  (5am:is (equal '(0 2) (to-list (where #(0 1 2 3) #'evenp))))
  (5am:is (equal '(1 3) (to-list (where #(0 1 2 3) #'oddp))))
  (5am:is (equal '() (to-list (where #(0 2 4 8) #'oddp))))
  (5am:is (equal '(0 2 4 8) (to-list (where #(0 2 4 8) #'evenp)))))

(5am:test vector.window
  (5am:is (equalp '(#(0) #(1) #(2) #(3)) (to-list (window #(0 1 2 3) 1))))
  (5am:is (equalp '(#(0 1) #(1 2) #(2 3)) (to-list (window #(0 1 2 3) 2))))
  (5am:is (equalp '(#(0 1 2) #(1 2 3)) (to-list (window #(0 1 2 3) 3))))
  (5am:is (equalp '(#(0 1 2 3)) (to-list (window #(0 1 2 3) 4))))
  (5am:is (equalp '() (to-list (window #(0 1 2 3) 5)))))

(5am:test vector.to-list
  (5am:is (equal '(1 2 3) (to-list #(1 2 3))))
  (5am:is (equal '() (to-list #())))
  (5am:is (equal '(0) (to-list #(0)))))

(5am:test vector.to-vector
  (5am:is (equalp #(1 2 3) (to-vector #(1 2 3))))
  (5am:is (equalp #() (to-vector #())))
  (5am:is (equalp #(0) (to-vector #(0)))))
