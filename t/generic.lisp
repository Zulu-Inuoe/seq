;;;enumerable-tests - tests for the enumerable library
;;;Written in 2018 by Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>
;;;
;;;To the extent possible under law, the author(s) have dedicated all copyright
;;;and related and neighboring rights to this software to the public domain
;;;worldwide. This software is distributed without any warranty.
;;;You should have received a copy of the CC0 Public Domain Dedication along
;;;with this software. If not, see
;;;<http://creativecommons.org/publicdomain/zero/1.0/>.

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
  (5am:is (equal (aggregate (%make-e '(0 2 4)) #'+) 6))
  (5am:is (equal (aggregate (%make-e '(0)) (lambda (x y)
                                   (declare (ignore x y))
                                   (error "fail")))
                 0))
  (5am:signals error
    (aggregate (%make-e '()) #'+)))

(5am:test generic.aggregate*
  (5am:is (equal (aggregate* (%make-e '(0 2 4)) #'+ 0) 6))
  (5am:is (equal (aggregate* (%make-e '(10)) #'+ 5) 15))
  (5am:is (equal (aggregate* (%make-e '()) #'+ 5) 5)))

(5am:test generic.all
  (5am:is (all (%make-e '(0 2 4)) #'evenp))
  (5am:is (not (all (%make-e '(1 2 4)) #'evenp)))
  (5am:is (not (all (%make-e '(0 1 4)) #'evenp)))
  (5am:is (not (all (%make-e '(0 2 3)) #'evenp)))
  (5am:is (all (%make-e '()) #'evenp))
  (5am:is (all (%make-e '(0)) #'evenp)))

(5am:test generic.any
  (5am:is (any (%make-e '(0))))
  (5am:is (not (any (%make-e '())))))

(5am:test generic.any*
  (5am:is (any* (%make-e '(0 2 4)) #'evenp))
  (5am:is (any* (%make-e '(1 2 4)) #'evenp))
  (5am:is (any* (%make-e '(0 3)) #'evenp))
  (5am:is (not (any* (%make-e '()) #'evenp)))
  (5am:is (any* (%make-e '(0 1 nil 2)) #'null)))

(5am:test generic.eappend
  (5am:is (equal (to-list (eappend (%make-e '(0 1)) 2)) '(0 1 2)))
  (5am:is (equal (to-list (eappend (%make-e '()) 0)) '(0))))

(5am:test generic.batch
  (5am:is (equalp (to-list (batch (%make-e '(0 1 2 3)) 1)) '(#(0) #(1) #(2) #(3))))
  (5am:is (equalp (to-list (batch (%make-e '(0 1 2 3)) 2)) '(#(0 1) #(2 3))))
  (5am:is (equalp (to-list (batch (%make-e '(0 1 2 3)) 3)) '(#(0 1 2) #(3))))
  (5am:is (equalp (to-list (batch (%make-e '(0 1 2 3)) 4)) '(#(0 1 2 3)))))

(5am:test generic.concat
  (5am:is (equal (to-list (concat (%make-e '(0 1)) (%make-e '(2 3)))) '(0 1 2 3)))
  (5am:is (equal (to-list (concat (%make-e '(0 1)) (%make-e '()))) '(0 1)))
  (5am:is (equal (to-list (concat (%make-e '()) (%make-e '(2 3)))) '(2 3)))
  (5am:is (equal (to-list (concat (%make-e '()) (%make-e '()))) '())))

(5am:test generic.consume
  ;; Need proper test for side-effects
  )

(5am:test generic.contains
  (5am:is (contains (%make-e '(0 1 2)) 0))
  (5am:is (contains (%make-e '(0 1 2)) 1))
  (5am:is (contains (%make-e '(0 1 2)) 2))
  (5am:is (contains (%make-e '("Hello")) "Hello" #'string=))
  (5am:signals type-error
    (contains (%make-e '(0 1 2)) 0 #'string=))
  (5am:is (not (contains (%make-e '(0 1 2)) 5)))
  (5am:is (not (contains (%make-e '("Hello")) 0)))
  (5am:is (contains (%make-e '(5 1 nil 2)) nil)))

(5am:test generic.ecount
  (5am:is (= (ecount (%make-e '())) 0))
  (5am:is (= (ecount (%make-e '(0))) 1))
  (5am:is (= (ecount (%make-e '(0 0))) 2))
  (5am:is (= (ecount (%make-e '(0 1))) 2)))

(5am:test generic.ecount*
  (5am:is (= (ecount* (%make-e '()) #'evenp) 0))
  (5am:is (= (ecount* (%make-e '(0)) #'evenp) 1))
  (5am:is (= (ecount* (%make-e '(0 0)) #'evenp) 2))
  (5am:is (= (ecount* (%make-e '(0 1)) #'evenp) 1)))

(5am:test generic.default-if-empty
  (5am:is (equal (to-list (default-if-empty (%make-e '()) 0)) '(0)))
  (5am:is (equal (to-list (default-if-empty (%make-e '(1)) 0)) '(1))))

(5am:test generic.distinct
  (5am:is (set-equal (to-list (distinct (%make-e '(0 1 0 1 3 2))))
                     '(0 1 2 3)))
  (5am:is (= (length
              (to-list (distinct (%make-e '(0 1 0 1 3 2)))))
             4)))

(5am:test generic.element-at
  (5am:is (= (element-at (%make-e '(0 5 7)) 0) 0))
  (5am:is (= (element-at (%make-e '(0 5 7)) 1) 5))
  (5am:is (= (element-at (%make-e '(0 5 7)) 2) 7))
  (5am:is (eq (element-at (%make-e '(0 5 7)) 100 :sentinel) :sentinel))
  (5am:is (eq (element-at (%make-e '()) 0 :sentinel) :sentinel)))

(5am:test generic.evaluate
  (5am:is (equal (to-list (evaluate (%make-e (list (lambda () 0) (lambda () 1) (lambda () 2)))))
                 '(0 1 2)))
  (5am:is (equal (to-list (evaluate (%make-e (list (lambda () 3) (lambda () 5) (lambda () 8)))))
                 '(3 5 8)))
  (5am:is (equal (to-list (evaluate (%make-e '())))
                 '())))

(5am:test generic.except
  (5am:is (equal (to-list (except (%make-e '(1 2)) (%make-e '(2)))) '(1)))
  (5am:is (equal (to-list (except (%make-e '(1 2)) (%make-e '(1 2)))) '()))
  (5am:is (equal (to-list (except (%make-e '(1 2)) (%make-e '()))) '(1 2)))
  (5am:is (equal (to-list (except (%make-e '()) (%make-e '()))) '()))
  (5am:is (equal (to-list (except (%make-e '()) (%make-e '(1 2)))) '())))

(5am:test generic.efirst
  (5am:is (= (efirst (%make-e '(1 2))) 1))
  (5am:is (= (efirst (%make-e '()) 5) 5))
  (5am:is (= (efirst (%make-e '(0)) 5) 0)))

(5am:test generic.efirst*
  (5am:is (= (efirst* (%make-e '(0 5 3 7)) #'oddp) 5))
  (5am:is (= (efirst* (%make-e '(0 5 3 7)) #'evenp) 0))
  (5am:is (eq (efirst* (%make-e '(0 2 10 6)) #'oddp) nil))
  (5am:is (eq (efirst* (%make-e '(0 2 10 6)) #'oddp :sentinel) :sentinel)))

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
  (5am:is (set-equal (to-list (intersect (%make-e '(1 2 3)) (%make-e '(1)))) '(1)))
  (5am:is (set-equal (to-list (intersect (%make-e '(1 2 3)) (%make-e '(1 2 3)))) '(1 2 3)))
  (5am:is (set-equal (to-list (intersect (%make-e '(1 2 3)) (%make-e '(3)))) '(3)))
  (5am:is (set-equal (to-list (intersect (%make-e '(1 2 3)) (%make-e '()))) '()))
  (5am:is (set-equal (to-list (intersect (%make-e '(1 2 3)) (%make-e '(3 2 1)))) '(1 2 3)))
  (5am:is (set-equal (to-list (intersect (%make-e '()) (%make-e '(1 2 3)))) '())))

(5am:test generic.elast
  (5am:is (= (elast (%make-e '(1 2 3))) 3))
  (5am:is (eq (elast (%make-e '())) nil))
  (5am:is (eq (elast (%make-e '()) :sentinel) :sentinel))
  (5am:is (= (elast (%make-e '(0)) :sentinel) 0)))

(5am:test generic.elast*
  (5am:is (= (elast* (%make-e '(1 2 3)) #'identity) 3))
  (5am:is (= (elast* (%make-e '(1 2 3)) #'evenp) 2))
  (5am:is (eq (elast* (%make-e '()) #'evenp) nil))
  (5am:is (eq (elast* (%make-e '(1)) #'evenp :sentinel) :sentinel))
  (5am:is (eq (elast* (%make-e '()) #'evenp :sentinel) :sentinel)))

(5am:test generic.prepend
  (5am:is (equal (to-list (prepend (%make-e '(1 2)) 0)) '(0 1 2)))
  (5am:is (equal (to-list (prepend (%make-e '()) 0)) '(0))))

(5am:test generic.select
  (5am:is (equal (to-list (select (%make-e '(0 1 2)) #'1+)) '(1 2 3)))
  (5am:is (equal (to-list (select (%make-e '(0)) #'1+)) '(1)))
  (5am:is (equal (to-list (select (%make-e '()) #'1+)) '())))

(5am:test generic.select*
  (5am:is (equal (to-list (select* (%make-e '(0 1 2)) #'cons)) '((0 . 0) (1 . 1) (2 . 2))))
  (5am:is (equal (to-list (select* (%make-e '(a b c)) #'cons)) '((a . 0) (b . 1) (c . 2))))
  (5am:is (equal (to-list (select* (%make-e '()) #'cons)) '())))

(5am:test generic.select-many
  (5am:is (equal (to-list (select-many (%make-e '(0 1 2)) (lambda (v) (repeat v v)))) '(1 2 2)))
  (5am:is (equal (to-list (select-many (%make-e '(0 1 2)) (lambda (v) (repeat v v)) #'1+)) '(2 3 3)))
  (5am:is (equal (to-list (select-many (%make-e '()) (lambda (v) (repeat v v)))) '())))

(5am:test generic.select-many*
  (5am:is (equal (to-list (select-many* (%make-e '(0 1 2 2)) #'repeat)) '(1 2 2 2 2 2)))
  (5am:is (equal (to-list (select-many* (%make-e '(0 1 2 2)) #'repeat #'1+)) '(2 3 3 3 3 3)))
  (5am:is (equal (to-list (select-many* (%make-e '()) #'repeat)) '())))

(5am:test generic.single
  (5am:is (equal (single (%make-e '(0))) 0))
  (5am:is (equal (single (%make-e '(0)) :sentinel) 0))
  (5am:is (equal (single (%make-e '())) nil))
  (5am:is (equal (single (%make-e '()) :sentinel) :sentinel))
  (5am:signals error
    (single (%make-e '(0 1))))
  (5am:signals error
    (single (%make-e '(0 1)) :sentinel)))

(5am:test generic.single*
  (5am:is (equal (single* (%make-e '(0)) #'evenp) 0))
  (5am:is (equal (single* (%make-e '(0)) #'evenp :sentinel) 0))
  (5am:is (equal (single* (%make-e '(-5 -1 0 5 1)) #'evenp) 0))
  (5am:is (equal (single* (%make-e '(-5 -1 0 5 1)) #'evenp :sentinel) 0))
  (5am:is (equal (single* (%make-e '()) #'evenp) nil))
  (5am:is (equal (single* (%make-e '()) #'evenp :sentinel) :sentinel))
  (5am:signals error
    (single* (%make-e '(0 1 2)) #'evenp))
  (5am:signals error
    (single* (%make-e '(0 1 2)) #'evenp :sentinel)))

(5am:test generic.skip
  (5am:is (equal (to-list (skip (%make-e '(0 1 2 3)) 0)) '(0 1 2 3)))
  (5am:is (equal (to-list (skip (%make-e '(0 1 2 3)) 2)) '(2 3)))
  (5am:is (equal (to-list (skip (%make-e '(0 1 2 3)) 4)) '()))
  (5am:is (equal (to-list (skip (%make-e '(0 1 2 3)) 2121)) '()))
  (5am:is (equal (to-list (skip (%make-e '(0 1 2 3)) -1)) '(0 1 2 3))))

(5am:test generic.skip-last
  (5am:is (equal (to-list (skip-last (%make-e '(0 1 2 3)) 0)) '(0 1 2 3)))
  (5am:is (equal (to-list (skip-last (%make-e '(0 1 2 3)) 1)) '(0 1 2)))
  (5am:is (equal (to-list (skip-last (%make-e '(0 1 2 3)) 2)) '(0 1)))
  (5am:is (equal (to-list (skip-last (%make-e '(0 1 2 3)) 4)) '()))
  (5am:is (equal (to-list (skip-last (%make-e '(0 1 2 3)) 2121)) '())))

(5am:test generic.skip-until
  (5am:is (equal (to-list (skip-until (%make-e '(0 1 2 3)) #'evenp)) '(0 1 2 3)))
  (5am:is (equal (to-list (skip-until (%make-e '(0 1 2 3)) #'oddp)) '(1 2 3)))
  (5am:is (equal (to-list (skip-until (%make-e '(0 2 4 8)) #'oddp)) '())))

(5am:test generic.skip-while
  (5am:is (equal (to-list (skip-while (%make-e '(0 1 2 3)) #'evenp)) '(1 2 3)))
  (5am:is (equal (to-list (skip-while (%make-e '(0 1 2 3)) #'oddp)) '(0 1 2 3)))
  (5am:is (equal (to-list (skip-while (%make-e '(0 2 4 8)) #'oddp)) '(0 2 4 8))))

(5am:test generic.take
  (5am:is (equal (to-list (take (%make-e '(0 1 2 3)) 0)) '()))
  (5am:is (equal (to-list (take (%make-e '(0 1 2 3)) 2)) '(0 1)))
  (5am:is (equal (to-list (take (%make-e '(0 1 2 3)) 4)) '(0 1 2 3)))
  (5am:is (equal (to-list (take (%make-e '(0 1 2 3)) 41201)) '(0 1 2 3))))

(5am:test generic.take-every
  (5am:signals error
    (to-list (take-every (%make-e '(0 1 2 3)) 0)))
  (5am:is (equal (to-list (take-every (%make-e '(0 1 2 3)) 1)) '(0 1 2 3)))
  (5am:is (equal (to-list (take-every (%make-e '(0 1 2 3)) 2)) '(0 2)))
  (5am:is (equal (to-list (take-every (%make-e '(0 1 2 3)) 3)) '(0 3)))
  (5am:is (equal (to-list (take-every (%make-e '(0 1 2 3)) 41201)) '(0))))

(5am:test generic.take-last
  (5am:signals error
    (to-list (take-last (%make-e '(0 1 2 3)) -12)))
  (5am:is (equal (to-list (take-last (%make-e '(0 1 2 3)) 0)) '()))
  (5am:is (equal (to-list (take-last (%make-e '(0 1 2 3)) 2)) '(2 3)))
  (5am:is (equal (to-list (take-last (%make-e '(0 1 2 3)) 4)) '(0 1 2 3)))
  (5am:is (equal (to-list (take-last (%make-e '(0 1 2 3)) 1212)) '(0 1 2 3))))

(5am:test generic.take-until
  (5am:is (equal (to-list (take-until (%make-e '(0 1 2 3)) #'evenp)) '()))
  (5am:is (equal (to-list (take-until (%make-e '(0 1 2 3)) #'oddp)) '(0)))
  (5am:is (equal (to-list (take-until (%make-e '(0 2 4 8)) #'oddp)) '(0 2 4 8))))

(5am:test generic.take-while
  (5am:is (equal (to-list (take-while (%make-e '(0 1 2 3)) #'evenp)) '(0)))
  (5am:is (equal (to-list (take-while (%make-e '(0 1 2 3)) #'oddp)) '()))
  (5am:is (equal (to-list (take-while (%make-e '(0 2 4 8)) #'oddp)) '()))
  (5am:is (equal (to-list (take-while (%make-e '(0 2 4 8)) #'evenp)) '(0 2 4 8))))

(5am:test generic.eunion
  (5am:is (set-equal (to-list (eunion (%make-e '(0 1 2 3)) (%make-e '(0 1 2 3))))
                     '(0 1 2 3)))
  (5am:is (set-equal (to-list (eunion (%make-e '()) (%make-e '(0 1 2 2 3))))
                     '(0 1 2 3)))
  (5am:is (set-equal (to-list (eunion (%make-e '(0 1 1 2 3)) (%make-e '())))
                     '(0 1 2 3)))
  (5am:is (set-equal (to-list (eunion (%make-e '(0 1 1)) (%make-e '(3 2 3))))
                     '(0 1 2 3)))
  (5am:is (= (length (to-list (eunion (%make-e '("foo" "FOO")) (%make-e '("foo" "FOO")) #'string-equal)))
             1)))

(5am:test generic.where
  (5am:is (equal (to-list (where (%make-e '(0 1 2 3)) #'evenp)) '(0 2)))
  (5am:is (equal (to-list (where (%make-e '(0 1 2 3)) #'oddp)) '(1 3)))
  (5am:is (equal (to-list (where (%make-e '(0 2 4 8)) #'oddp)) '()))
  (5am:is (equal (to-list (where (%make-e '(0 2 4 8)) #'evenp)) '(0 2 4 8))))

(5am:test generic.window
  (5am:is (equalp (to-list (window (%make-e '(0 1 2 3)) 1)) '(#(0) #(1) #(2) #(3))))
  (5am:is (equalp (to-list (window (%make-e '(0 1 2 3)) 2)) '(#(0 1) #(1 2) #(2 3))))
  (5am:is (equalp (to-list (window (%make-e '(0 1 2 3)) 3)) '(#(0 1 2) #(1 2 3))))
  (5am:is (equalp (to-list (window (%make-e '(0 1 2 3)) 4)) '(#(0 1 2 3))))
  (5am:is (equalp (to-list (window (%make-e '(0 1 2 3)) 5)) '())))

(5am:test generic.to-hash-table
  (5am:is (set-equal
           (to-list (to-hash-table '(1 2 3) #'identity))
           '((1 . 1) (2 . 2) (3 . 3))
           :test #'equal))
  (5am:is (set-equal
           (to-list (to-hash-table '() #'identity))
           '()
           :test #'equal))
  (5am:is (set-equal
           (to-list (to-hash-table '(1 2) #'1+))
           '((2 . 1) (3 . 2))
           :test #'equal))
  (5am:is (set-equal
           (to-list (to-hash-table '(1 2) #'identity :selector #'1+))
           '((1 . 2) (2 . 3))
           :test #'equal)))

(5am:test generic.to-list
  (5am:is (equal (to-list (%make-e '(1 2 3))) '(1 2 3)))
  (5am:is (equal (to-list (%make-e '())) '()))
  (5am:is (equal (to-list (%make-e '(0))) '(0))))

(5am:test generic.to-vector
  (5am:is (equalp (to-vector (%make-e '(1 2 3))) #(1 2 3)))
  (5am:is (equalp (to-vector (%make-e '())) #()))
  (5am:is (equalp (to-vector (%make-e '(0))) #(0))))
