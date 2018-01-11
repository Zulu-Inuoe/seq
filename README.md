# enumerable

Library for .NET style IEnumerables in CL.

Provides drivers for lists, sequences, hash tables, and packages.

## Usage

enumerables provides both a unified interface to enumerable types, as well as
various generator style algorithms on which they act.

``` common-lisp
;;Define an enumerable that represents all the natural numbers
(defenumerable natural-numbers ()
  (loop
    :for i :from 1 :by 1
    :do (yield i)))

;;Alternatively
(defun natural-numbers ()
  (with-enumerable
    (loop
      :for i :from 1 :by 1
      :do (yield i))))

;;Print the first 50 even numbers
(do-enumerable (n (take (where (natural-numbers) #'evenp) 50))
  (print n))

;;;CL sequence types, hash tables, and packages also enumerable

;;Lists
(do-enumerable (x (select '(1 2 3) #'1+))
  (print x)) ; prints 2 3 4

;;Vectors
(do-enumerable (x (skip #(1 2 3) 2))
  (print x)) ; prints 3

;;Strings
(do-enumerable (x (take "Hello, world!" 5))
  (print x)) ; prints #\H #\e #\l #\l #\o

;;Hash tables iterate over a cons who's car is the key, and cdr is the value
(do-enumerable (kvp (alexandria:alist-hash-table '((a . 0) (b . 1) (c . 2))))
  (print kvp)) ; prints (A . 0), (B . 1) ...

;;Packages iterate over the accessible symbols (eg do-symbols)
(do-enumerable (sym (find-package :cl))
  (print sym)) ; prints.. a lot
```

## Defining new enumerable types

Note that the CL type `sequence` already has a driver, so if your type
implements `sequence` by your implementation, it is already enumerable.

For other cases..

At minimum, an enumerable type defines

`get-enumerator`

from which an enumerator object is created.

This object must implement

`current` and `move-next`

The best simple example is the general [sequence enumerator](src/drivers/enumerable-sequence.lisp).

### Optimization

#### map-enumerable

The simplest way to optimize your type is to define a custom `map-enumerable` method.
This method is generally used by `do-enumerable`, and will therefore be used in many of
the built-in [implementations of expressions](src/drivers/enumerable-generic.lisp).

#### expressions

Each [expression](src/expressions.lisp) may be specialized on, when appropriate.

#### do-enumerable

Finally, using the `define-do-enumerable-expander` macro allows you to define an inline
loop expansion for your type. `do-enumerable` will use this expansion when it is able to identify
the type, and as part of an inline type dispatcher.

See the [builtin expanders](src/builtin-expanders.lisp) for examples.

With this system,

``` common-lisp
(defun print-squares (numbers)
  (declare (type list numbers))
  (do-enumerable (x numbers)
    (print (* x x))))
```

will expand into

``` common-lisp
(defun print-squares (numbers)
  (declare (type list numbers))
  (dolist (x numbers)
    (print (* x x))))
```

Meaning no overhead from using the more general `do-enumerable` for iteration.
