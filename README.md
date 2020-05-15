# seq

Library for Clojure-style seqs in CL.

Provides drivers for lists, sequences, hash tables, packages, streams, and optional support for:
* [cl-containers](https://common-lisp.net/project/cl-containers/)
* [fset](https://github.com/slburson/fset)
* [trivial-gray-streams](https://github.com/trivial-gray-streams/trivial-gray-streams)

Each available as an add-on system like `com.inuoe.seq.fset`.

There are additionally optional systems for [defining yield-style generators](src/seq.generators/seq.generators.lisp), a [rich C# Linq-like set of operations](src/seqio/seqio.lisp), and [an extensible iteration macro](src/doseq/doseq.lisp).

## Usage

Seqs provide a unified interface to a 'sequence' type, as well as the ability to trivially define generators via `lazy-seq`

``` common-lisp
(defpackage #:com.inuoe.seq-example1
  (:use #:cl)
  (:import-from
    #:com.inuoe.seq
    #:lazy-seq)
  (:import-from
    #:com.inuoe.doseq
    #:doseq)
  (:import-from
    #:com.inuoe.seqio
    #:select
    #:skip
    #:take
    #:where))

(in-package #:com.inuoe.seq-example1)

;;Define an enumerable that represents all the natural numbers
(defun natural-numbers ()
  (labels ((recurse (i)
           (lazy-seq
             (cons i (recurse (1+ i))))))
    (recurse 1)))

;;Print the first 50 even numbers
(doseq (n (take (where (natural-numbers) #'evenp) 50))
  (print n))

;;;CL sequence types, hash tables, and packages also enumerable

;;Lists
(doseq (x (select '(1 2 3) #'1+))
  (print x)) ; prints 2 3 4

;;Vectors
(doseq (x (skip #(1 2 3) 2))
  (print x)) ; prints 3

;;Strings
(doseq (x (take "Hello, world!" 5))
  (print x)) ; prints #\H #\e #\l #\l #\o

;;Hash tables iterate over a cons who's car is the key, and cdr is the value
(doseq (kvp (alexandria:alist-hash-table '((a . 0) (b . 1) (c . 2))))
  (print kvp)) ; prints (A . 0), (B . 1) ...

;;Packages iterate over the accessible symbols (eg do-symbols)
(doseq (sym (find-package :cl))
  (print sym)) ; prints.. a lot
```

## arrows

It might be worthwhile to also use one of the "arrow" packages with **seqio**
for better legibility. Check out [cl-arrows](https://github.com/nightfly19/cl-arrows) or
[arrow-macros](https://github.com/hipeta/arrow-macros).
In the [serapeum](https://github.com/TBRSS/serapeum), the symbol `~>` is used instead of `->`

Examples:

``` common-lisp
(defpackage #:com.inuoe.seq-example2
  (:use #:cl)
  (:import-from
    #:com.inuoe.seq.generators
    #:with-generator)
  (:import-from
    #:com.inuoe.doseq
    #:doseq)
  (:import-from
    #:com.inuoe.seqio
    #:take
    #:to-list
    #:where))

(in-package #:com.inuoe.seq-example2)

;; Get a list of 5 random numbers
(-> (with-generator (loop (yield (random 10000)))) ; random numbers
    (where #'evenp) ; that are even
    (take 5)  ; take 5 of them
    (to-list)) ; as a list
=>
(7702 8872 2114 8144 7488) ; for example
```

## Defining new seq types

Implementing a new seq type means providing seqable behaviour.
Specifically, at a minimum should define a `col-seq` implementation which returns a `seq`. A `seq` is an object with suitable `seq-first` and `seq-rest` specializations, or nil.

`seq-first` should return the first element of `seq`, and `seq-rest` should return a seqable object.

The simplest implementation will just leverage the existing `lazy-seq`.

### Optimization

#### mapcol

The simplest way to optimize your type is to define a custom `mapcol` method.
This method is generally used by `doseq` when no specialized expander exists.

#### seqio

Each [seqio operation](src/seqio/seqio.lisp) may be specialized on.

#### doseq

Finally, using the `define-doseq-expander` macro allows you to define an inline
loop expansion for your type. `doseq` will use this expansion when it is able to identify
the type, and as part of an inline type dispatcher.

See the [builtin expanders](src/doseq/doseq.lisp) for examples.
