
# Dynotune - Automated parameter tuner for CL [![Build Status](https://travis-ci.org/guicho271828/dynotune.svg?branch=master)](https://travis-ci.org/guicho271828/dynotune)

This library provides an easy-to-use interface to black-box optimization algorithms.

+ Intended for tuning the meta-parameter of a particular function/solver/system.
+ Focuses on the usability and should "work out of the box".
+ It should also support many algorithms, but it should not force the users to understand them.
+ Although the tuner should be interruptible and multi-threaded, this depends on the implementation of each optimizer.

## Example

``` lisp
TEST> (declaim (ftype (function ((double-float -1d0 1d0)) (double-float -1d0 1d0)) ^2))
; -> (|^2|)
TEST> (defun ^2 (x) (* x x))
|^2|
TEST> (tune '^2 (random-search 100))
2.9022303484219113d-4      ; the value of (^2 x)
(0.017035933635765055d0)   ; the value of x
NIL
TEST> 
```

More examples are in `t/`.

## API

Primary API is through a function `tune`.

    (tune function &optional method parameters)

`method` is an instance of an optimizer, such as random-search, GA,
local-search, etc. (we describe it below)

`parameters` is a list of type specifiers, one for each parameter.
We parse and retrieve the
meta-information in these parameters
using [TYPE-R](https://github.com/guicho271828/type-r), so it supports a
significant subset of standard type specifiers in common lisp.

+ `short/double/single/long-float`, `integer`
+ Compound type specifiers like `(double-float -5.0 5.0)`, `(integer -1 10)`, `(mod 5)`, `(unsigned-byte 5)`

Additionally, we accept the following specifiers for discrete optimization:

+ `(categorical :a :b :c)` == `(member :a :b :c)` --- a finite set.
+ `(ordinary :first :second :third)` --- implies that the set has an ordering.
+ `(interval :mon :tue :wed :thu :fri :sat :sun)` --- in addition to being an
  ordinary variable, it implies that the distance between each element is identical.

We generate a generator instance for each parameter from the given type specifier.
Each optimizer supports a limited class of generator.
For example, gradient descent works only for floats.

`parameters` is optional when the compiler can deduce the argument types of the `function`.
`ftype` information is quite useful because on SBCL it can retrieve the type information.


## Available optimizers

Currently, the available optimizers focuses on discrete parameters.

`[COMMON-KEYS]` denotes keyword arguments `(predicate #'<) keep-results`.

+ `(random-search max-trials &key [COMMON-KEYS])`
  + Supports all generators.
+ `(grid-search &key [COMMON-KEYS])` 
  + Supports all finite generators (non-floats).
+ `(hill-climbing &key [COMMON-KEYS])`
  + Evaluate the neighbors of the current state and move to the first neighbor that improves the result.
    Also known as first-choice hill-climbing.
  + Supports all finite generators (non-floats).
+ `(hill-climbing2 &key [COMMON-KEYS])`
  + Evaluate ALL neighbors of the current state and move to the BEST neighbor.
  + Supports all finite generators (non-floats).
+ `(random-restart &key [COMMON-KEYS] (restart 10) (optimizer (hill-climbing)))`
  + Iterates over `optimizer` for `restart` times from the
    different random initial parameter. Also known as *Shotgun hill climbing*.
    It is a surprisingly effective algorithm in many cases.
+ `(gradient-descent &key [COMMON-KEYS] (dx 0.01) (lr 0.01) (epoch 100) &allow-other-keys)`
  + Supports all continuous generators (floats).
  + Performs gradient descent by x_(n+1) = x_n - γ∇f(x_n) . γ=lr (learning rate).
  + Since the function is black-box, gradient is computed numerically.
  + The default performance is likely not good.

Work in progress (should be implemented in a few days, send me a message if I forgot this --- 2017/10/11)

+ Differential Evolution
+ Simulated Annealing
+ GA and real-valued GA
+ PSO and ACO
+ ~~CMA-ES~~ --- is NOT appropriate for "work out of box" situation. It requires lots of hyperparameters by itself.

We do NOT cover the following algorithms because they are algorithms for combinatorial problems.
They assume some constraints between the variables.

+ tabu-search

## Dependencies
This library is at least tested on implementation listed below:

+ SBCL 1.4.0 on X86-64 Linux 4.10.0-35-generic (author's environment)

Also, it depends on the following libraries:

+ iterate by ** :
    Jonathan Amsterdam's iterator/gatherer/accumulator facility
+ alexandria by *Nikodemus Siivola <nikodemus@sb-studio.net>, and others.* :
    Alexandria is a collection of portable public domain utilities.
+ trivia :
    

## Installation

## Author

* Masataro Asai (guicho2.71828@gmail.com)

## Copyright

Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)

# License

Licensed under the LLGPL License.


