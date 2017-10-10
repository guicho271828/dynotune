
# Dynotune - Automated parameter tuner for CL

This library provides an easy-to-use interface to the optimization algorithms.
It is primarily intended for tuning the meta-parameter of a particular function, solver or system.

It focuses on the usability and should "work out of the box". It should also
support many algorithms, but it should not force the users to understand them.
It could even automatically select the algorithm to use.

Although the tuner should be interruptible and multi-threaded, this greatly
depends on the implementation of each optimizer.

## Usage

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

## Available optimizer

+ `(random-search max-trials &key (mode :minimize) keep-results)` --- supports all generators.


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


