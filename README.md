
lazyTyper
=========

[![Build Status](https://travis-ci.org/akersting/lazyTyper.svg?branch=develop)](https://travis-ci.org/akersting/lazyTyper) [![Coverage Status](https://codecov.io/github/akersting/lazyTyper/coverage.svg?branch=develop)](https://codecov.io/github/akersting/lazyTyper?branch=develop)

lazyTyper adds the concept of strong typing to R. Before its first use, a variable can be declared to be of a specific type. Existing variables can be casted to a specific type. Assigning values of the wrong type to such typed variables will throw an error.

In addition to pure type information, it is also possible to associate additional properties with a variable, e.g. the length of a vector. Such properties are also validated when the variable is modified.

Installation
------------

There is no stable release of lazyTyper yet. To install the latest development version run:

``` r
devtools::install_github("akersting/lazyTyper@develop")
```

Introductory Examples
---------------------

### Declare and Cast

Use `declare` to type a non-existing variable and `cast` to type an existing one.

``` r
library(lazyTyper)
#> 
#> Attaching package: 'lazyTyper'
#> The following objects are masked from 'package:base':
#> 
#>     remove, rm

a <- 1:3
cast(a, "numeric", length = 2)
#> Error in cast(a, "numeric", length = 2): Variable a is not of type 'numeric' or it does not have the desired properties:
#> wrong length: expected 2, actual 3
cast(a, "numeric", length = 3)
declare(b, "character")

is.typed(a)
#> [1] TRUE
#> attr(,"type")
#> [1] "numeric"
#> attr(,"properties")
#> attr(,"properties")$length
#> [1] 3
is.valid(a)
#> [1] TRUE

is.typed(b)
#> [1] TRUE
#> attr(,"type")
#> [1] "character"
#> attr(,"properties")
#> list()
is.valid(b)
#> Error in is.valid(b): No such object: b
```

### Assignment Operators

There are two different operators for typed assignment: `%<-s%` and `%<-%`. The former one is kind of more "secure", since the variable (apparently) is not modified if the assignment would violate its type or its properties.

``` r
a %<-s% 1:3
names(a) %<-% c("first", "second", "third")
a[1] %<-s% 2

a[4] %<-s% 4
#> Error in a[4] %<-s% 4: Typed assignment failed for variable 'a'. Reason:
#> wrong length: expected 3, actual 4
a
#>  first second  third 
#>      2      2      3

class(a) %<-s% "character"
#> Error in class(a) %<-s% "character": Typed assignment failed for variable 'a'. Reason:
#> wrong type: character
a
#>  first second  third 
#>      2      2      3
is.valid(a)
#> [1] TRUE
```

`%<-%`, on the other hand, might leave the variable in an invalid state.

``` r
b %<-% "Hello World!"

b %<-% 123
#> Error in b %<-% 123: This assignment invalidated the variable 'b'. Reason:
#> Wrong type: double
b
#> [1] 123
is.valid(b)
#> [1] FALSE
#> attr(,"error")
#> [1] "Wrong type: double"
```

The advantage of `%<-%` however is that it allows to modify a variable in place, while `%<-s%` always creates a copy of it. This is due to the laziness of the type checking: it is only done after the actual assignment was performed. Hence, `%<-s%` has to create a backup of the variable beforehand, which it can then restore if the assignment invalidated the variable. The reason behind this laziness is that it is not possible to reliably determine how an assignment will change a variable without actually trying it out. For example, how `var` will change due to the following assignment depends on the exact definition of `myFancyFun<-`.

``` r
myFancyFun(var) <- c(TRUE, NA, FALSE)
```

It is almost always better to use the quicker and more memory friendly `%<-%`, except maybe when using R interactively.

#### RStudio Addins

The package ships with addins for RStudio (&gt;= v0.99.878) for quickly inserting "%&lt;-%" and "%&lt;-s%". Go to Tools &gt; Addins &gt; Browse Addins... &gt; Keyboard Shortcuts... to assign keyboard shortcuts for this.

### Secure Get

For all functions not part of lazyTyper, a typed variable in no way differs from an untyped one. This also implies that one can use the base assignment operator to assign an invalid value to a typed variable. Therefore, the function `g` offers a secure way to access the value of a type variable.

``` r
a <- "I'm supposed to be numeric!"
g(a)
#> Error in g(a): Variable 'a' is not valid:
#> wrong type: character
```

For example in tight for-loops one might want to deliberately use the base assignment operate since it does not perform type checking and hence can be considerably faster.

### Constants

With `const` it is possible to mark an existing variable as constant. If it is modified afterwards, accessing it through `g` will fail.

``` r
my_const <- runif(5)
const(my_const)
g(my_const)
#> [1] 0.45508660 0.01938115 0.16323587 0.19126105 0.12070913
my_const <- 1:5
g(my_const)
#> Error in g(my_const): Variable 'my_const' is not valid:
#> Modified constant.
```

Built-in Types
--------------

Currently only "any", "numeric" and "character" are supported, the latter two only partially.

ToDo: describe the types with all their properties.

### Additional Properties, Reference Counting and Copying

ToDo: detail why checking certain properties will lead to a copy the next time the variable is modified.

Registering Custom Types
------------------------

You can easily register a custom type. To do so you need three things:

-   a type name, e.g. "symmetric\_matrix". By using the name of a built-in type you can overload it *for all newly typed variables* -- already typed variables will keep their built-in type.
-   a function to check the validity of the additional properties passed to either declare or cast when creating a typed variable. This function must accept a single formal argument `x`, namely a (possibly empty) named list of properties and it should throw an error if invalid properties are used. The names of `x` are guaranteed to be unique and unequal to the empty string.
-   a quoted expression used to test the validity of a typed variable by e.g. `is.valid`, `%<-%` or `g`. This expression is evaluated in the environment of the variable to test and should hence (ideally) not create any (temporary) objects. Within this expression you must refer to the variable to test by `.XXX.` and the list of properties is available as `.lazyTyper_properties.` This expression should never fail but rather set the logical value `.lazyTyper_valid` to `FALSE` if the variable is invalid. In this case the "error"-attribute of the variable `.lazyTyper_valid` should contain the reason(s) for the invalidity as a character vector. The recommended way to set `lazyTyper_valid` to `FALSE` and to add a(nother) error message to it is by calling the helper function `setInvalidWError("This is pasted to ", "a single error message!")`.

ToDo: explain how to register custom types and overload built-in types.
