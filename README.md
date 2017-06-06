
lazyTyper
=========

[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip) [![Build Status](https://travis-ci.org/akersting/lazyTyper.svg?branch=develop)](https://travis-ci.org/akersting/lazyTyper) [![Coverage Status](https://codecov.io/github/akersting/lazyTyper/coverage.svg?branch=develop)](https://codecov.io/github/akersting/lazyTyper) [![MIT License](https://img.shields.io/badge/license-MIT-brightgreen.svg)](https://opensource.org/licenses/MIT)

lazyTyper adds the concepts of strong and explicit typing to R. Before its first use, a variable can be declared to be of a specific type. Existing variables can be casted to a specific type. Assigning values of the wrong type to such typed variables will throw an error.

In addition to pure type information, it is also possible to associate additional properties with a variable, e.g. the length of a vector and whether it may contain NAs or not. Such properties are also validated when the variable is modified.

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
#> Checking for a new development version of 'lazyTyper' on GitHub ...
#> You already have the most recent version of this package.
#> 
#> Attaching package: 'lazyTyper'
#> The following objects are masked from 'package:base':
#> 
#>     remove, rm

a <- 1:3
cast(a, "numeric", length = 2)
#> Error in cast(a, "numeric", length = 2): 
#> [lazyTyperError -> castError] Could not cast variable 'a'.
#> [... -> invalidTypeError -> invalidPropertyValueError] The variable has the wrong length. Expected length: 2, actual length: 3.
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
#> Error in is.valid(b): 
#> [lazyTyperError -> validationError] Could not validate variable 'b'.
#> [... -> notExistingError] No such object.
```

### Assignment Operators

There are two different operators for typed assignment: `%<-s%` and `%<-%`. The former one is kind of more “secure”, since the variable (apparently) is not modified if the assignment would violate its type or its properties.

``` r
a %<-s% .(1:3)
names(a) %<-% .(c("first", "second", "third"))
a[1] %<-s% .(2)

a[4] %<-s% .(4)
#> Error in a[4] %<-s% .(4): 
#> [lazyTyperError -> typedAssignmentError] This assignment would invalidate the variable 'a'.
#> [... -> invalidTypeError -> invalidPropertyValueError] The variable has the wrong length. Expected length: 3, actual length: 4.
a
#>  first second  third 
#>      2      2      3

class(a) %<-s% .("character")
#> Error in class(a) %<-s% .("character"): 
#> [lazyTyperError -> typedAssignmentError] This assignment would invalidate the variable 'a'.
#> [... -> invalidTypeError] The variable has the wrong type. Expected: numeric, actual: character.
a
#>  first second  third 
#>      2      2      3
is.valid(a)
#> [1] TRUE
```

`%<-%`, on the other hand, might leave the variable in an invalid state.

``` r
b %<-% .("Hello World!")

b %<-% .(123)
#> Error in b %<-% .(123): 
#> [lazyTyperError -> typedAssignmentError] This assignment invalidated the variable 'b'.
#> [... -> invalidTypeError] The variable has the wrong type. Expected: character, actual: numeric.
b
#> [1] 123
is.valid(b)
#> [1] FALSE
#> attr(,"errors")
#> attr(,"errors")[[1]]
#> <invalidTypeError in is.valid(b): 
#> [lazyTyperError -> validationError] The variable 'b' is invalid.
#> [... -> invalidTypeError] The variable has the wrong type. Expected: character, actual: numeric.>
```

The advantage of `%<-%` however is that it allows to modify a variable in place, while `%<-s%` always creates a copy of it. This is due to the laziness of the type checking: it is only done after the actual assignment was performed. Hence, `%<-s%` has to create a backup of the variable beforehand, which it can then restore if the assignment invalidated the variable. The reason behind this laziness is that it is not possible to reliably determine how an assignment will change a variable without actually trying it out. For example, how `var` will change due to the following assignment depends on the exact definition of `myFancyFun<-`.

``` r
myFancyFun(var) <- c(TRUE, NA, FALSE)
```

It is almost always better to use the quicker and more memory friendly `%<-%`, except maybe when using R interactively.

For both operators the right hand side must be enclosed in `.()`. This ensures that assignment expressions where the right hand side contains binary operators like `+` or `&` are evaluated in correct or — to be more precise — expected order. The necessity for this arises from the high precedence of custom operators like `%<-%` and `%<-s%`. Because of this,

``` r
a %<-% 3 + 7
```

would actually be interpreted as

``` r
(a %<-% 3) + 7,
```

which is probably not what we want. Of course, wrapping the right hand side in normal parentheses would generally solve this problem. However, there is no way to test for their presence or absence from within the operator functions, while this can be done for `.()`, which technically is a function call.

#### RStudio Addins

The package ships with addins for RStudio (&gt;= v0.99.1111) for quickly inserting `%<-% .()` and `%<-s% .()`. Go to Tools &gt; Addins &gt; Browse Addins… &gt; Keyboard Shortcuts… to assign keyboard shortcuts for this.

### Secure Get

For all functions not part of lazyTyper, a typed variable in no way differs from an untyped one. This also implies that one can use the base assignment operator to assign an invalid value to a typed variable. Therefore, the function `g` offers a secure way to access the value of a type variable.

``` r
a <- "I'm supposed to be numeric!"
g(a)
#> Error in g(a): 
#> [lazyTyperError -> getError] Failed to get the variable 'a'.
#> [... -> invalidTypeError] The variable has the wrong type. Expected: numeric, actual: character.
```

For example in tight for-loops one might want to deliberately use the base assignment operate since it does not perform type checking and hence can be considerably faster.

### Constants

With `const` it is possible to mark an existing variable as constant. If it is modified afterwards, accessing it through `g` will fail.

``` r
set.seed(123)
my_const <- runif(5)
const(my_const)
g(my_const)
#> [1] 0.2875775 0.7883051 0.4089769 0.8830174 0.9404673
my_const <- 1:5
g(my_const)
#> Error in g(my_const): 
#> [lazyTyperError -> getError] Failed to get the constant 'my_const'.
#> [... -> invalidTypeError -> modifiedConstantError] The constand has been modified.
```

Built-in Types
--------------

For now, only type `"any"` and the vector types discussed below are supported.

### Vector Types

The following table shows the supported properties (along the columns) of the currently implemented built-in vector types (along the rows). They all inherit from / are aliases of the hidden (base) type `.vector`.

A cell which is not crossed out indicates that the respective property can be set when using that type with `declare` or `cast`. The value in such a cell is the default value of the property for that type.

A crossed out cell indicates that the respective property cannot be set for that type, i.e. it is fixed to the (crossed out) value in the cell.

The meaning of a completely empty or crossed out empty cell, i.e of an unset property, is given by the first row.

<table style="width:100%;">
<colgroup>
<col width="6%" />
<col width="6%" />
<col width="6%" />
<col width="6%" />
<col width="6%" />
<col width="6%" />
<col width="6%" />
<col width="6%" />
<col width="6%" />
<col width="7%" />
<col width="6%" />
<col width="6%" />
<col width="5%" />
<col width="6%" />
<col width="7%" />
</colgroup>
<thead>
<tr class="header">
<th align="right">Type</th>
<th align="center">length</th>
<th align="center">min_length</th>
<th align="center">max_length</th>
<th align="center">set</th>
<th align="center">min</th>
<th align="center">max</th>
<th align="center">whole</th>
<th align="center">pattern</th>
<th align="center">allow_duplicates</th>
<th align="center">allow_NA</th>
<th align="center">allow_NaN</th>
<th align="center">allow_NULL</th>
<th align="center">allow_missing</th>
<th align="center">type</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="right"><strong>.vector</strong></td>
<td align="center"><em>not checked</em></td>
<td align="center"><em>not checked</em></td>
<td align="center"><em>not checked</em></td>
<td align="center"><em>not checked</em></td>
<td align="center"><em>not checked</em></td>
<td align="center"><em>not checked</em></td>
<td align="center"><em>not checked</em></td>
<td align="center"><em>not checked</em></td>
<td align="center"><em>not checked</em></td>
<td align="center"><em>not checked</em></td>
<td align="center"><em>not checked</em></td>
<td align="center"><em>no/FALSE</em></td>
<td align="center"><em>no/FALSE</em></td>
<td align="center"><em>N/A</em></td>
</tr>
<tr class="even">
<td align="right"><strong>vector</strong></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center">—</td>
<td align="center">—</td>
<td align="center">—</td>
<td align="center">—</td>
<td align="center">—</td>
<td align="center"></td>
<td align="center">—</td>
<td align="center">—</td>
<td align="center"></td>
<td align="center"></td>
<td align="center"><del>“vector”</del></td>
</tr>
<tr class="odd">
<td align="right"><strong>logical</strong></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center">—</td>
<td align="center">—</td>
<td align="center">—</td>
<td align="center">—</td>
<td align="center">—</td>
<td align="center">—</td>
<td align="center"></td>
<td align="center">—</td>
<td align="center"></td>
<td align="center"></td>
<td align="center"><del>“logical”</del></td>
</tr>
<tr class="even">
<td align="right"><strong>bool</strong></td>
<td align="center"><del>1</del></td>
<td align="center">—</td>
<td align="center">—</td>
<td align="center">—</td>
<td align="center">—</td>
<td align="center">—</td>
<td align="center">—</td>
<td align="center">—</td>
<td align="center">—</td>
<td align="center">FALSE</td>
<td align="center">—</td>
<td align="center"></td>
<td align="center"></td>
<td align="center"><del>“logical”</del></td>
</tr>
<tr class="odd">
<td align="right"><strong>numeric</strong></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center">—</td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"><del>“numeric”</del></td>
</tr>
<tr class="even">
<td align="right"><strong>integer</strong></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center">—</td>
<td align="center"></td>
<td align="center"></td>
<td align="center"><del>TRUE</del></td>
<td align="center">—</td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"><del>“numeric”</del></td>
</tr>
<tr class="odd">
<td align="right"><strong>count</strong></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center">—</td>
<td align="center"><del>0</del></td>
<td align="center"></td>
<td align="center"><del>TRUE</del></td>
<td align="center">—</td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"><del>“numeric”</del></td>
</tr>
<tr class="even">
<td align="right"><strong>scalar</strong></td>
<td align="center"><del>1</del></td>
<td align="center">—</td>
<td align="center">—</td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center">—</td>
<td align="center">—</td>
<td align="center">FALSE</td>
<td align="center">FALSE</td>
<td align="center"></td>
<td align="center"></td>
<td align="center"><del>“numeric”</del></td>
</tr>
<tr class="odd">
<td align="right"><strong>character</strong></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center">—</td>
<td align="center">—</td>
<td align="center">—</td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center">—</td>
<td align="center"></td>
<td align="center"></td>
<td align="center"><del>“character”</del></td>
</tr>
<tr class="even">
<td align="right"><strong>string</strong></td>
<td align="center"><del>1</del></td>
<td align="center">—</td>
<td align="center">—</td>
<td align="center"></td>
<td align="center">—</td>
<td align="center">—</td>
<td align="center">—</td>
<td align="center"></td>
<td align="center">—</td>
<td align="center">FALSE</td>
<td align="center">—</td>
<td align="center"></td>
<td align="center"></td>
<td align="center"><del>“character”</del></td>
</tr>
</tbody>
</table>

#### The Properties

-   `length`/`min_length`/`max_length`: the allowed exact/minimum/maximum length of the vector. If `length` is set then `min_length` and `max_length` cannot be set. `max_length` must be larger equal `min_length`.
-   `set`: a vector of allowed values. If set, then `min`, `max`, `whole` and `pattern` cannot be set. If `set` contains at least one `NA`/`NaN` then `allow_NA`/`allow_NaN` is set to `TRUE`.
-   `min`/`max`: the allowed minimum/maximum value to be stored in the vector. `max` must be larger equal `min`.
-   `whole`: if `TRUE` only approximately whole numbers may be stored in the vector, i.e. the difference between all values and their nearest whole number must be less or equal `.Machine$double.eps^0.5`.
-   `pattern`: only allow values in the vector which this regular expression matches.
-   `allow_duplicates`: are duplicated values allowed in the vector? `NA` and `NaN` (for numeric vectors) are treated as incomparable.
-   `allow_NA`/`allow_NaN`: are `NA`/`NaN` values allowed in the vector? `allow_NA` here strictly refers only to `NA` (and not `NaN`), i.e. `any(is.na(x) & !is.nan(x))` is used to test this. If `set` contains `NA`/`NaN` then `allow_NA`/`allow_NaN` must not be `FALSE`.
-   `allow_NULL`: may the whole variable be `NULL`? Note that the default does not allow this, i.e. not setting `allow_NULL` it is equivalent to setting `allow_NULL = FALSE`.
-   `allow_missing`: may the variable be a missing argument, i.e. identical to the empty name? This is relevant when casting a formal argument of a function. Note that the default does not allow this, i.e. not setting `allow_missing` it is equivalent to setting `allow_missing = FALSE`.

Registering Custom Types
------------------------

ToDo: explain how to register custom types and overload built-in types.

Advanced: Dynamic Properties
----------------------------

In `declare` and `cast` the aditional properties can be language objects (names, calls or expressions). Every time the typed variable is checked, such property specifications are re-evaluated in the environment of the variable:

``` r
X <- iris[1:100, ]
declare("weights", "numeric", length = quote(nrow(X)))
weights %<-% .(rep(1, 100))

X <- iris[1:50, ]
is.valid(weights)
#> [1] FALSE
#> attr(,"errors")
#> attr(,"errors")[[1]]
#> <invalidPropertyValueError in is.valid(weights): 
#> [lazyTyperError -> validationError] The variable 'weights' is invalid.
#> [... -> invalidTypeError -> invalidPropertyValueError] The variable has the wrong length. Expected length: 50, actual length: 100.>
```
