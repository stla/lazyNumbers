lazyNumbers: exact floating-point arithmetic
================

<!-- badges: start -->

[![R-CMD-check](https://github.com/stla/lazyNumbers/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/stla/lazyNumbers/actions/workflows/R-CMD-check.yaml)
[![R-CMD-check-valgrind](https://github.com/stla/lazyNumbers/actions/workflows/R-CMD-check-valgrind.yaml/badge.svg)](https://github.com/stla/lazyNumbers/actions/workflows/R-CMD-check-valgrind.yaml)
<!-- badges: end -->

It is well-known that floating-point arithmetic is inexact even with
some simple operations. For example:

``` r
1 - 7*0.1 == 0.3
## [1] FALSE
```

This package provides the *lazy numbers*, which allow exact
floating-point arithmetic. These numbers do *not* solve the above issue:

``` r
library(lazyNumbers)
x <- lazynb(1) - lazynb(7)*lazynb(0.1)
as.double(x) == 0.3
## [1] FALSE
```

Actually one can equivalent define `x` by, shorter, `1 - lazynb(7)*0.1`.

The above equality does not hold true because `0.1` and `0.3` as double
numbers do not exactly represent the true numbers `0.1` and `0.3`:

``` r
print(0.3, digits = 17L)
## [1] 0.29999999999999999
```

Whole numbers are exactly represented. The following equality is true:

``` r
x <- 1 - lazynb(7)/10
as.double(x) == 0.3
## [1] TRUE
```

It is also possible to compare lazy numbers between them:

``` r
y <- lazynb(3)/lazynb(10)
x == y
## [1] TRUE
```

And one can get a thin interval containing the exact value:

``` r
print(intervals(x), digits = 17L)
## $inf
## [1] 0.29999999999999999
## 
## $sup
## [1] 0.30000000000000004
```

Here is a more concrete example illustrating the benefits of the lazy
numbers. Consider the following recursive sequence:

``` r
u <- function(n) {
  if(n == 1) {
    return(1/7)
  }
  8 * u(n-1) - 1
}
```

It is clear that all terms of this sequence equal `1/7` (approx.
`0.1428571`). However this sequence becomes crazy as `n` increases:

``` r
u(15)
## [1] 0.1428223
u(18)
## [1] 0.125
u(20)
## [1] -1
u(30)
## [1] -1227133513
```

This is not the case of its lazy version:

``` r
u <- function(n) {
  if(n == 1) {
    return(1/lazynb(7))
  }
  8 * u(n-1) - 1
}
as.double(u(30))
## [1] 0.1428571
```

Vectors of lazy numbers and matrices of lazy numbers are implemented. It
is possible to get the determinant and the inverse of a square lazy
matrix:

``` r
set.seed(314159L)
# non-lazy:
M <- matrix(rnorm(9L), nrow = 3L, ncol = 3L)
invM <- solve(M)
M %*% invM == diag(3L)
##       [,1] [,2]  [,3]
## [1,] FALSE TRUE FALSE
## [2,] FALSE TRUE FALSE
## [3,] FALSE TRUE  TRUE
# lazy:
M_lazy <- lazymat(M)
invM_lazy <- lazyInv(M_lazy)
as.double(M_lazy %*% invM_lazy) == diag(3L)
##      [,1] [,2] [,3]
## [1,] TRUE TRUE TRUE
## [2,] TRUE TRUE TRUE
## [3,] TRUE TRUE TRUE
```

### About laziness

The lazy numbers are called like this because when an operation is
performed between numbers, the resulting lazy number is not the result
of the operation; rather, it is the unevaluated operation. Therefore,
performing some operations on lazy numbers is fast, but a call to
`as.double`, which triggers the exact evaluation, can be slow. A call to
`intervals` is fast.

## Blog posts

[The lazy numbers in
R](https://laustep.github.io/stlahblog/posts/lazyNumbers.html).

[The lazy numbers in R:
correction](https://laustep.github.io/stlahblog/posts/lazyNumbers2.html).

## License

This package is provided under the GPL-3 license but it uses the C++
library CGAL. If you wish to use CGAL for commercial purposes, you must
obtain a license from the
[GeometryFactory](https://geometryfactory.com).
