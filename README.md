lazyNumbers: exact floating-point arithmetic
================

<!-- badges: start -->

[![R-CMD-check](https://github.com/stla/lazyNumbers/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/stla/lazyNumbers/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

It is well-known that floating-point arithmetic is inexact even with
some simple operations. For example:

``` r
1 - 7 * 0.1 == 0.3
## [1] FALSE
```

This package provides the *lazy numbers*, which allow exact
floating-point arithmetic:

``` r
library(lazyNumbers)
x <- lazynb(1) - lazynb(7) * lazynb(0.1)
as.double(x) == 0.3
## [1] TRUE
```

Here is a more interesting example. Consider the following recursive
sequence:

``` r
u <- function(n) {
  if(n == 1) {
    return(1/7)
  }
  8 * u(n-1) - 1
}
```

It is clear that all terms of this sequence equal `1/7` (approx.
`0.1428571`). However:

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

When it is evaluated in double precision, the sequence becomes crazy.
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
# non-lazy:
M <- matrix(rnorm(9L), nrow = 3L, ncol = 3L)
invM <- solve(M)
M %*% invM == diag(3)
##       [,1] [,2] [,3]
## [1,] FALSE TRUE TRUE
## [2,] FALSE TRUE TRUE
## [3,] FALSE TRUE TRUE
# lazy:
M_lazy <- lazymat(M)
invM_lazy <- lazyInv(M_lazy)
as.double(M_lazy %*% invM_lazy) == diag(3)
##      [,1] [,2] [,3]
## [1,] TRUE TRUE TRUE
## [2,] TRUE TRUE TRUE
## [3,] TRUE TRUE TRUE
```

### Relative precision of the conversion to double

This is not always so nice… Some equalities which are mathematically
true are not always true with the lazy numbers. That can depend on the
relative precision of the conversion from lazy to double, which is
possible to set with the function `asDouble`. It is set to `1e-15` when
applying the function `as.double`.

For example the first equality we have seen does not hold true if we
decrease the relative precision:

``` r
x <- 1 - lazynb(7) * 0.1
asDouble(x, prec = 1e-16) == 0.3
## [1] FALSE
```

But we can get an interval containing the lazy number, and it contains
`0.3`:

``` r
( itv <- intervals(x) )
## [1] 0.3 0.3
(itv[1L] <= 0.3) && (0.3 <= itv[2L])
## [1] TRUE
```

And it is short:

``` r
diff(itv)
## [1] 5.551115e-17
```

In the example below, one has to decrease the precision to get the
equality:

``` r
set.seed(666L)
M <- matrix(rnorm(9L), nrow = 3L, ncol = 3L)
M_lazy <- lazymat(M)
invM_lazy <- lazyInv(M_lazy)
P <- M_lazy %*% invM_lazy
as.double(P) == diag(3)
##       [,1] [,2] [,3]
## [1,] FALSE TRUE TRUE
## [2,]  TRUE TRUE TRUE
## [3,]  TRUE TRUE TRUE
asDouble(P, prec = 1e-16) == diag(3)
##      [,1] [,2] [,3]
## [1,] TRUE TRUE TRUE
## [2,] TRUE TRUE TRUE
## [3,] TRUE TRUE TRUE
```

The coefficients of the identity matrix are included in the intervals of
the coefficients of `P`:

``` r
( itvs <- intervals(c(P)) )
##       [,1] [,2]
##  [1,]    1    1
##  [2,]    0    0
##  [3,]    0    0
##  [4,]    0    0
##  [5,]    1    1
##  [6,]    0    0
##  [7,]    0    0
##  [8,]    0    0
##  [9,]    1    1
(itvs[, 1L] <= c(diag(3))) & (c(diag(3)) <= itvs[, 2L])
## [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
```

And they are short:

``` r
itvs[, 2L] - itvs[, 1L]
## [1] 7.771561e-16 0.000000e+00 0.000000e+00 0.000000e+00 7.771561e-16
## [6] 0.000000e+00 0.000000e+00 0.000000e+00 7.771561e-16
```

## Blog post

[The lazy numbers in
R](https://laustep.github.io/stlahblog/posts/lazyNumbers.html)

## License

This package is provided under the GPL-3 license but it uses the C++
library CGAL. If you wish to use CGAL for commercial purposes, you must
obtain a license from the
[GeometryFactory](https://geometryfactory.com).
