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
##       [,1] [,2]  [,3]
## [1,]  TRUE TRUE  TRUE
## [2,] FALSE TRUE  TRUE
## [3,]  TRUE TRUE FALSE
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
asDouble(1 - lazynb(7) * 0.1, prec = 1e-16) == 0.3
## [1] FALSE
```

But sometimes one has to decrease the precision:

``` r
set.seed(666L)
M <- matrix(rnorm(9L), nrow = 3L, ncol = 3L)
M_lazy <- lazymat(M)
invM_lazy <- lazyInv(M_lazy)
I3 <- M_lazy %*% invM_lazy
as.double(I3) == diag(3)
##       [,1] [,2] [,3]
## [1,] FALSE TRUE TRUE
## [2,]  TRUE TRUE TRUE
## [3,]  TRUE TRUE TRUE
asDouble(I3, prec = 1e-16) == diag(3)
##      [,1] [,2] [,3]
## [1,] TRUE TRUE TRUE
## [2,] TRUE TRUE TRUE
## [3,] TRUE TRUE TRUE
```

## Blog post

[The lazy numbers in
R](https://laustep.github.io/stlahblog/posts/lazyNumbers.html)

## License

This package is provided under the GPL-3 license but it uses the C++
library CGAL. If you wish to use CGAL for commercial purposes, you must
obtain a license from the
[GeometryFactory](https://geometryfactory.com).
