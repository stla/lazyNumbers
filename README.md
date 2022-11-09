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

## License

This package is provided under the GPL-3 license but it uses the C++
library CGAL. If you wish to use CGAL for commercial purposes, you must
obtain a license from the
[GeometryFactory](https://geometryfactory.com).
