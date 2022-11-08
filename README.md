lazyNumbers: exact floating-point arithmetic
================

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
x <- ln(1) - ln(7) * ln(0.1)
as.double(x) == 0.3
## [1] TRUE
```

## License

This package is provided under the GPL-3 license but it uses the C++
library CGAL. If you wish to use CGAL for commercial purposes, you must
obtain a license from the
[GeometryFactory](https://geometryfactory.com).
