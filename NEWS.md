# lazyNumbers 1.2.2

No change. This is a resubmission because this package has been archived due to 
an issue with the RcppCGAL package.


# lazyNumbers 1.2.1

Added `include <optional>` in the header because of some errors on CRAN.


# lazyNumbers 1.2.0

First CRAN-released version.

* It is now possible to deal with missing values in lazy vectors and lazy matrices.

* The function `asDouble` has been removed since its `prec` argument caused no difference.

* The summary functions `min`, `max`, `range`, `sum` and `prod` gain the `na.rm` argument.

* New functions `diag` and `diag<-`.

* The function `lazymat` gains a `dim` argument.


# lazyNumbers 1.1.0

Second version, not released on CRAN.


# lazyNumbers 1.0.0

First version, not released on CRAN.