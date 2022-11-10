c_lv <- function(lv1, lv2) {
  xptr <- lazyConcat(lv1@xptr, lv2@xptr)
  l <- lv1@length + lv2@length
  new("lazyVector", xptr = xptr, length = l)
}

#' @name concat-lazyVectors
#' @aliases c,lazyVector-method
#' @title Concatenation of lazy vectors
#' @description Concatenate two or more \code{lazyVector} objects.
#' @param x a \code{lazyVector} object
#' @param ... some \code{lazyVector} objects
setMethod(
  "c",
  signature(x = "lazyVector"),
  function(x, ...) {
    if(nargs() == 1L) {
      x
    } else if(nargs() == 2L) {
      c_lv(x, ...)
    } else {
      c_lv(x, Recall(...))
    }
  }
)


setGeneric("cbind", signature = "...")
setGeneric("rbind", signature = "...")

cbind_lm <- function(lm1, lm2) {
  stopifnot(lm1@nrow == lm2@nrow)
  xptr <- lazyCbind(lm1@xptr, lm2@xptr)
  n <- lm1@ncol + lm2@ncol
  new("lazyMatrix", xptr = xptr, nrow = lm1@nrow, ncol = n)
}

rbind_lm <- function(lm1, lm2) {
  stopifnot(lm1@ncol == lm2@ncol)
  xptr <- lazyRbind(lm1@xptr, lm2@xptr)
  m <- lm1@nrow + lm2@nrow
  new("lazyMatrix", xptr = xptr, nrow = m, ncol = lm1@ncol)
}

as.lazyRowMatrix <- function(lv) {
  lvx <- lv@xptr
  lmx <- lazyRowMatrix(lvx)
  new("lazyMatrix", xptr = lmx, nrow = 1L, ncol = lv@length)
}


#' @name bind2-lazyMatrices
#' @aliases cbind2,lazyMatrix,missing-method cbind2,lazyMatrix,lazyMatrix-method cbind2,lazyVector,missing-method cbind2,lazyVector,lazyMatrix-method cbind2,lazyMatrix,lazyVector-method rbind2,lazyMatrix,missing-method rbind2,lazyMatrix,lazyMatrix-method
#' @title Concatenation of lazy matrices
#' @description Concatenate two \code{lazyMatrix} objects.
#' @param x,y \code{lazyMatrix} objects
setMethod(
  "cbind2",
  signature(x = "lazyMatrix", y = "missing"),
  function(x, y) {
    x
  }
)

#' @rdname bind2-lazyMatrices
setMethod(
  "cbind2",
  signature(x = "lazyVector", y = "missing"),
  function(x, y) {
    as.lazyMatrix.lazyVector(x)
  }
)

#' @rdname bind2-lazyMatrices
setMethod(
  "cbind2",
  signature(x = "lazyMatrix", y = "lazyMatrix"),
  function(x, y) {
    cbind_lm(x, y)
  }
)

#' @rdname bind2-lazyMatrices
setMethod(
  "cbind2",
  signature(x = "lazyVector", y = "lazyMatrix"),
  function(x, y) {
    cbind_lm(as.lazyMatrix.lazyVector(x), y)
  }
)

#' @rdname bind2-lazyMatrices
setMethod(
  "cbind2",
  signature(x = "lazyMatrix", y = "lazyVector"),
  function(x, y) {
    cbind_lm(x, as.lazyMatrix.lazyVector(y))
  }
)

#' @rdname bind2-lazyMatrices
setMethod(
  "rbind2",
  signature(x = "lazyMatrix", y = "missing"),
  function(x, y) {
    x
  }
)

#' @rdname bind2-lazyMatrices
setMethod(
  "rbind2",
  signature(x = "lazyMatrix", y = "lazyMatrix"),
  function(x, y) {
    rbind_lm(x, y)
  }
)

#' @name bind-lazyMatrices
#' @aliases cbind,lazyMatrix-method rbind,lazyMatrix-method
#' @title Concatenation of lazy matrices
#' @description Concatenate two or more \code{lazyMatrix} objects.
#' @param ... some \code{lazyMatrix} objects
#' @param deparse.level ignored
#' @exportMethod cbind
#' @exportMethod rbind
setMethod(
  "cbind",
  signature("lazyMatrix"),
  function(..., deparse.level) {
    if(nargs() <= 2L) {
      cbind2(...)
    } else {
      xs <- list(...)
      cbind2(xs[[1L]], do.call(Recall, xs[-1L]))
    }
  }
)

#' @rdname bind-lazyMatrices
setMethod(
  "rbind",
  signature("lazyMatrix"),
  function(..., deparse.level) {
    if(nargs() <= 2L) {
      rbind2(...)
    } else {
      xs <- list(...)
      rbind2(xs[[1L]], do.call(Recall, xs[-1L]))
    }
  }
)
