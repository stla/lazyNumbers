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

#' @name bind-lazyMatrices
#' @aliases cbind,lazyMatrix-method rbind,lazyMatrix-method
#' @title Concatenation of lazy matrices
#' @description Concatenate two or more \code{lazyMatrix} objects.
#' @param ... some \code{lazyMatrix} objects
#' @param deparse.level ignored
setMethod(
  "cbind",
  signature(),
  function(..., deparse.level) {
    if(nargs() == 1L) {
      identity(...)
    } else if(nargs() == 2L) {
      cbind_lm(...)
    } else {
      xs <- list(...)
      cbind_lm(xs[[1L]], do.call(Recall, xs[-1L]))
    }
  }
)

#' @rdname bind-lazyMatrices
setMethod(
  "rbind",
  signature(),
  function(..., deparse.level) {
    if(nargs() == 1L) {
      identity(...)
    } else if(nargs() == 2L) {
      rbind_lm(...)
    } else {
      rbind_lm(xs[[1L]], do.call(Recall, xs[-1L]))
    }
  }
)
