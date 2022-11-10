#' @name transpose-lazyMatrix
#' @aliases t t,lazyMatrix-method t,lazyVector-method
#' @title Transposition of lazy matrices
#' @description Transpose a \code{lazyMatrix} object.
#' @param x a \code{lazyMatrix} or \code{lazyVector} object
#' @return A \code{lazyMatrix} object.
#' @exportMethod t
setMethod(
  "t",
  signature(x = "lazyVector"),
  function(x) {
    rbind2(x)
  }
)

#' @rdname transpose-lazyMatrix
setMethod(
  "t",
  signature(x = "lazyMatrix"),
  function(x) {
    xptr = lazyTranspose(x@xptr)
    new("lazyMatrix", xptr = xptr, nrow = x@ncol, ncol = x@nrow)
  }
)
