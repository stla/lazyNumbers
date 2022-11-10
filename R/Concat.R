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
  function(x, ...){
    if(nargs() == 1L) {
      x
    } else if(nargs() == 2L) {
      c_lv(x, ...)
    } else {
      c_lv(x, Recall(...))
    }
  }
)