#' @name ExtractOrReplace
#' @aliases [,lazyVector,numeric,missing-method
#' @title Extract/replace in a lazy vector
#' @description Extract or replace elements in a lazy vector.
#' @param x a \code{lazyVector} object
#' @param i indices
#' @param j nothing
#' @param drop ignored
setMethod(
  "[", 
  signature("lazyVector", i = "numeric", j = "missing", drop = "ANY"), 
  function(x, i, j, drop) {
    stopifnot(isIndexVector(i))
    if(any(i) > x@length) {
      stop("Too large index.")
    }
    lvx <- lazyExtract(x@xptr, as.integer(i))
    new("lazyVector", xptr = lvx, length = length(i))
  }
)