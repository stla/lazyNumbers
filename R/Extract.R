#' @name Extract
#' @aliases [,lazyVector,numeric,missing-method [<-,lazyVector,numeric,missing,lazyVector-method
#' @title Extract/replace in a lazy vector
#' @description Extract or replace elements in a lazy vector.
#' @param x a \code{lazyVector} object
#' @param i indices
#' @param j nothing
#' @param drop ignored
#' @param value a \code{lazyVector} object
#' @return A \code{lazyVector} object.
setMethod(
  "[", 
  signature("lazyVector", i = "numeric", j = "missing", drop = "ANY"), 
  function(x, i, j, drop) {
    if(isIndexVector(-i)) {
      if(any(i) < -x@length) {
        stop("Too large index.")
      }
      i <- setdiff(1L:x@length, -i)
    } else if(isIndexVector(i)) {
      if(any(i) > x@length) {
        stop("Too large index.")
      }
    } else {
      stop("Invalid indices.")
    }
    lvx <- lazyExtract(x@xptr, as.integer(i) - 1L)
    new("lazyVector", xptr = lvx, length = length(i))
  }
)

#' @rdname Extract
setReplaceMethod(
  "[", 
  signature("lazyVector", i = "numeric", j = "missing", value = "lazyVector"), 
  function(x, i, j, value) {
    stopifnot(isIndexVector(i))
    if(any(i) > x@length) {
      stop("Too large index.")
    }
    if(length(i) != value@length) {
      stop("Incompatible lengths.")
    }
    lvx <- lazyReplace(x@xptr, as.integer(i) - 1L, value@xptr)
    new("lazyVector", xptr = lvx, length = x@length)
  }
)

#' @rdname Extract
#' @aliases [,lazyMatrix,numeric,numeric-method
#' @title Extract lazy submatrix
#' @description Extract a submatrix of a lazy matrix.
#' @param x a \code{lazyMatrix} object
#' @param i,j indices
#' @param drop ignored
#' @param value a \code{lazyMatrix} object
#' @return A \code{lazyMatrix} object.
setMethod(
  "[", 
  signature("lazyMatrix", i = "numeric", j = "numeric", drop = "ANY"), 
  function(x, i, j, drop) {
    stopifnot(isIndexVector(i), isIndexVector(j))
    if(any(i > x@nrow)) {
      stop("Too large row index.")
    }
    if(any(j > x@ncol)) {
      stop("Too large column index.")
    }
    indices <- as.matrix(expand.grid(as.integer(i), as.integer(j))) - 1L
    lmx <- MlazyExtract(x@xptr, indices, length(i), length(j))
    new("lazyMatrix", xptr = lmx, nrow = length(i), ncol = length(j))
  }
)
