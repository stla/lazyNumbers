#' @name Subvector
#' @aliases [,lazyVector,numeric-method [,lazyVector,numeric,ANY,ANY-method [,lazyVector,logical-method [<-,lazyVector,numeric,missing,lazyVector-method
#' @title Extract/replace in a lazy vector
#' @description Extract or replace elements in a lazy vector.
#' @param x a \code{lazyVector} object
#' @param i indices
#' @param j nothing
#' @param value a \code{lazyVector} object
#' @return A \code{lazyVector} object.
setMethod(
  "[", 
  signature("lazyVector", i = "numeric"), 
  function(x, i) {
    if(isIndexVector(-i)) {
      if(any(i < -x@length)) {
        stop("Too large index.")
      }
      i <- setdiff(1L:x@length, -i)
    } else if(isIndexVector(i)) {
      if(any(i > x@length)) {
        stop("Too large index.")
      }
    } else {
      stop("Invalid indices.")
    }
    lvx <- lazyExtract(x@xptr, as.integer(i) - 1L)
    new("lazyVector", xptr = lvx, length = length(i))
  }
)

#' @rdname Subvector
setMethod(
  "[", 
  signature("lazyVector", i = "logical"), 
  function(x, i) {
    x[which(rep_len(i, length.out = x@length))]
  }
)


#' @rdname Subvector
setReplaceMethod(
  "[", 
  signature("lazyVector", i = "numeric", j = "missing", value = "lazyVector"), 
  function(x, i, j, value) {
    stopifnot(isIndexVector(i))
    if(any(i > x@length)) {
      stop("Too large index.")
    }
    if(length(i) != value@length) {
      stop("Incompatible lengths.")
    }
    lvx <- lazyReplace(x@xptr, as.integer(i) - 1L, value@xptr)
    new("lazyVector", xptr = lvx, length = x@length)
  }
)

#' @name Submatrix
#' @aliases [,lazyMatrix,numeric-method [,lazyMatrix,numeric,numeric,logical-method [,lazyMatrix,numeric,numeric-method [,lazyMatrix,numeric,missing-method [,lazyMatrix,missing,numeric-method
#' @title Extract lazy submatrix
#' @description Extract a submatrix of a lazy matrix.
#' @param x a \code{lazyMatrix} object
#' @param i,j indices
#' @param ... ignored
#' @param drop Boolean, whether to drop the matrix structure if \code{i} or 
#'   \code{j} has only one element
#' @return A \code{lazyMatrix} object or a \code{lazyVector} object.
setMethod(
  "[", 
  signature("lazyMatrix", i = "numeric", j = "numeric", drop = "logical"), 
  function(x, i, j, ..., drop = TRUE) {
    if(isIndexVector(-i)) {
      if(any(i < -x@nrow)) {
        stop("Too large row index.")
      }
      i <- setdiff(1L:x@nrow, -i)
    } else if(isIndexVector(i)) {
      if(any(i > x@nrow)) {
        stop("Too large row index.")
      }
    } else {
      stop("Invalid row indices.")
    }
    if(isIndexVector(-j)) {
      if(any(j < -x@ncol)) {
        stop("Too large column index.")
      }
      j <- setdiff(1L:x@ncol, -j)
    } else if(isIndexVector(j)) {
      if(any(j > x@ncol)) {
        stop("Too large column index.")
      }
    } else {
      stop("Invalid column indices.")
    }
    indices <- as.matrix(expand.grid(as.integer(i), as.integer(j))) - 1L
    m <- length(i)
    n <- length(j)
    lmx <- MlazyExtract(x@xptr, indices, m, n)
    if(drop && (m == 1L || n == 1L)) {
      new("lazyVector", xptr = lazyFlatten(lmx), length = m*n)
    } else {
      new("lazyMatrix", xptr = lmx, nrow = m, ncol = n)
    }
  }
)

#' @rdname Submatrix
setMethod(
  "[", 
  signature("lazyMatrix", i = "numeric", j = "numeric", drop = "missing"), 
  function(x, i, j, ..., drop) {
    x[i, j, drop = TRUE]
  }
)

#' @rdname Submatrix
setMethod(
  "[", 
  signature("lazyMatrix", i = "numeric", j = "missing", drop = "missing"), 
  function(x, i, j, ..., drop) {
    n_args <- nargs()
    if(n_args == 3L) {
      x[i, 1L:x@ncol, drop = TRUE]  
    } else if(n_args == 2L) {
      if(isIndexVector(-i)) {
        if(any(i < -x@nrow * x@ncol)) {
          stop("Too large index.")
        }
        i <- setdiff(1L:(x@nrow * x@ncol), -i)
      } else if(isIndexVector(i)) {
        if(any(i > x@nrow * x@ncol)) {
          stop("Too large index.")
        }
      } else {
        stop("Invalid indices.")
      }
      lmx <- x@xptr
      new(
        "lazyVector", 
        xptr = lazyExtract(lazyFlatten(lmx), i - 1L), 
        length = length(i)
      )
    } else {
      stop("Invalid arguments in subsetting.")
    }
  }
)

#' @rdname Submatrix
setMethod(
  "[", 
  signature("lazyMatrix", i = "numeric", j = "missing", drop = "logical"), 
  function(x, i, j, ..., drop = TRUE) {
    n_args <- nargs()
    if(n_args == 4L) {
      x[i, 1L:x@ncol, drop = drop]  
    } else if(n_args == 3L) {
      x[i]
    } else {
      stop("Invalid arguments in subsetting.")
    }
  }
)

#' @rdname Submatrix
setMethod(
  "[", 
  signature("lazyMatrix", i = "missing", j = "numeric", drop = "logical"), 
  function(x, i, j, drop = TRUE) {
    x[1L:x@nrow, j, drop = drop]
  }
)

#' @rdname Submatrix
setMethod(
  "[", 
  signature("lazyMatrix", i = "missing", j = "numeric", drop = "missing"), 
  function(x, i, j, drop) {
    x[1L:x@nrow, j, drop = TRUE]
  }
)
