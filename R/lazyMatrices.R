#' @title Lazy matrices
#' @description Create a lazy matrix.
#' @param x a numeric matrix, a numeric vector, a \code{lazyVector} object, or 
#'   a \code{lazyMatrix} object
#' @param dim ignored if \code{x} is a (possibly lazy) matrix; otherwise, i.e. 
#'   if \code{x} is a (possibly lazy) vector, then \code{dim} must be 
#'   \code{NULL} or a vector of two integers, and \code{NULL} is equivalent
#'   to \code{c(length(x), 1)} (a column matrix)
#' @return An object of class \code{lazyMatrix}.
#' @export
#' @name lazyMatrix
#' @examples
#' library(lazyNumbers)
#' M <- lazymat(toeplitz(c(1, 2)))
#' as.double(M + M)
#' as.double(M * M)
#' as.double(M %*% M)
as.lazyMatrix <- function(x) UseMethod("as.lazyMatrix")

#' @rdname lazyMatrix
#' @export
lazymat <- function(x, dim = NULL){
  if(inherits(x, "lazyMatrix")) {
    x
  } else if(is.matrix(x)) {
    storage.mode(x) <- "double"
    new("lazyMatrix", xptr = nm2lmx(x), nrow = nrow(x), ncol = ncol(x))
  } else if(is.numeric(x)) {
    if(is.null(dim)) {
      M <- as.matrix(x)
      new("lazyMatrix", xptr = nm2lmx(M), nrow = nrow(M), ncol = ncol(M))
    } else {
      dim(x) <- dim
      new("lazyMatrix", xptr = nm2lmx(x), nrow = nrow(x), ncol = ncol(x))
    }
  } else if(inherits(x, "lazyVector")) {
    if(is.null(dim)) {
      lvx <- x@xptr
      lmx <- lazyColumnMatrix(lvx)
      new("lazyMatrix", xptr = lmx, nrow = x@length, ncol = 1L)
    } else {
      stopifnot(length(dim) == 2L)
      dim <- as.integer(dim)
      lvx <- x@xptr
      xptr <- lazyVector2lazyMatrix(lvx, dim[1L], dim[2L])
      new("lazyMatrix", xptr = xptr, nrow = dim[1L], ncol = dim[2L])
    }
  } else {
    stop("Wrong argument `x`.")
  }
}

as.lazyMatrix.lazyMatrix <- function(M) M

as.lazyMatrix.matrix <- function(M) {
  #stopifnot(is.numeric(M))
  storage.mode(M) <- "double"
  # if(any(is.na(M) | is.infinite(M))) {
  #   stop("Found NA/NaN/Inf values in `M`.", call. = FALSE)
  # }
  new("lazyMatrix", xptr = nm2lmx(M), nrow = nrow(M), ncol = ncol(M))
}

as.lazyMatrix.numeric <- function(x) {
  as.lazyMatrix.matrix(as.matrix(x))
}

as.lazyMatrix.lazyVector <- function(lv) {
  lvx <- lv@xptr
  lmx <- lazyColumnMatrix(lvx)
  new("lazyMatrix", xptr = lmx, nrow = lv@length, ncol = 1L)
}

#' @exportS3Method as.double lazyMatrix
as.double.lazyMatrix <- function(x, ...) {
  lmx2nm(x@xptr)
}

#' @title Determinant of lazy matrix
#' @description Compute the determinant of a lazy matrix.
#'
#' @param M a \code{lazyMatrix} object corresponding to a square matrix
#'
#' @return A lazy number (\code{lazyVector} object with length 1).
#' @export
#'
#' @examples
#' M <- lazymat(toeplitz(c(3, 2, 1)))
#' as.double(lazyDet(M))
lazyDet <- function(M) {
  stopifnot(inherits(M, "lazyMatrix"))
  if(M@nrow != M@ncol) {
    stop("The matrix is not square.")
  }
  detx <- lazyDeterminant(M@xptr)
  new("lazyVector", xptr = detx, length = 1L)
}

#' @title Inverse of lazy matrix
#' @description Compute the inverse of a lazy matrix.
#'
#' @param M a \code{lazyMatrix} object corresponding to a square matrix
#'
#' @return A \code{lazyMatrix} object.
#' @export
#' 
#' @note This function does not check the invertibility. If the matrix is 
#'   not invertible, you will get some \code{NaN} in the result (after 
#'   calling \code{as.double}).
#'
#' @examples
#' library(lazyNumbers)
#' set.seed(666L)
#' M <- lazymat(matrix(rpois(9L, lambda = 4), nrow = 3L, ncol = 3L))
#' invM <- lazyInv(M)
#' I3 <- M %*% invM
#' as.double(I3) == diag(3)
lazyInv <- function(M) {
  stopifnot(inherits(M, "lazyMatrix"))
  if(M@nrow != M@ncol) {
    stop("The matrix is not square.")
  }
  invx <- lazyInverse(M@xptr)
  new("lazyMatrix", xptr = invx, nrow = M@nrow, ncol = M@ncol)
}

#' @name diag
#' @aliases diag,lazyMatrix-method diag<-,lazyMatrix,lazyVector-method
#' @title Extract/replace diagonal of a lazy matrix
#' @description Extract or replace the diagonal of a square lazy matrix.
#' @param x a square lazy matrix
#' @param value a lazy vector 
#' @return The diagonal of \code{x} as a lazy vector, or the modified matrix.
#' @exportMethod diag
#' @exportMethod diag<-
#' @docType methods
setMethod(
  "diag",
  signature = "lazyMatrix",
  function(x) {
    if(x@nrow != x@ncol) {
      stop("The matrix is not square.")
    }
    lvx <- lazyDiagonal(x@xptr)
    new("lazyVector", xptr = lvx, length = x@ncol)
  }
)

#' @rdname diag
setMethod(
  "diag<-",
  signature = c("lazyMatrix", "lazyVector"),
  function(x, value) {
    n <- x@nrow
    if(n != x@ncol) {
      stop("The matrix is not square.")
    }
    if(n != value@length) {
      stop("Incompatible dimensions.")
    }
    lmx <- lazyReplaceDiagonal(x@xptr, value@xptr)
    new("lazyMatrix", xptr = lmx, nrow = n, ncol = n)
  }
)

#' @exportS3Method length lazyMatrix
length.lazyMatrix <- function(x) {
  x@nrow * x@ncol
}

#' @exportS3Method dim lazyMatrix
dim.lazyMatrix <- function(x) {
  c(x@nrow, x@ncol)
}
