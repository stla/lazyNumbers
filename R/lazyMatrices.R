#' @title Lazy matrices
#' @description Create a lazy matrix.
#' @param M a numeric matrix, a numeric vector, or a \code{lazyVector} object
#' @return An object of class \code{lazyMatrix}.
#' @export
#' @name lazyMatrix
#' @examples
#' library(lazyNumbers)
#' M <- lazymat(toeplitz(c(1, 2)))
#' as.double(M + M)
#' as.double(M * M)
#' as.double(M %*% M)
as.lazyMatrix <- function(M) UseMethod("as.lazyMatrix")

#' @rdname lazyMatrix
#' @export
lazymat <- function(M) as.lazyMatrix(M)

as.lazyMatrix.lazyMatrix <- function(M) M

as.lazyMatrix.matrix <- function(M) {
  stopifnot(is.numeric(M))
  storage.mode(M) <- "double"
  if(any(is.na(M) | is.infinite(M))) {
    stop("Found NA/NaN/Inf values in `M`.", call. = FALSE)
  }
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
  lmx2nm(x@xptr, 1e-15)
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