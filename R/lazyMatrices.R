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
  as.lazyMatrix.matrix(as.matrix(as.double(x)))
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
