#' @title Lazy matrices
#' @description Create a lazy matrix.
#' @param M a numeric matrix
#' @return An object of class \code{lazyMatrix}.
#' @export
#' @name lazyMatrix
#' @examples
#' library(lazyNumbers)
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

#' @exportS3Method as.double lazyMatrix
as.double.lazyMatrix <- function(x, ...) {
  lmx2nm(x@xptr)
}
