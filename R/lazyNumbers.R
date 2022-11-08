#' @title Lazy numbers
#' @description Create a lazy number.
#' @param x a numeric vector
#' @return An external pointer having class \code{lazyNumber}.
#' @export
as.lazyNumber <- function(x) UseMethod("as.lazyNumber")

as.lazyNumber.lazyNumber <- function(x) x

as.lazyNumber.numeric <- function(x) {
  y <- nv2lvx(x)
  class(y) <- "lazyNumber"
  y
}

as.lazyNumber.integer <- function(x) {
  as.lazyNumber.numeric(as.double(x))
}

#' @exportS3Method as.double lazyNumber
as.double.lazyNumber <- function(x, ...) {
  lvx2nv(x)
}