#' @title Lazy numbers
#' @description Create a lazy number.
#' @param x a numeric vector
#' @return An object of class \code{lazyNumber}.
#' @export
#' @name lazyNumber
as.lazyNumber <- function(x) UseMethod("as.lazyNumber")

#' @rdname lazyNumber
#' @export
lazynb <- function(x) as.lazyNumber(x)

as.lazyNumber.lazyNumber <- function(x) x

as.lazyNumber.numeric <- function(x) {
  new("lazyNumber", xptr = nv2lvx(x), length = length(x))
}

as.lazyNumber.integer <- function(x) {
  as.lazyNumber.numeric(as.double(x))
}

#' @exportS3Method as.double lazyNumber
as.double.lazyNumber <- function(x, ...) {
  lvx2nv(x@xptr)
}