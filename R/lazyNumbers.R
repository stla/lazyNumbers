#' @title Lazy numbers
#' @description Create a lazy number.
#' @param x a numeric vector
#' @return An object of class \code{lazyNumber}.
#' @export
#' @name lazyNumber
#' @examples
#' library(lazyNumbers)
#' 1 - 7 * 0.1 == 0.3 # FALSE
#' x <- lazynb(1) - lazynb(7) * lazynb(0.1)
#' as.double(x) == 0.3 # TRUE
as.lazyNumber <- function(x) UseMethod("as.lazyNumber")

#' @rdname lazyNumber
#' @export
lazynb <- function(x) as.lazyNumber(x)

as.lazyNumber.lazyNumber <- function(x) x

as.lazyNumber.numeric <- function(x) {
  if(any(is.na(x) | is.infinite(x))) {
    stop("Found NA/NaN/Inf values in `x`.", call. = FALSE)
  }
  new("lazyNumber", xptr = nv2lvx(x), length = length(x))
}

as.lazyNumber.integer <- function(x) {
  as.lazyNumber.numeric(as.double(x))
}

#' @exportS3Method as.double lazyNumber
as.double.lazyNumber <- function(x, ...) {
  lvx2nv(x@xptr)
}