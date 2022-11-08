#' @title Lazy numbers
#' @description Create a lazy number.
#' @param x a numeric vector
#' @return An object of class \code{lazyNumber}.
#' @export
#' @name lazyVector
#' @examples
#' library(lazyNumbers)
#' 1 - 7 * 0.1 == 0.3 # FALSE
#' x <- lazynb(1) - lazynb(7) * lazynb(0.1)
#' as.double(x) == 0.3 # TRUE
as.lazyVector <- function(x) UseMethod("as.lazyVector")

#' @rdname lazyVector
#' @export
as.lazyNumber <- function(x) as.lazyVector(x)

#' @rdname lazyVector
#' @export
lazynb <- function(x) as.lazyVector(x)

as.lazyVector.lazyVector <- function(x) x

as.lazyVector.numeric <- function(x) {
  if(any(is.na(x) | is.infinite(x))) {
    stop("Found NA/NaN/Inf values in `x`.", call. = FALSE)
  }
  new("lazyVector", xptr = nv2lvx(x), length = length(x))
}

as.lazyVector.integer <- function(x) {
  as.lazyVector.numeric(as.double(x))
}

#' @exportS3Method as.double lazyVector
as.double.lazyVector <- function(x, ...) {
  lvx2nv(x@xptr)
}

#' @title Intervals for lazy numbers
#' @description For each lazy number in a \code{lazyVector} object, this function 
#'   returns an interval containing this lazy number.
#'
#' @param x a \code{lazyVector} object
#'
#' @return A numeric matrix with two columns and \code{x@length} rows.
#' @export
#'
#' @examples
#' library(lazyNumbers)
#' x <- lazynb(22) / lazynb(7)
#' print(intervals(x), digits = 17L)
intervals <- function(x) {
  stopifnot(inherits(x, "lazyVector"))
  intervals_lvx(x@xptr)
}