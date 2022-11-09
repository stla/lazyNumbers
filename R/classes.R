#' @useDynLib lazyNumbers, .registration=TRUE
#' @importFrom Rcpp evalCpp
#' @importFrom methods setMethod setClass new
NULL

setClass(
  "lazyVector",
  slots = c(xptr = "externalptr", length = "integer")
)

setClass(
  "lazyMatrix",
  slots = c(xptr = "externalptr", nrow = "integer", ncol = "integer")
)

isInteger <- function(x) {
  is.numeric(x) && length(x) == 1L && !is.na(x) && as.integer(x) == x
}

isIndexVector <- function(x) {
  is.numeric(x) && !anyNA(x) && all(floor(x) == x) && all(x >= 1)
}
