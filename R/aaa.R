#' @useDynLib lazyNumbers, .registration=TRUE
#' @importFrom Rcpp evalCpp
#' @importFrom methods setMethod setClass new
NULL

setClass(
  "lazyNumber",
  slots = c(xptr = "externalptr", length = "integer")
)

setClass(
  "lazyMatrix",
  slots = c(xptr = "externalptr", nrow = "integer", ncol = "integer")
)
