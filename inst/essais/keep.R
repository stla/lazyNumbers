#' @name bind-lazyMatrices
#' @aliases cbind rbind cbind,lazyMatrix-method rbind,lazyMatrix-method
#' @title Concatenation of lazy matrices
#' @description Concatenate two or more \code{lazyMatrix} objects.
#' @param ... some \code{lazyMatrix} objects
#' @param deparse.level ignored
#' @return A \code{lazyMatrix} object.
#' @exportMethod cbind
#' @exportMethod rbind
setMethod(
  "cbind",
  signature("lazyMatrix"),
  function(..., deparse.level) {
    if(nargs() <= 2L) {
      cbind2(...)
    } else {
      xs <- list(...)
      cbind2(xs[[1L]], do.call(Recall, xs[-1L]))
    }
  }
)

#' @rdname bind-lazyMatrices
setMethod(
  "rbind",
  signature("lazyMatrix"),
  function(..., deparse.level) {
    if(nargs() <= 2L) {
      rbind2(...)
    } else {
      xs <- list(...)
      rbind2(xs[[1L]], do.call(Recall, xs[-1L]))
    }
  }
)
