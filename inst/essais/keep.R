setGeneric("asDouble", function(x, prec) {
  stop("`asDouble` is not implemented for this signature.")
})

#' @name asDouble
#' @aliases asDouble,lazyVector,numeric-method asDouble,lazyMatrix,numeric-method asDouble,lazyVector,missing-method asDouble,lazyMatrix,missing-method
#' @title Coerce lazy numbers to double numbers
#' @description Coerce a lazy vector (\code{lazyVector} object) to a numeric 
#'   vector and a lazy matrix (\code{lazyMatrix} object) to a numeric matrix.
#' @param x a lazy vector or a lazy matrix
#' @param prec relative precision, a number between 0 and 1
#' @return A numeric vector or a numeric matrix.
#' @note Once \code{as.double} or \code{asDouble} (\code{as.double(x)} is 
#'   equivalent to \code{asDouble(x, prec = 1e-15)}) has been applied to a 
#'   lazy vector, then this lazy vector is resolved (see 
#'   \code{\link{lazyResolve}}). 
#' @exportMethod asDouble
#' @examples 
#' x <- 1 - lazynb(7) * 0.1
#' asDouble(x, prec = 1e-15) == 0.3
#' asDouble(x, prec = 1e-16) == 0.3
setMethod(
  "asDouble",
  signature(x = "lazyVector", prec = "numeric"),
  function(x, prec = 1e-15) {
    stopifnot(prec > 0, prec < 1)
    lvx2nv(x@xptr, prec)    
  }
)

#' @rdname asDouble
setMethod(
  "asDouble",
  signature(x = "lazyVector", prec = "missing"),
  function(x, prec) {
    lvx2nv(x@xptr, 1e-15)    
  }
)

#' @rdname asDouble
setMethod(
  "asDouble",
  signature(x = "lazyMatrix", prec = "numeric"),
  function(x, prec = 1e-15) {
    stopifnot(prec > 0, prec < 1)
    lmx2nm(x@xptr, prec)    
  }
)

#' @rdname asDouble
setMethod(
  "asDouble",
  signature(x = "lazyMatrix", prec = "missing"),
  function(x, prec) {
    lmx2nm(x@xptr, 1e-15)    
  }
)















setGeneric("cbind", signature = "...")
setGeneric("rbind", signature = "...")


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
