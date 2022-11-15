#' @title Lazy vector
#' @description Create a vector of lazy numbers.
#' @param x a numeric vector or a lazy matrix (\code{lazyMatrix} object)
#' @return An object of class \code{lazyVector}.
#' @export
#' @name lazyVector
#' @examples
#' library(lazyNumbers)
#' x <- lazynb(1) - lazynb(7) * lazynb(0.1)
#' as.double(x)
#' # shorter:
#' x <- 1 - lazynb(7) * 0.1
as.lazyVector <- function(x) UseMethod("as.lazyVector")

#' @rdname lazyVector
#' @export
as.lazyNumber <- function(x) as.lazyVector(x)

#' @rdname lazyVector
#' @export
lazyvec <- function(x) as.lazyVector(x)

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

as.lazyVector.lazyMatrix <- function(x) {
  new(
    "lazyVector",
    xptr = lazyFlatten(x@xptr),
    length = x@nrow * x@ncol
  )
}

#' @exportS3Method as.double lazyVector
as.double.lazyVector <- function(x, ...) {
  lvx2nv(x@xptr, 1e-15)
}


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

#' @title Intervals for lazy numbers
#' @description For each lazy number in a \code{lazyVector} object or a 
#'   \code{lazyMatrix} object, this function computes an interval containing 
#'   this lazy number.
#'
#' @param x a \code{lazyVector} object or a \code{lazyMatrix} object
#'
#' @return A named list (\code{"inf"} and \code{"sup"}) containing: two numeric 
#'   vectors if \code{x} is a lazy vector, two numeric matrices if \code{x} is 
#'   a lazy matrix. 
#' @export
#'
#' @examples
#' library(lazyNumbers)
#' x <- lazynb(22) / lazynb(7)
#' itrv <- intervals(x)
#' print(itrv, digits = 17L)
#' x_dbl <- as.double(x)
#' itrv$inf <= x_dbl & x_dbl <= itrv$sup
intervals <- function(x) {
  stopifnot(inherits(x, "lazyVector") || inherits(x, "lazyMatrix"))
  if(inherits(x, "lazyVector")) {
    intervals_lvx(x@xptr)
  } else {
    intervals_lmx(x@xptr)
  }
}

#' @title Resolve lazy numbers
#' @description Resolve the lazy numbers in a lazy vector or a lazy matrix; 
#'   see details.
#'
#' @param x a \code{lazyVector} object or a \code{lazyMatrix} object
#'
#' @return Invisibly returns the lazy vector or matrix \code{x}, resolved.
#' @export
#' 
#' @details When an operation between lazy numbers is performed, the resulting 
#'   lazy number is not the result of the operation, it is the unevaluated 
#'   operation (wherefrom the word "lazy"). This function performs the 
#'   evaluation of the operations contained in the lazy numbers of the 
#'   vector/matrix; the returned lazy vector/matrix has the same values as the 
#'   input lazy vector/matrix. Applying this function can help to avoid a stack 
#'   overflow.
#'
#' @note Once you call \code{as.double} or \code{\link{asDouble}} on a lazy 
#'   number, then this number is resolved (see the example).
#'   
#' @examples 
#' \donttest{library(lazyNumbers)
#' n <- 500
#' p <- seq(1, n, by = 1)
#' q <- seq(3, 2*n + 1, by = 2)
#' # fast, because the operations are not evaluated:
#' x1 <- 2 * (1 + sum(cumprod(lazynb(p) / lazynb(q))))
#' x2 <- 2 * (1 + sum(cumprod(lazynb(p) / lazynb(q))))
#' x3 <- 2 * (1 + sum(cumprod(lazynb(p) / lazynb(q))))
#' # slow, because this evaluates the operations:
#' lazyResolve(x1)
#' # fast, because `x1` is resolved now:
#' as.double(x1)
#' # slow, because `x2` must be resolved:
#' as.double(x2)
#' # fast, because the call to `as.double` has resolved `x2`
#' as.double(x2)
#' # slow, because `x3` is not resolved:
#' x1 == x3
#' # fast, because `x3` has been resolved by the equality test:
#' as.double(x3)}
lazyResolve <- function(x) {
  stopifnot(inherits(x, "lazyVector") || inherits(x, "lazyMatrix"))
  if(inherits(x, "lazyVector")) {
    . <- lazyExact(x@xptr)
  } else{
    . <- MlazyExact(x@xptr)
  }
  invisible(x)
}
