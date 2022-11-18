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
  # if(any(is.na(x) | is.infinite(x))) {
  #   stop("Found NA/NaN/Inf values in `x`.", call. = FALSE)
  # }
  new("lazyVector", xptr = nv2lvx(x), length = length(x))
}

as.lazyVector.integer <- function(x) {
  as.lazyVector.numeric(as.double(x))
}

as.lazyVector.logical <- function(x) {
  as.lazyVector.numeric(as.double(x))
}

as.lazyVector.matrix <- function(x) {
  storage.mode(x) <- "double"
  as.lazyVector.numeric(c(x))
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
  lvx2nv(x@xptr)
}

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
#' @note Once you call \code{as.double} on a lazy number, 
#'   then this number is resolved (see the example).
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
  } else {
    . <- MlazyExact(x@xptr)
  }
  invisible(x)
}

# #' @title Copy a lazy object
# #' @description Make a copy of a lazy vector or a lazy matrix.
# #'
# #' @param x a \code{lazyVector} object or a \code{lazyMatrix} object
# #'
# #' @return A \code{lazyVector} object or a \code{lazyMatrix} object, a copy 
# #'   of \code{x}.
# #' @export
# lazyCopy <- function(x) {
#   stopifnot(inherits(x, "lazyVector") || inherits(x, "lazyMatrix"))
#   if(inherits(x, "lazyVector")) {
#     new("lazyVector", xptr = lazyDuplicate(x@xptr), length = x@length)
#   } else {
#     new("lazyMatrix", xptr = MlazyDuplicate(x@xptr), nrow = x@nrow, ncol = x@ncol)
#   }
# }

#' @name is.na
#' @aliases is.na,lazyVector-method is.na,lazyMatrix-method anyNA anyNA,lazyVector-method anyNA,lazyMatrix-method
#' @title Missing lazy values
#' @description Check whether values are missing in lazy vectors and lazy 
#'   matrices.
#' @param x a lazy vector or a lazy matrix
#' @param recursive ignored
#' @return The \code{is.na} function returns a logical vector or a 
#'   logical matrix, and the \code{anyNA} function returns a logical value.
#' @note The \code{is.na} function does not detect lazy NaN numbers; see 
#'   the note in \code{\link{isNaN_or_Inf}}.
#' @exportMethod is.na
#' @exportMethod anyNA
#' @docType methods
#' @examples 
#' is.na(NA_lazy_)
#' is.na(lazyvec(c(1, 2, NA, NaN, Inf)))
#' anyNA(lazyvec(c(1, 2, NA)))
setMethod(
  "is.na",
  signature = "lazyVector",
  function(x) {
    isLazyNA(x@xptr)
  }
)

#' @rdname is.na
setMethod(
  "is.na",
  signature = "lazyMatrix",
  function(x) {
    MisLazyNA(x@xptr)
  }
)

#' @rdname is.na
setMethod(
  "anyNA",
  signature = "lazyVector",
  function(x, recursive = FALSE) {
    anyLazyNA(x@xptr)
  }
)

#' @rdname is.na
setMethod(
  "anyNA",
  signature = "lazyMatrix",
  function(x, recursive = FALSE) {
    ManyLazyNA(x@xptr)
  }
)

#' @name NA_lazy_
#' @title The missing lazy value.
#' @export
"NA_lazy_"

#' @importFrom stats na.omit
#' @exportS3Method na.omit lazyVector
na.omit.lazyVector <- function(object, ...) {
  xptr <- lazyNAomit(object@xptr)
  l <- attr(xptr, "length")
  attr(xptr, "length") <- NULL
  new("lazyVector", xptr = xptr, length = l)
}

#' @title Lazy infinite or NaN numbers
#' @description Check whether values are infinite or NaN in lazy vectors and 
#'   lazy matrices.
#' @param x a lazy vector or a lazy matrix
#' @return A logical vector or a logical matrix.
#' @export
#' @note If you want to check whether a lazy number is infinite or whether 
#'   a lazy number is NaN, you have to call `as.double`. There is no way to 
#'   distinguish an infinite lazy number from a NaN lazy number without 
#'   resorting to its double approximation.
#' @examples 
#' isNaN_or_Inf(lazyvec(c(1, NaN, NA, Inf)))
isNaN_or_Inf <- function(x) {
  stopifnot(inherits(x, "lazyVector") || inherits(x, "lazyMatrix"))
  if(inherits(x, "lazyVector")) {
    isLazyVectorNaN_or_Inf(x@xptr)    
  } else {
    isLazyMatrixNaN_or_Inf(x@xptr)
  }
}

#' @exportS3Method rep lazyVector
rep.lazyVector <- function(x, times = 1, length.out = NA, each = 1, ...) {
  x[rep(1L:x@length, times = times, length.out = length.out, each = each)]
}

#' @exportS3Method length lazyVector
length.lazyVector <- function(x) {
  x@length
}

#' @exportS3Method dim lazyVector
dim.lazyVector <- function(x) {
  NULL
}
