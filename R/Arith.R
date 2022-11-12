#' @include classes.R
NULL

#' @name lazyVector-unary
#' @title Unary operators for lazy vectors
#' @description Unary operators for lazy vectors.
#' @aliases +,lazyVector,missing-method -,lazyVector,missing-method
#' @param e1 object of class \code{lazyVector}
#' @param e2 nothing
#' @return A \code{lazyVector} object.
setMethod(
  "+", 
  signature(e1 = "lazyVector", e2 = "missing"), 
  function(e1, e2) e1
)
#' @rdname lazyVector-unary
setMethod(
  "-", 
  signature(e1 = "lazyVector", e2 = "missing"), 
  function(e1, e2) {
    new("lazyVector", xptr = minus_lvx(e1@xptr), length = e1@length)
  }
)

lazyPow <- function(lvx, alpha) {
  stopifnot(isInteger(alpha))
  lazyPower(lvx, as.integer(alpha))
}

lazyVector_arith_lazyVector <- function(e1, e2) {
  switch(
    .Generic,
    "+" = new(
      "lazyVector", 
      xptr = lvx_plus_lvx(e1@xptr, e2@xptr), 
      length = max(e1@length, e2@length)
    ),
    "-" = new(
      "lazyVector", 
      xptr = lvx_minus_lvx(e1@xptr, e2@xptr), 
      length = max(e1@length, e2@length)
    ),
    "*" = new(
      "lazyVector", 
      xptr = lvx_times_lvx(e1@xptr, e2@xptr), 
      length = max(e1@length, e2@length)
    ),
    "/" = new(
      "lazyVector", 
      xptr = lvx_dividedby_lvx(e1@xptr, e2@xptr), 
      length = max(e1@length, e2@length)
    ),
    stop(gettextf(
      "Binary operator %s not defined for lazy vectors.", dQuote(.Generic)
    ))
  )
}

lazyVector_arith_numeric <- function(e1, e2) {
  switch(
    .Generic,
    "+" = e1 + as.lazyVector(e2),
    "-" = e1 - as.lazyVector(e2),
    "*" = e1 * as.lazyVector(e2),
    "/" = e1 / as.lazyVector(e2),
    "^" = new("lazyVector", xptr = lazyPow(e1@xptr, e2), length = e1@length),
    stop(gettextf(
      "Binary operator %s not defined for lazy vectors.", dQuote(.Generic)
    ))
  )
}

numeric_arith_lazyVector <- function(e1, e2) {
  switch(
    .Generic,
    "+" = as.lazyVector(e1) + e2,
    "-" = as.lazyVector(e1) - e2,
    "*" = as.lazyVector(e1) * e2,
    "/" = as.lazyVector(e1) / e2,
    stop(gettextf(
      "Binary operator %s not defined for lazy vectors.", dQuote(.Generic)
    ))
  )
}

setMethod(
  "Arith", 
  signature(e1 = "lazyVector", e2 = "lazyVector"), 
  lazyVector_arith_lazyVector
)

setMethod(
  "Arith", 
  signature(e1 = "lazyVector", e2 = "numeric"), 
  lazyVector_arith_numeric
)

setMethod(
  "Arith", 
  signature(e1 = "numeric", e2 = "lazyVector"), 
  numeric_arith_lazyVector
)

# lazy matrices ####

MlazyPow <- function(lm, alpha) {
  stopifnot(isInteger(alpha))
  lmx <- lm@xptr
  new(
    "lazyMatrix", 
    xptr = MlazyPower(lmx, as.integer(alpha)),
    nrow = lm@nrow,
    ncol = lm@ncol
  )
}

#' @name lazyMatrix-unary
#' @title Unary operators for lazy matrices
#' @description Unary operators for lazy matrices.
#' @aliases +,lazyMatrix,missing-method -,lazyMatrix,missing-method
#' @param e1 object of class \code{lazyMatrix}
#' @param e2 nothing
#' @return A \code{lazyMatrix} object.
setMethod(
  "+", 
  signature(e1 = "lazyMatrix", e2 = "missing"), 
  function(e1, e2) e1
)
#' @rdname lazyMatrix-unary 
setMethod(
  "-", 
  signature(e1 = "lazyMatrix", e2 = "missing"), 
  function(e1, e2) {
    new("lazyMatrix", xptr = minus_lmx(e1@xptr), nrow = e1@nrow, ncol = e1@ncol)
  }
)

#' @name lazyMatrix-product
#' @title Matricial product of lazy matrices
#' @description Matricial product of lazy matrices.
#' @aliases %*%,lazyMatrix,lazyMatrix-method %*%,lazyMatrix,matrix-method %*%,lazyMatrix,numeric-method %*%,matrix,lazyMatrix-method %*%,numeric,lazyMatrix-method
#' @param x,y objects of class \code{lazyMatrix}
#' @return A \code{lazyMatrix} object.
setMethod(
  "%*%", 
  signature(x = "lazyMatrix", y = "lazyMatrix"), 
  function(x, y) {
    stopifnot(x@ncol == y@nrow)
    new(
      "lazyMatrix", 
      xptr = lmx_times_lmx(x@xptr, y@xptr), 
      nrow = x@nrow, ncol = y@ncol
    )
  }
)
setMethod(
  "%*%", 
  signature(x = "lazyMatrix", y = "matrix"), 
  function(x, y) {
    x %*% as.lazyMatrix(y)
  }
)
setMethod(
  "%*%", 
  signature(x = "matrix", y = "lazyMatrix"), 
  function(x, y) {
    as.lazyMatrix(x) %*% y
  }
)
setMethod(
  "%*%", 
  signature(x = "lazyMatrix", y = "numeric"), 
  function(x, y) {
    x %*% as.lazyMatrix(y)
  }
)
setMethod(
  "%*%", 
  signature(x = "numeric", y = "lazyMatrix"), 
  function(x, y) {
    as.lazyMatrix(x) %*% y
  }
)

lazyMatrix_arith_lazyMatrix <- function(e1, e2) {
  stopifnot(e1@nrow == e2@nrow, e1@ncol == e2@ncol)
  switch(
    .Generic,
    "+" = new(
      "lazyMatrix", 
      xptr = lmx_plus_lmx(e1@xptr, e2@xptr), 
      nrow = e1@nrow, ncol = e1@ncol
    ),
    "-" = new(
      "lazyMatrix", 
      xptr = lmx_minus_lmx(e1@xptr, e2@xptr), 
      nrow = e1@nrow, ncol = e1@ncol
    ),
    "*" = new(
      "lazyMatrix", 
      xptr = lmx_cwtimes_lmx(e1@xptr, e2@xptr), 
      nrow = e1@nrow, ncol = e1@ncol
    ),
    "/" = new(
      "lazyMatrix", 
      xptr = lmx_dividedby_lmx(e1@xptr, e2@xptr), 
      nrow = e1@nrow, ncol = e1@ncol
    ),
    stop(gettextf(
      "Binary operator %s not defined for lazy matrices.", dQuote(.Generic)
    ))
  )
}

lazyMatrix_arith_matrix <- function(e1, e2) {
  m <- e1@nrow
  n <- e1@ncol
  if(length(e2) == 1L) {
    e2 <- matrix(e2, nrow = m, ncol = n)
  }
  switch(
    .Generic,
    "+" = e1 + as.lazyMatrix(e2),
    "-" = e1 - as.lazyMatrix(e2),
    "*" = e1 * as.lazyMatrix(e2),
    "/" = e1 / as.lazyMatrix(e2),
    stop(gettextf(
      "Binary operator %s not defined for lazy matrices.", dQuote(.Generic)
    ))
  )
}

lazyMatrix_arith_numeric <- function(e1, e2) {
  m <- e1@nrow
  n <- e1@ncol
  if(length(e2) == 1L) {
    e2 <- matrix(e2, nrow = m, ncol = n)
  }
  switch(
    .Generic,
    "+" = e1 + as.lazyMatrix(e2),
    "-" = e1 - as.lazyMatrix(e2),
    "*" = e1 * as.lazyMatrix(e2),
    "/" = e1 / as.lazyMatrix(e2),
    "^" = MlazyPow(e1, e2),
    stop(gettextf(
      "Binary operator %s not defined for lazy matrices.", dQuote(.Generic)
    ))
  )
}

matrix_arith_lazyMatrix <- function(e1, e2) {
  m <- e2@nrow
  n <- e2@ncol
  if(length(e1) == 1L) {
    e1 <- matrix(e1, nrow = m, ncol = n)
  }
  switch(
    .Generic,
    "+" = as.lazyMatrix(e1) + e2,
    "-" = as.lazyMatrix(e1) - e2,
    "*" = as.lazyMatrix(e1) * e2,
    "/" = as.lazyMatrix(e1) / e2,
    stop(gettextf(
      "Binary operator %s not defined for lazy matrices.", dQuote(.Generic)
    ))
  )
}

setMethod(
  "Arith", 
  signature(e1 = "lazyMatrix", e2 = "lazyMatrix"), 
  lazyMatrix_arith_lazyMatrix
)
setMethod(
  "Arith", 
  signature(e1 = "lazyMatrix", e2 = "matrix"), 
  lazyMatrix_arith_matrix
)
setMethod(
  "Arith", 
  signature(e1 = "matrix", e2 = "lazyMatrix"), 
  matrix_arith_lazyMatrix
)
setMethod(
  "Arith", 
  signature(e1 = "lazyMatrix", e2 = "numeric"), 
  lazyMatrix_arith_numeric
)
setMethod(
  "Arith", 
  signature(e1 = "numeric", e2 = "lazyMatrix"), 
  matrix_arith_lazyMatrix
)
