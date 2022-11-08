#' @include aaa.R
NULL

setMethod(
  "+", 
  signature(e1 = "lazyNumber", e2 = "missing"), 
  function(e1, e2) e1
)
setMethod(
  "-", 
  signature(e1 = "lazyNumber", e2 = "missing"), 
  function(e1, e2) {
    new("lazyNumber", xptr = minus_lvx(e1@xptr), length = e1@length)
  }
)

lazyNumber_arith_lazyNumber <- function(e1, e2) {
  switch(
    .Generic,
    "+" = new(
      "lazyNumber", 
      xptr = lvx_plus_lvx(e1@xptr, e2@xptr), 
      length = max(e1@length, e2@length)
    ),
    "-" = new(
      "lazyNumber", 
      xptr = lvx_minus_lvx(e1@xptr, e2@xptr), 
      length = max(e1@length, e2@length)
    ),
    "*" = new(
      "lazyNumber", 
      xptr = lvx_times_lvx(e1@xptr, e2@xptr), 
      length = max(e1@length, e2@length)
    ),
    "/" = new(
      "lazyNumber", 
      xptr = lvx_dividedby_lvx(e1@xptr, e2@xptr), 
      length = max(e1@length, e2@length)
    ),
    stop(gettextf(
      "Binary operator %s not defined for lazy numbers.", dQuote(.Generic)
    ))
  )
}

lazyNumber_arith_numeric <- function(e1, e2) {
  switch(
    .Generic,
    "+" = e1 + as.lazyNumber(e2),
    "-" = e1 - as.lazyNumber(e2),
    "*" = e1 * as.lazyNumber(e2),
    "/" = e1 / as.lazyNumber(e2),
    stop(gettextf(
      "Binary operator %s not defined for lazy numbers.", dQuote(.Generic)
    ))
  )
}

numeric_arith_lazyNumber <- function(e1, e2) {
  switch(
    .Generic,
    "+" = as.lazyNumber(e1) + e2,
    "-" = as.lazyNumber(e1) - e2,
    "*" = as.lazyNumber(e1) * e2,
    "/" = as.lazyNumber(e1) / e2,
    stop(gettextf(
      "Binary operator %s not defined for lazy numbers.", dQuote(.Generic)
    ))
  )
}

setMethod(
  "Arith", 
  signature(e1 = "lazyNumber", e2 = "lazyNumber"), 
  lazyNumber_arith_lazyNumber
)

setMethod(
  "Arith", 
  signature(e1 = "lazyNumber", e2 = "numeric"), 
  lazyNumber_arith_numeric
)

setMethod(
  "Arith", 
  signature(e1 = "numeric", e2 = "lazyNumber"), 
  numeric_arith_lazyNumber
)

# lazy matrices ####

setMethod(
  "+", 
  signature(e1 = "lazyMatrix", e2 = "missing"), 
  function(e1, e2) e1
)
setMethod(
  "-", 
  signature(e1 = "lazyMatrix", e2 = "missing"), 
  function(e1, e2) {
    new("lazyMatrix", xptr = minus_lmx(e1@xptr), nrow = e1@nrow, ncol = e1@ncol)
  }
)

lazyMatrix_arith_lazyMatrix <- function(e1, e2) {
  stopifnot(e1@row == e2@row, e1@col == e2@col)
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
    stop(gettextf(
      "Binary operator %s not defined for lazy matrices.", dQuote(.Generic)
    ))
  )
}

numeric_arith_lazyMatrix <- function(e1, e2) {
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
  signature(e1 = "lazyMatrix", e2 = "numeric"), 
  lazyMatrix_arith_numeric
)

setMethod(
  "Arith", 
  signature(e1 = "numeric", e2 = "lazyMatrix"), 
  numeric_arith_lazyMatrix
)
