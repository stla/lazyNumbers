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
