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

# "onion_arith_numeric" <- function(e1,e2){  # e1 onion, e2 numeric
#   switch(.Generic,
#          "+" = onion_plus_numeric (e1,  e2),
#          "-" = onion_plus_numeric (e1, -e2),
#          "*" = onion_prod_numeric (e1,  e2),
#          "/" = onion_prod_numeric (e1,1/e2),
#          "^" = onion_power_numeric(e1,  e2),
#          stop(gettextf("binary operator %s not defined for onions", dQuote(.Generic)))
#   )
# }
# 
# "numeric_arith_onion" <- function(e1,e2){ # e1 numeric, e2 onion
#   switch(.Generic,
#          "+" = onion_plus_numeric(e2,  e1),
#          "-" = onion_plus_numeric(-e2, e1),
#          "*" = onion_prod_numeric(e2,  e1),
#          "/" = onion_prod_numeric(onion_inverse(e2),e1),  # onions commute with numeric multiplication
#          "^" = stop("x^onion not defined"),
#          stop(gettextf("binary operator %s not defined for onions", dQuote(.Generic)))
#   )
# }


setMethod(
  "Arith", 
  signature(e1 = "lazyNumber", e2 = "lazyNumber"), 
  lazyNumber_arith_lazyNumber
)

# setMethod("Arith",signature(e1 = "onion"  , e2="numeric"),   onion_arith_numeric)
# setMethod("Arith",signature(e1 = "numeric", e2="onion"  ), numeric_arith_onion  )
