setMethod(
  "Summary", "lazyNumber",
  function(x, ..., na.rm = FALSE) {
    switch(.Generic,
           max    = new("lazyNumber", xptr = lazyMax(x@xptr), length = 1L),
           min    = new("lazyNumber", xptr = lazyMin(x@xptr), length = 1L),
           range  = new("lazyNumber", xptr = lazyRange(x@xptr), length = 2L),
           prod   = new("lazyNumber", xptr = lazyProd(x@xptr), length = 1L),
           sum    = new("lazyNumber", xptr = lazySum(x@xptr), length = 1L),
           stop(gettextf(
             "Function %s not defined for lazy numbers.", dQuote(.Generic)
           ))
    )
  }
)