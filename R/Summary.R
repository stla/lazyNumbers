setMethod(
  "Summary", "lazyVector",
  function(x, ..., na.rm = FALSE) {
    switch(.Generic,
           max    = new("lazyVector", xptr = lazyMax(x@xptr), length = 1L),
           min    = new("lazyVector", xptr = lazyMin(x@xptr), length = 1L),
           range  = new("lazyVector", xptr = lazyRange(x@xptr), length = 2L),
           prod   = new("lazyVector", xptr = lazyProd(x@xptr), length = 1L),
           sum    = new("lazyVector", xptr = lazySum(x@xptr), length = 1L),
           stop(gettextf(
             "Function %s not defined for lazy vectors.", dQuote(.Generic)
           ))
    )
  }
)