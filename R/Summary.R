setMethod(
  "Summary", "lazyNumber",
  function(x) {
    switch(.Generic,
           prod = new("lazyNumber", xptr = lazyProd(x@xptr), length = 1L),
           sum  = new("lazyNumber", xptr = lazySum(x@xptr), length = 1L),
           stop(gettextf(
             "Function %s not defined for lazy numbers.", dQuote(.Generic)
           ))
    )
  }
)