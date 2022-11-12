setMethod(
  "Math", "lazyVector",
  function(x) {
    switch(
      .Generic,
      abs     = new("lazyVector", xptr = lazyAbs(x@xptr), length = x@length),
      cumprod = new("lazyVector", xptr = lazyCumprod(x@xptr), length = x@length),
      cumsum  = new("lazyVector", xptr = lazyCumsum(x@xptr), length = x@length),
      stop(gettextf(
        "Function %s not defined for lazy vectors.", dQuote(.Generic)
      ))
    )
  }
)

setMethod(
  "Math", "lazyMatrix",
  function(x) {
    switch(
      .Generic,
      abs = new("lazyMatrix", xptr = MlazyAbs(x@xptr), nrow = x@nrow, ncol = x@ncol),
      stop(gettextf(
        "Function %s not defined for lazy matrices.", dQuote(.Generic)
      ))
    )
  }
)