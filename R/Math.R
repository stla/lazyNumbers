setMethod(
  "Math", "lazyVector",
  function(x) {
    switch(
      .Generic,
      cumprod = new("lazyVector", xptr = lazyCumprod(x@xptr), length = x@length),
      cumsum  = new("lazyVector", xptr = lazyCumsum(x@xptr), length = x@length),
      stop(gettextf(
        "Function %s not defined for lazy vectors.", dQuote(.Generic)
      ))
    )
  }
)