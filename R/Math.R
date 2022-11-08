setMethod(
  "Math", "lazyNumber",
  function(x){
    switch(
      .Generic,
      cumprod = new("lazyNumber", xptr = lazyCumprod(x@xptr), length = x@length),
      cumsum  = new("lazyNumber", xptr = lazyCumsum(x@xptr), length = x@length),
      stop(gettextf(
        "Function %s not defined for lazy numbers.", dQuote(.Generic)
      ))
    )
  }
)