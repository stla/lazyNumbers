setMethod(
  "Math", "lazyNumber",
  function(x){
    switch(.Generic,
           cumprod = lazyCumprod(x),
           cumsum  = lazyCumsum(x),
           stop(gettextf(
             "Function %s not defined for lazy numbers.", dQuote(.Generic)
           ))
    )
  }
)