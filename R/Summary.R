setMethod(
  "Summary", c("lazyVector", "logical"),
  function(x, ..., na.rm = FALSE) {
    switch(.Generic,
           max    = if(x@length == 0L) {
             lazyvec(max(numeric(0L)))
           } else {
             warning(
               "The result is possibly wrong if there are some NaN or Inf values."
             )
             new("lazyVector", xptr = lazyMax(x@xptr, na.rm), length = 1L)
           },
           min    = if(x@length == 0L) {
             lazyvec(min(numeric(0L)))
           } else {
             warning(
               "The result is possibly wrong if there are some NaN or Inf values."
             )
             new("lazyVector", xptr = lazyMin(x@xptr, na.rm), length = 1L)
           },
           range  = if(x@length == 0L) {
             lazyvec(range(numeric(0L)))
           } else {
             warning(
               "The result is possibly wrong if there are some NaN or Inf values."
             )
             new("lazyVector", xptr = lazyRange(x@xptr, na.rm), length = 2L)
           },
           prod   = new(
             "lazyVector", xptr = lazyProd(x@xptr, na.rm), length = 1L
            ),
           sum    = new(
             "lazyVector", xptr = lazySum(x@xptr, na.rm), length = 1L
            ),
           stop(gettextf(
             "Function %s not defined for lazy vectors.", dQuote(.Generic)
           ))
    )
  }
)

setMethod(
  "Summary", c("lazyMatrix", "logical"),
  function(x, ..., na.rm = FALSE) {
    switch(.Generic,
           max    = if(x@nrow == 0L || x@ncol == 0L) {
             lazyvec(max(numeric(0L)))
           } else {
             warning(
               "The result is possibly wrong if there are some NaN or Inf values."
             )
             new("lazyVector", xptr = MlazyMax(x@xptr, na.rm), length = 1L)
           },
           min    = if(x@nrow == 0L || x@ncol == 0L) {
             lazyvec(min(numeric(0L)))
           } else {
             warning(
               "The result is possibly wrong if there are some NaN or Inf values."
             )
             new("lazyVector", xptr = MlazyMin(x@xptr, na.rm), length = 1L)
           },
           range  = if(x@nrow == 0L || x@ncol == 0L) {
             lazyvec(range(numeric(0L)))
           } else {
             warning(
               "The result is possibly wrong if there are some NaN or Inf values."
             )
             new("lazyVector", xptr = MlazyRange(x@xptr, na.rm), length = 2L)
           },
           prod   = new(
             "lazyVector", xptr = MlazyProd(x@xptr, na.rm), length = 1L
            ),
           sum    = new(
             "lazyVector", xptr = MlazySum(x@xptr, na.rm), length = 1L
            ),
           stop(gettextf(
             "Function %s not defined for lazy matrices.", dQuote(.Generic)
           ))
    )
  }
)