setMethod(
  "Compare",
  signature(e1 = "lazyVector", e2 = "lazyVector"),
  function(e1, e2) {
    n1 <- e1@length
    n2 <- e2@length
    if(n1 != n2 && n1 != 1L && n2 != 1L) {
      stop("Cannot compare these two lazy vectors.")
    }
    lazyCompare(e1@xptr, e2@xptr, .Generic)
  }
)

setMethod(
  "Compare",
  signature(e1 = "lazyMatrix", e2 = "lazyMatrix"),
  function(e1, e2) {
    n1 <- e1@nrow * e1@ncol
    n2 <- e2@nrow * e2@ncol
    if((e1@nrow != e2@nrow || e1@ncol != e2@ncol) && n1 != 1L && n2 != 1L) {
      stop("Cannot compare these two lazy matrices.")
    }
    x <- lazyCompare(lazyFlatten(e1@xptr), lazyFlatten(e2@xptr), .Generic)
    if(n1 != 1L) {
      matrix(x, nrow = e1@nrow, ncol = e1@ncol)
    } else if(n2 != 1L) {
      matrix(x, nrow = e2@nrow, ncol = e2@ncol)
    } else {
      x
    }
  }
)

setMethod(
  "Compare",
  signature(e1 = "lazyVector", e2 = "lazyMatrix"),
  function(e1, e2) {
    e1 <- as.lazyMatrix.lazyVector(e1)
    switch(
      .Generic,
      "==" = e1 == e2,
      "!=" = e1 != e2,
      "<"  = e1 < e2,
      "<=" = e1 <= e2,
      ">"  = e1 > e2,
      ">=" = e1 >= e2
    )
  }
)

setMethod(
  "Compare",
  signature(e1 = "lazyMatrix", e2 = "lazyVector"),
  function(e1, e2) {
    e2 <- as.lazyMatrix.lazyVector(e2)
    switch(
      .Generic,
      "==" = e1 == e2,
      "!=" = e1 != e2,
      "<"  = e1 < e2,
      "<=" = e1 <= e2,
      ">"  = e1 > e2,
      ">=" = e1 >= e2
    )
  }
)
