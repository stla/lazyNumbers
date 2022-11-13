setMethod(
  "Compare",
  signature(e1 = "lazyVector", e2 = "lazyVector"),
  function(e1, e2) {
    n1 <- e1@length
    n2 <- e2@length
    if(n1 != n2 && n1 != 1 && n2 != 1) {
      stop("Cannot compare these two lazy vectors.")
    }
    lazyCompare(e1@xptr, e2@xptr, .Generic)
  }
)