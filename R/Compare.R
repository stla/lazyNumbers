setMethod(
  "Compare",
  signature(e1 = "lazyVector", e2 = "lazyVector"),
  function(e1, e2) {
    # checker lengths
    lazyCompare(e1@xptr, e2@xptr, .Generic)
  }
)