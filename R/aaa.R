.onLoad <- function(libname, pkgname) {
  assign(
    "NA_lazy_", 
    new("lazyVector", xptr = lazyNA(), length = 1L),
    envir = parent.env(environment())
  )
  invisible()
}