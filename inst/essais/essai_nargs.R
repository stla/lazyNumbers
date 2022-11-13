f <- function(i, j, drop) {
  n_args <- nargs()
  if(!missing(drop)) {
    n_args <- n_args - 1L
  }
  n_args
}

f(1)
f(1,)
f(1, ,)
# => si , alors >= 2