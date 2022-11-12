library(lazyNumbers)
library(Rmpfr)

u_lazy <- function(n) {
  if(n == 1) {
    return(1 / lazynb(7))
  }
  8 * u_lazy(n-1) - 1
}
as.double(u_lazy(100))

u_mpfr <- function(n) {
  if(n == 1) {
    return(1 / mpfr(7, prec = 256L))
  }
  8 * u_mpfr(n-1) - 1
}
asNumeric(u_mpfr(85))


library(microbenchmark)
microbenchmark(
  lazy = u_lazy(200),
  mpfr = u_mpfr(200),
  times = 20L
)