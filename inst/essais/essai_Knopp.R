library(lazyNumbers)

minusonepowk <- function(k) {
  2 * (k %% 2) - 1
}

n <- 500
dens <- seq(1, n+1, by = 1)
odds <- seq(1, 2*n+1, by = 2)

sum(minusonepowk(dens)/dens * cumsum(1 / odds))

f1 <- function() {
  sum(lazyvec(minusonepowk(dens))/lazyvec(dens) * cumsum(1 / lazyvec(odds))) |> as.double()
}

f2 <- function() {
  sum(lazyvec(minusonepowk(dens))/lazyvec(dens) * lazyResolve(cumsum(1 / lazyvec(odds)))) |> as.double()
}

library(microbenchmark)
microbenchmark(
  f1 = f1(),
  f2 = f2(),
  times = 2
)
