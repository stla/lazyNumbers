library(lazyNumbers)

minusonepowk <- function(k) {
  2 * (k %% 2) - 1
}

n <- 2250
dens <- seq(1, n+1, by = 1)
odds <- seq(1, 2*n+1, by = 2)

sum(minusonepowk(dens)/dens * cumsum(1 / odds))

sum(lazyvec(minusonepowk(dens))/lazyvec(dens) * cumsum(1 / lazyvec(odds))) |> as.double()
