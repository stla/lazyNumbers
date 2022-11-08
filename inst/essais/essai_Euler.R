library(lazyNumbers)

n <- 170
x <- seq(1, n, by = 1)
y <- seq(3, 2*n+1, by = 2)

2 * (1 + sum(cumprod(x) / cumprod(y)))
lnumber <- 2 * (1 + sum(cumprod(lazynb(x)) / cumprod(lazynb(y))))

as.double(lnumber)

print(intervals(lnumber), digits = 20)