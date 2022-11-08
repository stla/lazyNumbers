library(lazyNumbers)

n <- 170
x <- seq(1, n, by = 1)
y <- seq(3, 2*n+1, by = 2)

sum(cumprod(x) / cumprod(y))

as.double(sum(cumprod(lazynb(x)) / cumprod(lazynb(y))))

print(intervals(sum(cumprod(lazynb(x)) / cumprod(lazynb(y)))), digits = 20)