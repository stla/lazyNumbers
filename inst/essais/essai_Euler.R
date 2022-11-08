library(lazyNumbers)

n <- 170
x <- seq(1, n, by = 1)
y <- seq(3, 2*n+1, by = 2)

print(2 * (1 + sum(cumprod(x) / cumprod(y))), digits = 20)

lnumber <- 2 * (1 + sum(cumprod(lazynb(x)) / cumprod(lazynb(y))))

print(as.double(lnumber), digits = 20)

print(intervals(lnumber), digits = 20)

print(mean(intervals(lnumber)), digits = 20)