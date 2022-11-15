library(lazyNumbers)

n <- 400
x <- seq(1, n, by = 1)
y <- seq(3, 2*n+1, by = 2)

lnumber <- 2 * (1 + sum(cumprod(lazynb(x) / lazynb(y))))

print(as.double(lnumber), digits = 20)




print(2 * (1 + sum(cumprod(x) / cumprod(y))), digits = 20)

lnumber <- 2 * (1 + sum(cumprod(lazynb(x)) / cumprod(lazynb(y))))

print(as.double(lnumber), digits = 20)

print(intervals(lnumber), digits = 20)

print(mean(intervals(lnumber)), digits = 20)



library(lazyNumbers)
n <- 500
p <- seq(1, n, by = 1)
q <- seq(3, 2*n + 1, by = 2)
# fast, because the operations are not evaluated:
x1 <- 2 * (1 + sum(cumprod(lazynb(p) / lazynb(q))))
x2 <- 2 * (1 + sum(cumprod(lazynb(p) / lazynb(q))))
x3 <- 2 * (1 + sum(cumprod(lazynb(p) / lazynb(q))))
# slow, because this evaluates the operations:
lazyResolve(x1)
# fast, because `x1` is resolved now:
as.double(x1)
# slow, because `x2` must be resolved:
as.double(x2)
# fast, because the call to `as.double` has resolved `x2`
as.double(x2)
# slow, because `x3` is not resolved:
x1 == x3
# fast, because `x3` has been resolved by the equality test:
as.double(x3)