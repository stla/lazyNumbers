library(lazyNumbers)
set.seed(666L)
M <- lazymat(matrix(rpois(9L, lambda = 4), nrow = 3L, ncol = 3L))
invM <- lazyInv(M)
I3 <- M %*% invM
as.double(I3) == diag(3)

