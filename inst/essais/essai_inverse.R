library(lazyNumbers)
set.seed(666L)
M <- lazymat(matrix(rnorm(9L), nrow = 3L, ncol = 3L))
invM <- lazyInv(M)
I3 <- M %*% invM
as.double(I3) == diag(3)


M <- rbind(
  cbind(M, matrix(0, 3, 2)),
  cbind(matrix(0, 2, 3), diag(2))
)

I5 <- M %*% lazyInv(M)
as.double(I5) == diag(5)

