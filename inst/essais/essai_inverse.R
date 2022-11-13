library(lazyNumbers)
set.seed(666L)
M <- lazymat(matrix(rnorm(9L, 100000), nrow = 3L, ncol = 3L))
invM <- lazyInv(M)
I3 <- M %*% invM
as.double(I3) == diag(3)


M <- rbind(
  cbind(M, matrix(0, 3, 2)),
  cbind(matrix(0, 2, 3), diag(2))
)

I5 <- M %*% lazyInv(M)
as.double(I5) == diag(5)

############################
library(lazyNumbers)
set.seed(666L)
M <- matrix(rnorm(9L), nrow = 3L, ncol = 3L)
Ml <- lazymat(M)
d <- lazyDet(Ml)

lvs <- vector("list", 3L)
for(i in 1:3) {
  a <- lazyDet(lazymat(M[-i, -1]))
  b <- lazyDet(lazymat(M[-i, -2]))
  c <- lazyDet(lazymat(M[-i, -3]))
  lvs[[i]] <- if(i %% 2 == 1) c(a, -b, c) / d else c(-a, b, -c) / d
}
invM <- do.call(cbind, lvs)

as.double(Ml %*% invM) == diag(3)
as.double(invM %*% Ml) == diag(3)
