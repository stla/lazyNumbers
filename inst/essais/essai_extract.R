library(lazyNumbers)

( M <- matrix(rpois(12L, 4), nrow = 4L, ncol = 3L) )

A <- lazymat(M)

as.double(A[1:2, c(1,3)])