library(lazyNumbers)

( M <- matrix(rpois(12L, 4), nrow = 4L, ncol = 3L) )

A <- lazymat(M)

as.double(A[, 1])
as.double(A[, 1, drop = FALSE])

as.double(A[1:2, c(1,3)])

as.double(A[2, ])
as.double(A[2, , drop = FALSE])
as.double(A[2])

