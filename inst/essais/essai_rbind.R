library(lazyNumbers)

M1 <- lazymat(toeplitz(1:2))
M2 <- lazymat(toeplitz(3:4))
M3 <- lazyvec(5:6)


as.double(rbind(M1, M2))

as.double(rbind(M2, M3))