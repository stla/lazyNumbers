library(lazyNumbers)

M1 <- lazymat(toeplitz(1:2))
M2 <- lazymat(toeplitz(3:4))
M3 <- lazyvec(5:6)
M4 <- 5:6
M5 <- toeplitz(c(8,9))

as.double(c(M5, M4))

as.double(c(M3, M5))

as.double(c(M1, M2))

as.double(c(M2, M3))

as.double(c(M3, M1))

as.double(c(M1, M4))

as.double(c(M3, M4))