library(lazyNumbers)

M1 <- lazymat(toeplitz(1:2))
M2 <- lazymat(toeplitz(3:4))
M3 <- lazymat(toeplitz(5:6))

rbind2(M1, M2)

M <- rbind(M2, M3)

M <- rbind(M2, M3)