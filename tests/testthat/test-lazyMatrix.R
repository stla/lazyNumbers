test_that("lazy matrix from numeric matrix", {
  M <- toeplitz(c(1, 2))
  lm <- lazymat(M)
  expect_identical(as.double(lm), M)
  lm <- lazymat(M, dim = c(3, 5))
  expect_identical(as.double(lm), M)
})

test_that("lazy matrix from numeric vector", {
  nv <- c(1, 2, 3, 4)
  lm <- lazymat(nv)
  expect_identical(dim(lm), c(4L, 1L))
  expect_identical(as.double(lm), as.matrix(nv))
  lm <- lazymat(nv, dim = c(2, 2))
  expect_identical(dim(lm), c(2L, 2L))
})

test_that("lazy matrix with NA, NaN, and Inf values", {
  nm <- matrix(c(1, 2, NA, NaN, Inf, -Inf), nrow = 3L, ncol = 2L)
  lm <- lazymat(nm)
  expect_identical(as.double(lm), nm)
})

test_that("check NA in a lazy matrix", {
  x <- c(1, NA, NaN, Inf, -Inf)
  lm <- lazymat(x)
  expect_identical(
    is.na(lm), 
    as.matrix(c(FALSE, TRUE, FALSE, FALSE, FALSE))
  )
})

test_that("check NaN/Inf in a lazy matrix", {
  x <- c(1, NA, NaN, Inf, -Inf)
  lm <- lazymat(x)
  expect_identical(
    isNaN_or_Inf(lm), 
    as.matrix(c(FALSE, FALSE, TRUE, TRUE, TRUE))
  )
})

test_that("sum of a lazy matrix", {
  lm <- lazymat(c(1, 2, NA, NA))
  expect_true(is.na(as.double(sum(lm))))
  expect_true(as.double(sum(lm, na.rm = TRUE)) == 3)
})

test_that("transpose of a lazy matrix", {
  nm <- matrix(c(1, 2, NA, NaN, Inf, -Inf), nrow = 3L, ncol = 2L)
  lm <- t(lazymat(nm))
  expect_identical(as.double(lm), t(nm))
})

test_that("matricial product of lazy matrices", {
  M1 <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3L, ncol = 2L)
  M2 <- toeplitz(c(5, 6))
  lm <- lazymat(M1) %*% lazymat(M2)
  expect_identical(as.double(lm), M1 %*% M2)
})

test_that("rbind lazy matrices", {
  nm1 <- matrix(c(1, 2, NA, NaN, Inf, -Inf), nrow = 3L, ncol = 2L)
  nm2 <- toeplitz(c(1, 2))
  lm1 <- lazymat(nm1)
  lm2 <- lazymat(nm2)
  expect_identical(as.double(rbind(lm1, lm2)), rbind(nm1, nm2))
})

test_that("cbind lazy matrices", {
  nm1 <- matrix(c(1, 2, NA, NaN, Inf, -Inf), nrow = 3L, ncol = 2L)
  nm2 <- toeplitz(c(1, 2, 3))
  lm1 <- lazymat(nm1)
  lm2 <- lazymat(nm2)
  expect_identical(as.double(cbind(lm1, lm2)), cbind(nm1, nm2))
})

test_that("lazy matrix to lazy vector", {
  nm <- matrix(c(1, 2, NA, NaN, Inf, -Inf), nrow = 3L, ncol = 2L)
  lm <- lazymat(nm)
  expect_identical(as.double(lazyvec(lm)), c(nm))
  expect_identical(as.double(c(lm)), c(nm))
})

test_that("extract from a lazy matrix", {
  nm <- matrix(c(1, 2, NA, NaN, Inf, -Inf, 8, 9, NA), nrow = 3L, ncol = 3L)
  lm <- lazymat(nm)[2:3, 2:3]
  expect_identical(as.double(lm), nm[2:3, 2:3])
})

test_that("extract from a lazy matrix with negative indices", {
  nm <- matrix(c(1, 2, NA, NaN, Inf, -Inf, 8, 9, NA), nrow = 3L, ncol = 3L)
  lm <- lazymat(nm)[2:3, -2]
  expect_identical(as.double(lm), nm[2:3, -2])
})

test_that("extract diagonal of a lazy matrix", {
  nm <- matrix(c(1, 2, NA, NaN, Inf, -Inf, 8, 9, NA), nrow = 3L, ncol = 3L)
  lv <- diag(lazymat(nm))
  expect_identical(as.double(lv), diag(nm))
})

test_that("replace diagonal of a lazy matrix", {
  nm <- matrix(c(1, 2, NA, NaN, Inf, -Inf, 8, 9, NA), nrow = 3L, ncol = 3L)
  lm <- lazymat(nm)
  x <- c(NA, 2, -Inf)
  diag(nm) <- x
  diag(lm) <- lazyvec(x)
  expect_identical(as.double(lm), nm)
})
