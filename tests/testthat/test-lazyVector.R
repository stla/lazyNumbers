test_that("lazy vector from numeric vector", {
  nv <- c(1, 2, 3)
  lv <- lazyvec(nv)
  expect_length(lv, 3L)
})

test_that("lazy vector with NA, NaN, and Inf values", {
  nv <- c(1, NA, NaN, Inf, -Inf)
  lv <- lazyvec(nv)
  expect_length(lv, 5L)
  x <- as.double(lv)
  expect_identical(x, nv)
})

test_that("lazy vector from logical vector", {
  x <- c(TRUE, FALSE, NA)
  lv <- lazyvec(x)
  expect_identical(as.double(lv), as.double(x))
})

test_that("lazy vector from numeric matrix", {
  M <- toeplitz(c(1, 2))
  lv <- lazyvec(M)
  expect_identical(as.double(lv), c(M))
})

test_that("na.omit for a lazy vector", {
  lv <- na.omit(lazyvec(c(1, NA, NA)))
  expect_length(lv, 1L)
})

test_that("check NA in a lazy vector", {
  lv <- lazyvec(c(1, NA, NaN, Inf, -Inf))
  expect_identical(is.na(lv), c(FALSE, TRUE, FALSE, FALSE, FALSE))
  expect_true(anyNA(lv))
})

test_that("check NaN/Inf in a lazy vector", {
  lv <- lazyvec(c(1, NA, NaN, Inf, -Inf))
  expect_identical(isNaN_or_Inf(lv), c(FALSE, FALSE, TRUE, TRUE, TRUE))
})

test_that("sum of a lazy vector", {
  lv <- lazyvec(c(1, 2, NA))
  expect_true(is.na(as.double(sum(lv))))
  expect_true(as.double(sum(lv, na.rm = TRUE)) == 3)
})

test_that("replace in a lazy vector", {
  x1 <- c(1, 2, NA, NaN, Inf)
  x2 <- c(9, NaN, 4, NA)
  lv1 <- lazyvec(x1)
  lv2 <- lazyvec(x2)
  lv1[1:4] <- lv2
  x1[1:4] <- x2
  expect_identical(as.double(lv1), x1)
})

test_that("extract from a lazy vector", {
  x <- c(1, 2, NA, NaN, Inf)
  lv <- lazyvec(x)
  expect_identical(as.double(lv[2:5]), x[2:5])
})

test_that("extract from a lazy vector with negative indices", {
  x <- c(1, 2, NA, NaN, Inf)
  lv <- lazyvec(x)
  expect_identical(as.double(lv[-2]), x[-2])
})
