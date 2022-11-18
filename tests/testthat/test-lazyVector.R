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

test_that("check NaN/Inf in a lazy vector", {
  lv <- lazyvec(c(1, NA, NaN, Inf, -Inf))
  expect_identical(isNaN_or_Inf(lv), c(FALSE, FALSE, TRUE, TRUE, TRUE))
})
