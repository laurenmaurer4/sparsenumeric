## test_sparse_numeric.R

library(testthat)
library(sparsenumeric)

test_that("check validity method exists", {
  expect_false({
    validity_method <- getValidity(getClassDef("sparse_numeric"))
    is.null(validity_method)
  })
})

test_that("check validity method", {
  expect_true({
    x <- new("sparse_numeric",
             value = c(1, 2, 3, 1),
             pos = c(1L, 2L, 3L, 5L),
             length = 5L)
    validObject(x)
  })
})

test_that("check validity method 2", {
  expect_error({
    x <- new("sparse_numeric",
             value = c(1, 2, 3, 1),
             pos = c(1L, 2L, 3L, 5L),
             length = 5L)
    x@length <- 2L
    validObject(x)
  })
})

test_that("check coercion return class", {
  expect_s4_class({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
  }, "sparse_numeric")
})

test_that("check for show method", {
  expect_no_error({
    getMethod("show", "sparse_numeric")
  })
})

test_that("check for plot method", {
  expect_no_error({
    getMethod("plot", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("check for + method", {
  expect_no_error({
    getMethod("+", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("check for - method", {
  expect_no_error({
    getMethod("-", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("check for * method", {
  expect_no_error({
    getMethod("*", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("sparse add generic", expect_true(isGeneric("sparse_add")))
test_that("sparse mult generic", expect_true(isGeneric("sparse_mult")))
test_that("sparse sub generic", expect_true(isGeneric("sparse_sub")))
test_that("sparse crossprod generic", expect_true(isGeneric("sparse_crossprod")))

test_that("sparse add formals", {
  expect_true(length(formals(sparse_add)) >= 2L)
})

test_that("sparse mult formals", {
  expect_true(length(formals(sparse_mult)) >= 2L)
})

test_that("sparse sub formals", {
  expect_true(length(formals(sparse_sub)) >= 2L)
})

test_that("sparse crossprod formals", {
  expect_true(length(formals(sparse_crossprod)) >= 2L)
})

test_that("check returned class for add", {
  expect_s4_class({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
    sparse_add(x, y)
  }, "sparse_numeric")
})

test_that("sparse_add", {
  result <- as(c(1, 1, 0, 1, 6), "sparse_numeric")
  expect_equal({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
    sparse_add(x, y)
  }, result)
})

test_that("sparse add dense", {
  result <- as(c(2, 4, 6, 10, 12), "sparse_numeric")
  expect_equal({
    x <- as(c(1, 3, 4, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 2, 9, 10), "sparse_numeric")
    sparse_add(x, y)
  }, result)
})

test_that("all zero wrong length", {
  expect_error({
    x <- as(rep(0, 10), "sparse_numeric")
    y <- as(rep(0, 9), "sparse_numeric")
    sparse_add(x, y)
  })
})

# ----------------------------------------#
test_that("sparse_sub returns correct result with different numbers", {
  x <- as(c(5, 0, 3, 2), "sparse_numeric")
  y <- as(c(1, 4, 0, 3), "sparse_numeric")
  result <- as(c(4, -4, 3, -1), "sparse_numeric")
  expect_equal(sparse_sub(x, y), result)
})

test_that("sparse_mult returns correct result with new numbers", {
  x <- as(c(2, 0, 5, 1), "sparse_numeric")
  y <- as(c(3, 4, 0, 2), "sparse_numeric")
  result <- as(c(6, 0, 0, 2), "sparse_numeric")
  expect_equal(sparse_mult(x, y), result)
})

test_that("sparse_crossprod computes correct scalar with new numbers", {
  x <- as(c(2, 1, 4), "sparse_numeric")
  y <- as(c(0, 3, 5), "sparse_numeric")
  expect_equal(sparse_crossprod(x, y), sum(c(2,1,4) * c(0,3,5)))
})

test_that("mean method calculates correctly for different values", {
  x <- as(c(0, 4, 2), "sparse_numeric")
  expect_equal(mean(x), (0+4+2)/3)
})

test_that("norm method works for different vector", {
  x <- as(c(6, 8, 0), "sparse_numeric")
  expect_equal(norm(x, type="2"), 10)
})

test_that("standardize method works with new vector", {
  x <- as(c(2, 4, 6), "sparse_numeric")
  s <- standardize(x)
  expect_s4_class(s, "sparse_numeric")
  expect_equal(mean(as(s, "numeric")), 0)
})

test_that("operators return correct values and classes with new numbers", {
  x <- as(c(3,0,2), "sparse_numeric")
  y <- as(c(1,1,4), "sparse_numeric")

  expect_s4_class(x + y, "sparse_numeric")
  expect_equal(as(x + y, "numeric"), c(4,1,6))

  expect_s4_class(x - y, "sparse_numeric")
  expect_equal(as(x - y, "numeric"), c(2,-1,-2))

  expect_s4_class(x * y, "sparse_numeric")
  expect_equal(as(x * y, "numeric"), c(3,0,8))
})

test_that("validity detects zero values in new case", {
  expect_error(
    new("sparse_numeric", value = c(0, 5), pos = 1:2, length = 2L),
    "Some values are equal to zero"
  )
})

test_that("validity detects duplicated positions with new numbers", {
  expect_error(
    new("sparse_numeric", value = c(5,3), pos = c(2L,2L), length = 2L),
    "pos must not contain duplicates"
  )
})

test_that("validity detects positions out of range with new example", {
  expect_error(
    new("sparse_numeric", value = c(2,4), pos = c(1L,3L), length = 2L),
    "Some positions are outside the valid range"
  )
})

test_that("validity detects zero length vectors", {
  expect_error(
    new("sparse_numeric", value = numeric(0), pos = integer(0), length = 0L),
    "length must be >0"
  )
})

test_that("sparse_add returns empty sparse_numeric for all zeros with new numbers", {
  x <- as(c(2, -2), "sparse_numeric")
  y <- as(c(-2, 2), "sparse_numeric")
  res <- sparse_add(x, y)
  expect_s4_class(res, "sparse_numeric")
  expect_equal(length(res@value), 0)
})

test_that("show works for empty sparse_numeric", {
  x <- new("sparse_numeric", value=numeric(0), pos=integer(0), length=3L)
  expect_output(show(x))
})

test_that("summary works for single-element sparse_numeric with different values", {
  x <- as(c(0,7,0), "sparse_numeric")
  expect_output(summary(x))
})

test_that("plot works for empty sparse_numeric vectors", {
  x <- new("sparse_numeric", value=numeric(0), pos=integer(0), length=4L)
  y <- new("sparse_numeric", value=numeric(0), pos=integer(0), length=4L)
  expect_silent(plot(x, y))
})

test_that("standardize works when all values identical but different numbers", {
  x <- as(c(3,3,3), "sparse_numeric")
  s <- standardize(x)
  expect_s4_class(s, "sparse_numeric")
  expect_equal(mean(as(s, "numeric")), 0)
})

test_that("addition with empty sparse_numeric returns empty", {
  x <- new("sparse_numeric", value=numeric(0), pos=integer(0), length=5L)
  y <- new("sparse_numeric", value=numeric(0), pos=integer(0), length=5L)
  res <- sparse_add(x, y)
  expect_equal(length(res@value), 0)
})

test_that("crossprod with empty sparse_numeric returns 0", {
  x <- new("sparse_numeric", value=numeric(0), pos=integer(0), length=5L)
  y <- new("sparse_numeric", value=numeric(0), pos=integer(0), length=5L)
  expect_equal(sparse_crossprod(x, y), 0)
})

test_that("standardize single nonzero value", {
  x <- as(c(0,9,0), "sparse_numeric")
  s <- standardize(x)
  expect_s4_class(s, "sparse_numeric")
})

test_that("plot with partially overlapping positions using new numbers", {
  x <- as(c(3,0,5), "sparse_numeric")
  y <- as(c(0,2,0), "sparse_numeric")
  expect_silent(plot(x, y))
})

test_that("sparse_add fails on different lengths with new vectors", {
  x <- as(1:4, "sparse_numeric")
  y <- as(1:3, "sparse_numeric")
  expect_error(sparse_add(x, y), "Sparse vectors must have the same length")
})

test_that("sparse_mult fails on different lengths with new vectors", {
  x <- as(1:4, "sparse_numeric")
  y <- as(1:2, "sparse_numeric")
  expect_error(sparse_mult(x, y), "Sparse vectors must have the same length")
})

test_that("sparse_sub fails on different lengths with new vectors", {
  x <- as(1:5, "sparse_numeric")
  y <- as(1:4, "sparse_numeric")
  expect_error(sparse_sub(x, y), "Sparse vectors must have the same length")
})

test_that("sparse_crossprod fails on different lengths with new vectors", {
  x <- as(1:6, "sparse_numeric")
  y <- as(1:3, "sparse_numeric")
  expect_error(sparse_crossprod(x, y), "Sparse vectors must have the same length")
})

test_that("plot fails on different lengths", {
  x <- as(1:3, "sparse_numeric")
  y <- as(1:2, "sparse_numeric")
  expect_error(plot(x, y), "Vectors must have the same length")
})
