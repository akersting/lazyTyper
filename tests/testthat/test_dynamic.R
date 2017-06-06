context("dynamic typing")

test_that("properties can be language objects", {
  x <- 1:10
  declare(var, "numeric", length = quote(length(x)))
  x <- 1:5
  expect_error({var %<-% .(1:10)})
  expect_silent({var %<-% .(1:5)})
})

test_that("properties are evaluated only once", {
  declare(x, "numeric", length = quote(quote(1 + 0)))
  expect_error({x %<-% .(123)})
})

test_that("other features work with dynamic properties", {
  assertType(1 + 1, "numeric", length = quote(1 + 0))

  var <- 123
  expect_silent(cast(var, "numeric", set = quote(100 + 23)))
  expect_true(is.valid(var))
  expect_silent(g(var))

  var <- 111
  expect_false(is.valid(var))
  expect_error(g(var))
})

