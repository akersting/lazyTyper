context("declare")

test_that("declare fails when expected", {
  a <- 1
  expect_error(declare(a, "numeric"))
  declare(b, "numeric")
  expect_error(declare(b, "numeric"))
})

test_that("declare accepts a character vector", {
  declare(c("a", "b"), "numeric", .character = TRUE)
  expect_true(is.typed(a))
  expect_true(is.typed(b))
  expect_error(declare("c", .character = TRUE))
  expect_error(declare(c("c", "d"), .character = TRUE))
})
