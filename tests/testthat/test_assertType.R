context("assertType")

test_that("assertType works as expected", {
  expect_equal(assertType(1:10, "numeric"), 1:10)
  expect_error(assertType(1:10, "numeric", length = 3))
  expect_error(assertType(quote(1:10), "numeric"))
})
