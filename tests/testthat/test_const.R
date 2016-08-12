context("const")

test_that("const works as expected", {
  a <- 1
  const(a)
  a <- 2
  expect_error(g(a))
})
