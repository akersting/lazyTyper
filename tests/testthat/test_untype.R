context("untype")

test_that("untype issues expected warnings", {
  expect_warning(untype(a))
  a <- 1
  expect_warning(untype(a))
  declare("b", "numeric", .character = TRUE)
  expect_warning(untype(a))
})

test_that("untype removes type information", {
  declare(a, "numeric")
  expect_true(is.typed(a))
  untype(a)
  expect_false(is.typed(a))
})
