context("get")

test_that("get fails for untyped variables", {
  a <- 1
  expect_error(g(a))
})

test_that("get returns from the correct environment", {
  a <- 1
  cast("a", "numeric", .character = TRUE)

  env <- new.env()
  assign("a", 2, envir = env)
  cast(a, "numeric", env = env)

  expect_equal(g(a), 1)
  expect_equal(g("a", env = env, .character = TRUE), 2)
})

test_that("get works with inherits", {
  a <- 1
  cast(a, "numeric")
  expect_equal(g(a, inherits = TRUE), 1)

  f1 <- function() g(a, inherits = TRUE)
  f2 <- function() g(a, env = parent.frame())
  f3 <- function() g(a)

  expect_equal(f1(), 1)
  expect_equal(f2(), 1)
  expect_error(f3())
})
