context("cast")

test_that("cast fails for non-existing object", {
  expect_error(cast(a, "numeric"), "No such object.")
})

test_that("cast fails for already typed object", {
  a <- 1
  cast(a, "numeric")
  expect_error(cast(a, "numeric"), "already declared/typed")
})

test_that("cast fails for misspecified properties", {
  a <- 1
  expect_error(cast(a, "numeric", a = 1))
  b <- 2
  expect_error(cast(b, "numeric", a = 1, 2))
  d <- 3
  expect_error(cast(d, "numeric", a = 1, a = 2))
})

test_that("cast works with empty properties", {
  a <- 1
  cast(a, "numeric")
  env <- environment()
  lazyTyper_env <- get(".lazyTyper_env", envir = env, inherits = FALSE)
  lazyTyper_obj <- get("a", envir = lazyTyper_env, inherits = FALSE)
  expect_equal(lazyTyper_obj$type, "numeric")
  expect_equal(lazyTyper_obj$properties,
               structure(list(), dynamic_properties = FALSE))
})

test_that("cast works with additional properties", {
  a <- 1
  cast(a, "numeric", length = 1)
  env <- environment()
  lazyTyper_env <- get(".lazyTyper_env", envir = env, inherits = FALSE)
  lazyTyper_obj <- get("a", envir = lazyTyper_env, inherits = FALSE)
  expect_equal(lazyTyper_obj$type, "numeric")
  expect_equal(lazyTyper_obj$properties,
               structure(list(length = enquote(1)), dynamic_properties = FALSE))
})

test_that("cast fails for wrong type", {
  a <- "a"
  expect_error(cast(a, "numeric"))
})

test_that("cast fails if lengh(x) != 1", {
  expect_error(cast(type = "numeric"))
})
