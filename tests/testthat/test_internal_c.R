context("internal C")

test_that("eval2Reference works with sys.nframe", {
  eval2Reference(sys.nframe(), "nframes")
  expect_identical(nframes, sys.nframe())

  env <- new.env()
  eval2Reference(sys.nframe(), "nframes", eval_env = env)
  expect_identical(nframes, sys.nframe())
})

test_that("evalPromiseCode works with non-promises", {
  g <- function(x) evalPromiseCode("x")
  f <- function() g(1)
  f <- compiler::cmpfun(f)
  expect_identical(f(), 1)

  h <- function() {x <- quote(1 + 1); evalPromiseCode("x")}
  expect_identical(h(), quote(1 + 1))
})

test_that("simpleGet can return the empty name", {
  x <- quote(expr = )
  expect_identical(simpleGet("x"), quote(expr = ))
})

test_that("simpleGet returns the value of a promise", {
  f <- function(x) simpleGet("x")
  expect_identical(f(1:10), 1:10)
})
