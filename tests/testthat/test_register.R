context("register")

test_that("custom times can be registered", {
  checkTypeFun <- function(x) {
    if (!is.logical(x)) {
      markInvalidWError("not logical")
    }
  }

  checkPropertiesFun <- function() {
    args2list()
  }

  registerCustomType("log", checkPropertiesFun, checkTypeFun)
  declare(a, "log")
  expect_error(declare(b, "log", foo = bar))
  expect_error(a %<-% .(1))
  expect_silent(a %<-% .(c(TRUE, FALSE)))
})
