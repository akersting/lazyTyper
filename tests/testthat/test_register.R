context("register")

test_that("custom times can be registered", {
  check_type_expr <- quote({
    if (!is.logical(.XXX.)) {
      markInvalidWError("not logical")
    }
  })

  checkPropertiesFun <- function() {
    args2list()
  }

  registerCustomType("log", checkPropertiesFun, check_type_expr)
  declare(a, "log")
  expect_error(declare(b, "log", foo = bar))
  expect_error(a %<-% .(1))
  expect_silent(a %<-% .(c(TRUE, FALSE)))
})
