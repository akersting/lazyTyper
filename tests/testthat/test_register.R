context("register")

test_that("custom times can be registered", {
  check_type_expr <- quote({
    if (!is.logical(x)) {
      markInvalidWError("not logical")
    }
  })

  checkPropertiesFun <- function(x) {
    if (length(x) != 0) {
      stop("did not expect any properties")
    }
  }

  registerCustomType("log", checkPropertiesFun, check_type_expr)
  declare(a, "log")
  expect_error(declare(b, "log", foo = bar))
  expect_error(a %<-% 1)
})
