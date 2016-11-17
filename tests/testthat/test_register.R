context("register")

test_that("custom types can be registered", {
  checkTypeFun <- function(x) {
    if (!is.logical(x)) {
      conditionR::signal(
        conditionR::stackError("not logical",
                                    base_class = "lazyTyperError")
      )
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

test_that("custom aliases can be registered", {
  registerCustomAlias("ID", "character", fixed = alist(
    set = ,
    allow_NA = FALSE,
    pattern = "[0-9]{8}"
  ))
})
