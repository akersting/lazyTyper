context("assign")

test_that("assign fails for invalid RHS", {
  declare(a, "numeric")
  expect_error(a %<-% 1)
  expect_error(a %<-% .())
  expect_error(a %<-% .(1, 1))

  expect_error(a %<-s% 1)
  expect_error(a %<-s% .())
  expect_error(a %<-s% .(1, 1))
})

for (warning2error in c(FALSE, TRUE)) {
  .lazyTyper_warning2error <- warning2error

  test_that("assign fails for undeclared/untyped variable", {
    expect_error(a %<-% .(1))
    expect_error(a %<-s% .(1))
    a <- 1
    expect_error(a %<-% .(1))
    expect_error(a %<-s% .(1))
  })

  test_that("assign works as expected", {
    declare(a, "numeric")
    expect_error(a %<-s% .("a"))
    expect_false(exists("a", inherits = FALSE))
    expect_silent(a %<-% .(1))
    expect_equal(a, 1)
    expect_error(a %<-s% .("a"))
    expect_equal(a, 1)
    expect_error(a %<-% .("a"))
    expect_equal(a, "a")
    expect_silent(a %<-s% .(2))
    expect_equal(a, 2)
    expect_error(a %<-% .(quote(1:10)))
  })

  test_that("assign works as expected for complex expressions", {
    declare(a, "numeric")
    a %<-% .(c(1, 2, 3))
    expect_equal(a, c(1, 2, 3))
    a[2] %<-% .(9)
    expect_equal(a, c(1, 9, 3))
    names(a) %<-% .(letters[1:3])
    expect_equal(names(a), letters[1:3])
    names(a)[2] %<-% .("z")
    expect_equal(names(a), c("a", "z", "c"))
    expect_error(class(a) %<-% .("character"))
  })

  test_that("assign returns the RHS", {
    declare(a, "numeric")
    expect_equal(a %<-% .(c(1,2,3)), c(1,2,3))
    expect_equal(a[2] %<-% .(0), 0)
    expect_equal(a %<-s% .(c(4,5,6)), c(4,5,6))
    expect_equal(a[2:3] %<-s% .(c(8,9)), c(8,9))
  })

  test_that("assign works correctly if RHS involves sys.nframe()", {
    declare(a, "numeric")
    expect_equal(a %<-% .(sys.nframe() + 1), sys.nframe() + 1)
  })
}
rm(.lazyTyper_warning2error)

test_that("assign fails on warning", {
  .lazyTyper_warning2error <- TRUE
  declare(a, "numeric")
  a <- NULL
  expect_error(a[1:5] %<-% .(1:3))

  .lazyTyper_hard_bounds <- TRUE
  declare(b, "numeric")
  b <- NULL
  expect_error(b[1] %<-% .(1))
})

test_that("secure assign fails on warning", {
  .lazyTyper_warning2error <- TRUE
  declare(a, "numeric")
  a <- NULL
  expect_error(a[1:5] %<-s% .(1:3))

  .lazyTyper_hard_bounds <- TRUE
  declare(b, "numeric")
  b <- NULL
  expect_error(b[1] %<-s% .(1))
})

test_that("assign does not fail on warning in RHS", {
  .lazyTyper_warning2error <- TRUE
  declare(a, "numeric")
  f <- function() {
    warning("Warn!")
    1
  }
  expect_warning(a %<-% .(f()), "Warn!")
  expect_warning(a %<-s% .(f()), "Warn!")
})

test_that("assign warns on invalid usage of hard_bounds", {
  .lazyTyper_hard_bounds <- TRUE
  declare(b, "numeric")
  b <- NULL
  expect_warning(b[1] %<-% .(1), "hard bound")
  expect_warning(b[1] %<-s% .(1), "hard bound")
})
