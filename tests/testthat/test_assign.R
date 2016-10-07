context("assign")

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
  expect_error(class(a) %<-% "character")
  expect_error(class(names(a)) %<-% "character")
})
