context("remove/rm")

test_that("remove/rm does not unexpectately differ from base remove/rm", {
  expect_equal(as.character(body(remove))[-(7:11)],
               as.character(body(base::remove))[-7])
  expect_equal(as.character(body(rm))[-c(7:11)],
               as.character(body(base::rm))[-7])
})

test_that("code of remove and rm is identical", {
  expect_equal(as.character(body(rm)),
               as.character(body(remove)))
})

test_that("remove does not untype non-existing variables", {
  declare(a, "numeric")
  suppressWarnings(remove(a))
  expect_true(is.typed(a))
  suppressWarnings(remove(list = "a"))
  expect_true(is.typed(a))

  try(remove(a, inherits = TRUE), silent = TRUE)
  expect_true(is.typed(a))
  try(remove(list = "a", inherits = TRUE), silent = TRUE)
  expect_true(is.typed(a))
})
