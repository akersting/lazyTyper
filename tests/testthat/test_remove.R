context("remove/rm")

test_that("remove/rm does not unexpectately differ from base remove/rm", {
  A <- as.character(body(base::remove))[-7]
  B <- gsub("\n[ ]*", "", as.character(body(remove))[-(7:11)])
  for (i in seq_along(A)) {
    expect_equal(grep(A[i], B[i], fixed = TRUE), 1)
  }

  A <- as.character(body(base::rm))[-7]
  B <- gsub("\n[ ]*", "", as.character(body(rm))[-(7:11)])
  for (i in seq_along(A)) {
    expect_equal(grep(A[i], B[i], fixed = TRUE), 1)
  }

  # fails if run by covr:

  # expect_equal(as.character(body(remove))[-(7:11)],
  #              as.character(body(base::remove))[-7])
  # expect_equal(as.character(body(rm))[-c(7:11)],
  #              as.character(body(base::rm))[-7])
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
