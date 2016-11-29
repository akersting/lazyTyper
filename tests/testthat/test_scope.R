context("SCOPE")

test_that("scopes can be nested", {
  SCOPE %->% {
    declare(a, "numeric")
    eval(untype(a))
    a <- "348732"

    declare(b, "numeric")
    b <- 1
    rm(b)

    SCOPE %->% {
      b <- 782398
      declare(d, "character")

      evalq(declare(e, "character"))
      e <- "467765"
    }
    declare(d, "character")
    d <- "329480"
  }

  expect_equal(a, "348732")
  expect_equal(b, 782398)
  expect_false(exists("d", inherits = FALSE))
  expect_equal(e, "467765")
})
