context("type_numeric")

test_that("all properties can be set", {
  declare(a, "numeric", length = 1)
  declare(b, "numeric", min_length = 1)
  declare(c, "numeric", max_length = 1)
  declare(d, "numeric", set = 1)
  declare(e, "numeric", min = 1)
  declare(f, "numeric", max = 1)
  declare(g, "numeric", allow_NA = TRUE)
  declare(h, "numeric", allow_NaN = TRUE)
})
