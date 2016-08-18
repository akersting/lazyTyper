context("type_character")

test_that("all properties can be set", {
  declare(a, "character", length = 1)
  declare(b, "character", min_length = 1)
  declare(c, "character", max_length = 1)
  declare(d, "character", set = "abc")
  declare(e, "character", pattern = "")
  declare(f, "character", allow_NA = TRUE)
})
