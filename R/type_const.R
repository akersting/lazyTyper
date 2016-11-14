checkPropertiesFun.const <- function(value, hash) {
  stopifnot(hasValue("value"))

  if (hasValue("hash") && hash) {
    if (requireNamespace("digest", quietly = TRUE)) {
      value <- digest::digest(value)
    } else {
      hash <- FALSE
      warning("To use hashing for constants the package 'digest' must be ",
              "installed. Hasing was disabled for this constant.",
              call. = FALSE)
    }

  }
  args2list()
}

checkTypeFun.const <- function(x, value, hash) {
  if (hasValue("hash") && hash) {
    x <- digest::digest(x)
  }
  if (!identical(x, value)) {
    markInvalidWError("Modified constant.")
  }
}
