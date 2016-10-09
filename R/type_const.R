checkPropertiesFun.const <- function(value) {
  stopifnot(hasValue("value"))

  args2list()
}

checkTypeFun.const <- function(x, value) {
  if (!identical(x, value)) {
    markInvalidWError("Modified constant.")
  }
}
