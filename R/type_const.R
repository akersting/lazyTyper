checkPropertiesFun.const <- function(x) {
  stopifnot(length(x) == 1, names(x) == "value")

  x
}

check_type_expr.const <- quote({
  if (!identical(.XXX., .lazyTyper_properties[["value"]])) {
    markInvalidWError("Modified constant.")
  }
})
