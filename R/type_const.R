checkPropertiesFun.const <- function(value) {
  stopifnot(hasValue("value"))

  args2list()
}

check_type_expr.const <- quote({
  if (!identical(.XXX., .lazyTyper_properties[["value"]])) {
    markInvalidWError("Modified constant.")
  }
})
