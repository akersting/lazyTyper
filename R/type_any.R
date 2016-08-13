checkPropertiesFun.any <- function(x) {
  if (length(x) != 0) {
    stop("Invalid properties: ", paste0(names(x), collapse = ", "))
  }

  x
}

check_type_expr.any <- quote({})
