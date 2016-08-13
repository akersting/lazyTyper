checkPropertiesFun.character <- function(x) {
  valid_properties <- c("length", "min_length", "max_length", "set", "pattern",
                        "allow_NA")
  if (any(!names(x) %in% valid_properties)) {
    stop("Invalid properties: ",
         paste0(names(x)[!names(x) %in% valid_properties], collapse = ", "))
  }

  if (!is.null(x[["length"]])) {
    stopifnot(is.numeric(x[["length"]]), length(x[["length"]]) == 1,
              x[["length"]] >= 0)
  }
  if (!is.null(x[["min_length"]])) {
    stopifnot(is.numeric(x[["min_length"]]), length(x[["min_length"]]) == 1,
              x[["min_length"]] >= 0)
    if (!is.null(x[["length"]])) {
      warning("Ignoring 'min_length' since 'length' is given.")
      x[["min_length"]] <- NULL
    }
  }
  if (!is.null(x[["max_length"]])) {
    stopifnot(is.numeric(x[["max_length"]]), length(x[["max_length"]]) == 1,
              x[["max_length"]] >= 0)
    if (!is.null(x[["length"]])) {
      warning("Ignoring 'max_length' since 'length' is given.")
      x[["max_length"]] <- NULL
    }
    if (!is.null(x[["min_length"]])) {
      stopifnot(x[["max_length"]] >= x[["min_length"]])
    }
  }

  if (!is.null(x[["set"]])) {
    stopifnot(is.character(x[["set"]]))
  }
  if (!is.null(x[["pattern"]])) {
    stopifnot(is.character(x[["pattern"]]))
    if (!is.null(x[["set"]])) {
      warning("Ignoring 'pattern' since 'set' is given.")
      x[["pattern"]] <- NULL
    }
  }

  if (!is.null(x[["allow_NA"]])) {
    stopifnot(is.logical(x[["allow_NA"]]),
              length(x[["allow_NA"]]) == 1)
  }

  x
}

# nocov start
check_type_expr.character <- quote({

  if (!is.character(.XXX.)) {
    valid <- FALSE
    markInvalidWError("Wrong type: ", typeof(.XXX.))
  } else {
    if (!is.null(.lazyTyper_properties[["length"]]) &&
        length(.XXX.) != .lazyTyper_properties[["length"]]) {
      markInvalidWError("Wrong length: expected ",
                        .lazyTyper_properties[["length"]], ", actual ",
                        length(x))
    }

    if (!is.null(.lazyTyper_properties[["set"]])) {
      if (!all(.XXX. %in% .lazyTyper_properties[["set"]])) {
        markInvalidWError("Not all elements are in the expected set: ",
                          .lazyTyper_properties[["set"]])
      }
    } else if (!is.null(.lazyTyper_properties[["pattern"]]) &&
               length(grep(.lazyTyper_properties[["pattern"]], .XXX.)) !=
               length(.XXX.)) {
      markInvalidWError("Not all elements match the expected pattern: ",
                        .lazyTyper_properties[["pattern"]])
    }
  }
})
# nocov end
