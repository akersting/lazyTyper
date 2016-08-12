checkPropertiesFun.numeric <- function(x) {
  valid_properties <- c("length", "min_length", "max_length", "set", "min",
                        "max", "allow_NA", "allow_NaN")
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
    }
  }
  if (!is.null(x[["max_length"]])) {
    stopifnot(is.numeric(x[["max_length"]]), length(x[["max_length"]]) == 1,
              x[["max_length"]] >= 0)
    if (!is.null(x[["length"]])) {
      warning("Ignoring 'max_length' since 'length' is given.")
    }
    if (!is.null(x[["min_length"]])) {
      stopifnot(x[["max_length"]] >= x[["min_length"]])
    }
  }

  if (!is.null(x[["set"]])) {
    stopifnot(is.numeric(x[["set"]]))
  }
  if (!is.null(x[["min"]])) {
    stopifnot(is.numeric(x[["min"]]), length(x[["min"]]) == 1)
    if (!is.null(x[["set"]])) {
      warning("Ignoring 'min' since 'set' is given.")
    }
  }
  if (!is.null(x[["max"]])) {
    stopifnot(is.numeric(x[["max"]]), length(x[["max"]]) == 1)
    if (!is.null(x[["set"]])) {
      warning("Ignoring 'max' since 'set' is given.")
    }
    if (!is.null(x[["min"]])) {
      stopifnot(x[["max"]] >= x[["min"]])
    }
  }

  if (!is.null(x[["allow_NA"]])) {
    stopifnot(is.logical(x[["allow_NA"]]),
              length(x[["allow_NA"]]) == 1)
  }
  if (!is.null(x[["allow_NaN"]])) {
    stopifnot(is.logical(x[["allow_NaN"]]),
              length(x[["allow_NaN"]]) == 1)
  }
}

# nocov start
check_type_expr.numeric  <- quote({
  if (!is.numeric(.XXX.)) {
    lazyTyper::markInvalidWError("wrong type: ", typeof(.XXX.))
  } else {
    if (!is.null(.lazyTyper_properties[["length"]]) &&
        length(.XXX.) != .lazyTyper_properties[["length"]]) {
      lazyTyper::markInvalidWError("wrong length: expected ",
               .lazyTyper_properties[["length"]], ", actual ", length(.XXX.))
    }

    if (!is.null(.lazyTyper_properties[["set"]]) &&
        any(!.XXX. %in% .lazyTyper_properties[["set"]])) {
      lazyTyper::markInvalidWError("invalid values: ",
               .XXX.[!.XXX. %in% .lazyTyper_properties[["set"]]])
    }
  }
})
# nocov end
