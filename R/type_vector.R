# nocov start
checkPropertiesFun.vector <- function(length, min_length, max_length, set, min,
                                      max, whole, pattern, allow_NA,
                                      allow_NaN, allow_NULL, type) {
  # get fixed values
  list2env(as.list(parent.env(environment()), all.names = TRUE),
           envir = environment())

  if (hasValue("length")) {
    stopifnot(is.numeric(length), base::length(length) == 1, length >= 0)
  }

  if (hasValue("min_length")) {
    if (hasValue("length")) {
      warning("Ignoring 'min_length' since 'length' is given.")
      rm(min_length)
    } else {
      stopifnot(is.numeric(min_length), base::length(min_length) == 1,
                min_length >= 0)
    }
  }

  if (hasValue("max_length")) {
    if (hasValue("length")) {
      warning("Ignoring 'max_length' since 'length' is given.")
      rm(max_length)
    } else {
      stopifnot(is.numeric(max_length), base::length(max_length) == 1,
                max_length >= 0)

      if (hasValue("min_length")) {
        stopifnot(max_length >= min_length)
      }
    }
  }

  if (hasValue("set")) {
    switch(type,
           logical = stopifnot(is.logical(set)),
           numeric = stopifnot(is.numeric(set)),
           character = stopifnot(is.character(set)))
    if (any(is.na(set))) {
      if (hasValue("allow_NA") && !allow_NA) {
        stop("NA in set by allow_NA = FALSE")
      }
      allow_NA <- TRUE
    }
    if (any(is.nan(set))) {
      if (hasValue("allow_NaN") && !allow_NaN) {
        stop("NaN in set by allow_NaN = FALSE")
      }
      allow_NaN <- TRUE
    }
  }
  if (hasValue("min")) {
    if (hasValue("set")) {
      warning("Ignoring 'min' since 'set' is given.")
      rm(min)
    } else {
      stopifnot(is.numeric(min), base::length(min) == 1, !is.na(min))
    }
  }
  if (hasValue("max")) {
    if (hasValue("set")) {
      warning("Ignoring 'max' since 'set' is given.")
      rm(max)
    } else {
      stopifnot(is.numeric(max), base::length(max) == 1, !is.na(max))

      if (hasValue("min")) {
        stopifnot(max >= min)
      }
    }
  }
  if (hasValue("whole")) {
    if (hasValue("set")) {
      warning("Ignoring 'whole' since 'set' is given.")
      rm(whole)
    } else {
      stopifnot(is.logical(whole), base::length(whole) == 1, !is.na(whole))
    }
  }

  if (hasValue("pattern")) {
    if (hasValue("set")) {
      warning("Ignoring 'pattern' since 'set' is given.")
      rm(pattern)
    } else {
      stopifnot(is.character(pattern), base::length(pattern) == 1)
      grep(pattern, "")  # test the regular expression
    }
  }

  if (hasValue("allow_NA")) {
    stopifnot(is.logical(allow_NA), base::length(allow_NA) == 1,
              !is.na(allow_NA))
    # if (!allow_NA && hasValue("set") && any(is.na(set))) {
    #   warning("Ignoring 'allow_NA = FALSE' since 'set' contains 'NA'.")
    #   rm(allow_NA)
    # }
  }
  if (hasValue("allow_NaN")) {
    stopifnot(is.logical(allow_NaN), base::length(allow_NaN) == 1,
              !is.na(allow_NaN))
    # if (!allow_NaN && hasValue("set") && any(is.nan(set))) {
    #   warning("Ignoring 'allow_NaN = FALSE' since 'set' contains 'NaN'.")
    #   rm(allow_NaN)
    # }
  }
  if (hasValue("allow_NULL")) {
    stopifnot(is.logical(allow_NULL), base::length(allow_NULL) == 1,
              !is.na(allow_NULL))
  }

  args2list()
}

#' @importFrom stats na.omit
checkTypeFun.vector <- function(x, length, min_length, max_length, set, min,
                                max, whole, pattern, allow_NA, allow_NaN,
                                allow_NULL, type) {
  if (is.null(x)) {
    if (!hasValue("allow_NULL") || !allow_NULL) {
      signal(
        stackError(
          "The variable is of type 'NULL'.",
          base_class = "lazyTyperError"
        )
      )
    }
    return(invisible())  # X is NULL -> we are done
  }

  correct_type <- switch(
    type,
    logical = {
      is.null(dim(x)) && is.logical(x)
    },
    numeric = {
      is.null(dim(x)) && is.numeric(x)
    },
    character = {
      is.null(dim(x)) && is.character(x)
    },
    vector = {
      is.vector(x)
    }
  )
  if (!correct_type) {
    signal(
      stackError(
        paste0("The variable has the wrong type. Expected: ", type,
               ", actual: ", mode(x), "."),
        base_class = "lazyTyperError"
      )
    )
    return(invisible())  # stop here if ignoreError restart is invoked
  }

  if (hasValue("length") && base::length(x) != length) {
    signal(
      stackError(
        paste0("The variable has the wrong length. Expected length: ", length,
               ", actual length: ", base::length(x), "."),
        c("invalidPropertyValueError"),
        base_class = "lazyTyperError"
      )
    )
  }
  if (hasValue("min_length") && base::length(x) < min_length) {
    signal(
      stackError(
        paste0("The variable has the wrong length. Expected min length: ",
               min_length, ", actual length: ", base::length(x), "."),
        c("invalidPropertyValueError"),
        base_class = "lazyTyperError"
      )
    )
  }
  if (hasValue("max_length") && base::length(x) > max_length) {
    signal(
      stackError(
        paste0("The variable has the wrong length. Expected max length: ",
               min_length, ", actual length: ", base::length(x), "."),
        c("invalidPropertyValueError"),
        base_class = "lazyTyperError"
      )
    )
  }

  if (hasValue("set") || hasValue("min") || hasValue("max") ||
      hasValue("whole") || hasValue("pattern")) {
    x_omit <- na.omit(x)
  }

  if (hasValue("set") && !all(x_omit %in% set)) {
    signal(
      stackError(
        paste0("The variable contains the following values which are not in ",
               "the set of allowed values: ",
               paste0(x_omit[!x_omit %in% set], collapse = ", "), "."),
        c("invalidPropertyValueError"),
        base_class = "lazyTyperError"
      )
    )
  }

  if (hasValue("min") && any(x_omit < min)) {
    signal(
      stackError(
        paste0("The variable contains too small values. Allowed smallest ",
               "value: ", min, ", actual smallest value: ", min(x_omit), "."),
        c("invalidPropertyValueError"),
        base_class = "lazyTyperError"
      )
    )
  }
  if (hasValue("max") && any(x_omit > max)) {
    signal(
      stackError(
        paste0("The variable contains too large values. Allowed largest ",
               "value: ", max, ", actual largest value: ", max(x_omit), "."),
        c("invalidPropertyValueError"),
        base_class = "lazyTyperError"
      )
    )
  }

  if (hasValue("whole")) {
    if (any(abs(x_omit - round(x_omit)) > .Machine$double.eps^0.5)) {
      signal(
        stackError(
          "The variable does not only contain whole numbers.",
          c("invalidPropertyValueError"),
          base_class = "lazyTyperError"
        )
      )
    }
  }

  if (hasValue("pattern")) {
    x_invalid <- grep(pattern, x_omit, value = TRUE, invert = TRUE)
    if (base::length(x_invalid) > 0) {
      signal(
        stackError(
          paste0("The variable contains the following values which do not ",
                 "match the pattern: ", paste0(x_invalid, collapse = ", "),
                 "."),
          c("invalidPropertyValueError"),
          base_class = "lazyTyperError"
        )
      )
    }
  }

  if (hasValue("allow_NA") && !allow_NA && any(is.na(x) & !is.nan(x))) {
    signal(
      stackError(
        "The variable contains NAs.",
        c("invalidPropertyValueError"),
        base_class = "lazyTyperError"
      )
    )
  }

  if (hasValue("allow_NaN") && !allow_NaN && any(is.nan(x))) {
    signal(
      stackError(
        "The variable contains NaNs.",
        c("invalidPropertyValueError"),
        base_class = "lazyTyperError"
      )
    )
  }
}
# nocov end
