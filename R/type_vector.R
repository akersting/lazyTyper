# nocov start
checkPropertiesFun.vector <- function(length, min_length, max_length, set, min,
                                      max, whole, pattern, allow_duplicates,
                                      allow_NA, allow_NaN, allow_NULL, type) {
  # get fixed values
  list2env(as.list(parent.env(environment()), all.names = TRUE),
           envir = environment())

  if (hasValue("length") && (!is.numeric(length) || base::length(length) != 1 ||
                             is.na(length) || length < 0)) {
    signal(
      stackError(
        "'length' must be a scalar greater equal 0.",
        base_class = "lazyTyperError"
      )
    )
  }

  if (hasValue("min_length")) {
    if (hasValue("length")) {
      signal(
        stackError(
          "'min_length' must not be set if 'length' is set.",
          "conflictingPropertyError",
          base_class = "lazyTyperError"
        )
      )
    } else if (!is.numeric(min_length) || base::length(min_length) != 1 ||
               is.na(min_length) || min_length < 0) {
      signal(
        stackError(
          "'min_length' must be a scalar greater equal 0.",
          base_class = "lazyTyperError"
        )
      )
    }
  }

  if (hasValue("max_length")) {
    if (hasValue("length")) {
      signal(
        stackError(
          "'max_length' must not be set if 'length' is set.",
          "conflictingPropertyError",
          base_class = "lazyTyperError"
        )
      )
    } else {
      if (!is.numeric(max_length) || base::length(max_length) != 1 ||
          is.na(max_length) || max_length < 0) {
        signal(
          stackError(
            "'max_length' must be a scalar greater equal 0.",
            base_class = "lazyTyperError"
          )
        )
      }

      if (hasValue("min_length") && min_length > max_length) {
        signal(
          stackError(
            "'max_length' must be larger equal 'min_length'.",
            "conflictingPropertyError",
            base_class = "lazyTyperError"
          )
        )
      }
    }
  }

  if (hasValue("set")) {
    if (hasValue("min") || hasValue("max") || hasValue("whole") ||
        hasValue("pattern")) {
      signal(
        stackError(
          paste0(
            "If 'set' is given, ",
            paste0("'", c("min", "max", "whole", "pattern")[
              c(hasValue("min"), hasValue("max"),
                hasValue("whole"), hasValue("pattern"))], "'", collapse = ", "),
            " must not be given."
          ),
          base_class = "lazyTyperError"
        )
      )

    }
    correct_type <- switch(
      type,
      logical = {
        is.null(dim(set)) && is.logical(set)
      },
      numeric = {
        is.null(dim(set)) && is.numeric(set)
      },
      character = {
        is.null(dim(set)) && is.character(set)
      }
    )

    if (!correct_type) {
      signal(
        stackError(
          paste0("'set' must be a ", type, " vector."),
          base_class = "lazyTyperError"
        )
      )
    }

    if (any(is.na(set))) {
      if (hasValue("allow_NA") && !allow_NA) {
        signal(
          stackError(
            "If 'NA' is in 'set' then 'allow_NA' must not be FALSE.",
            "conflictingPropertyError",
            base_class = "lazyTyperError"
          )
        )
      }
      allow_NA <- TRUE
    }
    if (any(is.nan(set))) {
      if (hasValue("allow_NaN") && !allow_NaN) {
        signal(
          stackError(
            "If 'NaN' is in 'set' then 'allow_NaN' must not be FALSE.",
            "conflictingPropertyError",
            base_class = "lazyTyperError"
          )
        )
      }
      allow_NaN <- TRUE
    }

    set <- unique(na.omit(set))
  }

  if (hasValue("min") && (!is.numeric(min) || base::length(min) != 1 ||
                          is.na(min))) {
    signal(
      stackError(
        "'min' must be a scalar.",
        base_class = "lazyTyperError"
      )
    )
  }

  if (hasValue("max")) {
    if (!is.numeric(max) || base::length(max) != 1 || is.na(max)) {
      signal(
        stackError(
          "'max' must be a scalar.",
          base_class = "lazyTyperError"
        )
      )
    }

    if (hasValue("min") && min > max) {
      signal(
        stackError(
          "'max' must be larger equal 'min'.",
          "conflictingPropertyError",
          base_class = "lazyTyperError"
        )
      )
    }
  }



  if (hasValue("whole") && (!is.logical(whole) || base::length(whole) != 1 ||
                            is.na(whole))) {
    signal(
      stackError(
        "'whole' must be a logical value.",
        base_class = "lazyTyperError"
      )
    )
  }

  if (hasValue("pattern")) {
    if (!is.character(pattern) ||
        base::length(pattern) != 1 ||
        is.na(pattern)) {
      signal(
        stackError(
          "'pattern' must be a character string.",
          base_class = "lazyTyperError"
        )
      )
    }

    contextualize(
      grep(pattern, ""),
      error = list(
        message = "'pattern' is not a valid regular expression.",
        base_class = "lazyTyperError"
      )
    )
  }

  if (hasValue("allow_duplicates") && (!is.logical(allow_duplicates) ||
                             base::length(allow_duplicates) != 1 ||
                             is.na(allow_duplicates))) {
    signal(
      stackError(
        "'allow_duplicates' must be a logical value.",
        base_class = "lazyTyperError"
      )
    )
  }

  if (hasValue("allow_NA") && (!is.logical(allow_NA) ||
                               base::length(allow_NA) != 1 ||
                               is.na(allow_NA))) {
    signal(
      stackError(
        "'allow_NA' must be a logical value.",
        base_class = "lazyTyperError"
      )
    )
  }

  if (hasValue("allow_NaN") && (!is.logical(allow_NaN) ||
                                base::length(allow_NaN) != 1 ||
                                is.na(allow_NaN))) {
    signal(
      stackError(
        "'allow_NaN' must be a logical value.",
        base_class = "lazyTyperError"
      )
    )
  }

  if (hasValue("allow_NULL") && (!is.logical(allow_NULL) ||
                                 base::length(allow_NULL) != 1 ||
                                 is.na(allow_NULL))) {
    signal(
      stackError(
        "'allow_NULL' must be a logical value.",
        base_class = "lazyTyperError"
      )
    )
  }

  args2list()
}

#' @importFrom stats na.omit
checkTypeFun.vector <- function(x, length, min_length, max_length, set, min,
                                max, whole, pattern, allow_duplicates, allow_NA,
                                allow_NaN, allow_NULL, type) {
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

  if (hasValue("allow_duplicates") && !allow_duplicates &&
      ((is.numeric(x) && anyDuplicated(x, incomparables = c(NA, NaN))) ||
      (!is.numeric(x) && anyDuplicated(x, incomparables = NA)))) {
    signal(
      stackError(
        "The elements of the variable are not unique.",
        c("invalidPropertyValueError"),
        base_class = "lazyTyperError"
      )
    )
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
