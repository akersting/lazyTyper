# nocov start
checkPropertiesFun.vector <- function(length, min_length, max_length, set, min,
                                      max, whole, pattern, allow_duplicates,
                                      allow_NA, allow_NaN, allow_NULL,
                                      allow_missing, type) {
  # get fixed values
  list2env(as.list(parent.env(environment()), all.names = TRUE),
           envir = environment())

  has_value <- hasValue(c("length", "min_length", "max_length", "set", "min",
                          "max", "whole", "pattern", "allow_duplicates",
                          "allow_NA", "allow_NaN", "allow_NULL",
                          "allow_missing"))

  if (has_value["length"] && (!is.numeric(length) ||
                              base::length(length) != 1 || is.na(length) ||
                              length < 0)) {
    signal(
      stackError(
        "'length' must be a scalar greater equal 0.",
        base_class = "lazyTyperError"
      )
    )
  }

  if (has_value["min_length"]) {
    if (has_value["length"]) {
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

  if (has_value["max_length"]) {
    if (has_value["length"]) {
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

      if (has_value["min_length"] && min_length > max_length) {
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

  if (has_value["set"]) {
    if (has_value["min"] || has_value["max"] || has_value["whole"] ||
        has_value["pattern"]) {
      signal(
        stackError(
          paste0(
            "If 'set' is given, ",
            paste0("'", c("min", "max", "whole", "pattern")[
              c(has_value["min"], has_value["max"],
                has_value["whole"], has_value["pattern"])], "'",
              collapse = ", "),
            " must not be given."
          ),
          base_class = "lazyTyperError"
        )
      )

    }

    correct_type <- iMode(set) == type
    if (!correct_type) {
      signal(
        stackError(
          paste0("'set' must be a ", type, " vector."),
          base_class = "lazyTyperError"
        )
      )
    }

    if (any(is.na(set))) {
      if (has_value["allow_NA"] && !allow_NA) {
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
      if (has_value["allow_NaN"] && !allow_NaN) {
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

  if (has_value["min"] && (!is.numeric(min) || base::length(min) != 1 ||
                           is.na(min))) {
    signal(
      stackError(
        "'min' must be a scalar.",
        base_class = "lazyTyperError"
      )
    )
  }

  if (has_value["max"]) {
    if (!is.numeric(max) || base::length(max) != 1 || is.na(max)) {
      signal(
        stackError(
          "'max' must be a scalar.",
          base_class = "lazyTyperError"
        )
      )
    }

    if (has_value["min"] && min > max) {
      signal(
        stackError(
          "'max' must be larger equal 'min'.",
          "conflictingPropertyError",
          base_class = "lazyTyperError"
        )
      )
    }
  }



  if (has_value["whole"] && (!is.logical(whole) || base::length(whole) != 1 ||
                             is.na(whole))) {
    signal(
      stackError(
        "'whole' must be a logical value.",
        base_class = "lazyTyperError"
      )
    )
  }

  if (has_value["pattern"]) {
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

  if (has_value["allow_duplicates"] && (!is.logical(allow_duplicates) ||
                                        base::length(allow_duplicates) != 1 ||
                                        is.na(allow_duplicates))) {
    signal(
      stackError(
        "'allow_duplicates' must be a logical value.",
        base_class = "lazyTyperError"
      )
    )
  }

  if (has_value["allow_NA"] && (!is.logical(allow_NA) ||
                                base::length(allow_NA) != 1 ||
                                is.na(allow_NA))) {
    signal(
      stackError(
        "'allow_NA' must be a logical value.",
        base_class = "lazyTyperError"
      )
    )
  }

  if (has_value["allow_NaN"] && (!is.logical(allow_NaN) ||
                                 base::length(allow_NaN) != 1 ||
                                 is.na(allow_NaN))) {
    signal(
      stackError(
        "'allow_NaN' must be a logical value.",
        base_class = "lazyTyperError"
      )
    )
  }

  if (has_value["allow_NULL"] && (!is.logical(allow_NULL) ||
                                  base::length(allow_NULL) != 1 ||
                                  is.na(allow_NULL))) {
    signal(
      stackError(
        "'allow_NULL' must be a logical value.",
        base_class = "lazyTyperError"
      )
    )
  }

  if (has_value["allow_missing"] && (!is.logical(allow_missing) ||
                                     base::length(allow_missing) != 1 ||
                                     is.na(allow_missing))) {
    signal(
      stackError(
        "'allow_missing' must be a logical value.",
        base_class = "lazyTyperError"
      )
    )
  }

  args2list()
}

#' @importFrom stats na.omit
checkTypeFun.vector <- function(x, length, min_length, max_length, set, min,
                                max, whole, pattern, allow_duplicates, allow_NA,
                                allow_NaN, allow_NULL, allow_missing, type) {
  has_value <- hasValue(c("length", "min_length", "max_length", "set", "min",
                          "max", "whole", "pattern", "allow_duplicates",
                          "allow_NA", "allow_NaN", "allow_NULL",
                          "allow_missing"))

  if (identical(x, quote(expr = ))) {
    if (!has_value["allow_missing"] || !allow_missing) {
      signal(
        stackError(
          paste0("The variable is a missing argument, i.e. it is identical to ",
                 "the empty name."),
          base_class = "lazyTyperError"
        )
      )
    }
    return(invisible())  # X is missing -> we are done
  }

  if (is.null(x)) {
    if (!has_value["allow_NULL"] || !allow_NULL) {
      signal(
        stackError(
          "The variable is of type 'NULL'.",
          base_class = "lazyTyperError"
        )
      )
    }
    return(invisible())  # X is NULL -> we are done
  }

  correct_type <- if (iMode(x) == type) {
    TRUE
  } else if (type == "vector" && !is.array(x) &&
             (is.logical(x) || is.numeric(x) || is.complex(x) ||
              is.character(x) || is.raw(x) || is.factor(x))) {
    TRUE
  } else {
    FALSE
  }
  if (!correct_type) {
    signal(
      stackError(
        paste0("The variable has the wrong type. Expected: ", type,
               ", actual: ", iMode(x), "."),
        base_class = "lazyTyperError"
      )
    )
    return(invisible())  # stop here if ignoreError restart is invoked
  }

  if (has_value["length"] && base::length(x) != length) {
    signal(
      stackError(
        paste0("The variable has the wrong length. Expected length: ", length,
               ", actual length: ", base::length(x), "."),
        c("invalidPropertyValueError"),
        base_class = "lazyTyperError"
      )
    )
  }
  if (has_value["min_length"] && base::length(x) < min_length) {
    signal(
      stackError(
        paste0("The variable has the wrong length. Expected min length: ",
               min_length, ", actual length: ", base::length(x), "."),
        c("invalidPropertyValueError"),
        base_class = "lazyTyperError"
      )
    )
  }
  if (has_value["max_length"] && base::length(x) > max_length) {
    signal(
      stackError(
        paste0("The variable has the wrong length. Expected max length: ",
               min_length, ", actual length: ", base::length(x), "."),
        c("invalidPropertyValueError"),
        base_class = "lazyTyperError"
      )
    )
  }

  if (has_value["set"] || has_value["min"] || has_value["max"] ||
      has_value["whole"] || has_value["pattern"]) {
    x_omit <- na.omit(x)

    if (has_value["set"] && !all(x_omit %in% set)) {
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

    if (has_value["min"] && any(x_omit < min)) {
      signal(
        stackError(
          paste0("The variable contains too small values. Allowed smallest ",
                 "value: ", min, ", actual smallest value: ", min(x_omit), "."),
          c("invalidPropertyValueError"),
          base_class = "lazyTyperError"
        )
      )
    }
    if (has_value["max"] && any(x_omit > max)) {
      signal(
        stackError(
          paste0("The variable contains too large values. Allowed largest ",
                 "value: ", max, ", actual largest value: ", max(x_omit), "."),
          c("invalidPropertyValueError"),
          base_class = "lazyTyperError"
        )
      )
    }

    if (has_value["whole"]) {
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

    if (has_value["pattern"]) {
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
  }

  if (has_value["allow_duplicates"] && !allow_duplicates &&
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

  if (has_value["allow_NA"] && !allow_NA && any(is.na(x) & !is.nan(x))) {
    signal(
      stackError(
        "The variable contains NAs.",
        c("invalidPropertyValueError"),
        base_class = "lazyTyperError"
      )
    )
  }

  if (has_value["allow_NaN"] && !allow_NaN && any(is.nan(x))) {
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
