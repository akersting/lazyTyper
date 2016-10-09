#' Typed Numeric Vectors
#'
#' The following arguments can be passed as \code{...} to \code{\link{declare}}
#' and \code{\link{cast}}.
#'
#' @param length the exact length of the vector.
#' @param min_length,max_length the minimum/maximum length of the vector
#'   (ignored if \code{lenght} is given).
#' @param *set the set of allowed values as a numeric vector.
#' @param min,max the minimum/maximum values allowed (ignored if \code{set} is
#'   given).
#' @param allow_NA,allow_NaN are \code{NA} and \code{NaN} values allowed?
#'   \code{FALSE} for \code{allow_NA} or \code{allow_NaN} is ignored here if
#'   \code{set} contains \code{NA} or \code{NaN} respectively.
#'
#' @details Properties marked with a * are checked using non-primitive
#'   functions, which increases the reference count of the object. Hence, after
#'   checking the validity of a variable with such a property, it can no longer
#'   be modified in place, i.e the next modification of it will result in a
#'   copy of it being made.
#'
#' @name numeric
#' @aliases numeric
#' @family types
NULL

checkPropertiesFun.numeric <- function(length, min_length, max_length, set, min,
                                       max, allow_NA, allow_NaN) {
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
    stopifnot(is.numeric(set))
  }
  if (hasValue("min")) {
    if (hasValue("set")) {
      warning("Ignoring 'min' since 'set' is given.")
      rm(min)
    } else {
      stopifnot(is.numeric(min), base::length(min) == 1)
    }
  }
  if (hasValue("max")) {
    if (hasValue("set")) {
      warning("Ignoring 'max' since 'set' is given.")
      rm(max)
    } else {
      stopifnot(is.numeric(max), base::length(max) == 1)

      if (hasValue("min")) {
        stopifnot(max >= min)
      }
    }
  }

  if (hasValue("allow_NA")) {
    stopifnot(is.logical(allow_NA), base::length(allow_NA) == 1,
              !is.na(allow_NA))
    if (!allow_NA && hasValue("set") && any(is.na(set))) {
      warning("Ignoring 'allow_NA = FALSE' since 'set' contains 'NA'.")
      rm(allow_NA)
    }
  }
  if (hasValue("allow_NaN")) {
    stopifnot(is.logical(allow_NaN), base::length(allow_NaN) == 1,
              !is.na(allow_NaN))
    if (!allow_NaN && hasValue("set") && any(is.nan(set))) {
      warning("Ignoring 'allow_NaN = FALSE' since 'set' contains 'NaN'.")
      rm(allow_NaN)
    }
  }

  args2list()
}

checkTypeFun.numeric <- function(x, length, min_length, max_length, set, min,
                                 max, allow_NA, allow_NaN) {
  if (!is.numeric(x)) {
    markInvalidWError("wrong type: ", typeof(x))
  } else {
    if (!missing(length) && base::length(x) != length) {
      markInvalidWError("wrong length: expected ", length, ", actual ",
                        base::length(x))
    }

    if (!missing(min_length) && base::length(x) < min_length) {
      markInvalidWError("wrong length: length expected to be min",
                        min_length, ", actual ", base::length(x))
    }
  }
}
