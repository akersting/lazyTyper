#' Typed Character Vectors
#'
#' The following arguments can be passed as \code{...} to \code{\link{declare}}
#' and \code{\link{cast}}.
#'
#' @param length the exact length of the vector.
#' @param min_length,max_length the minimum/maximum length of the vector
#'   (ignored if \code{lenght} is given).
#' @param *set the set of allowed values as a character vector.
#' @param *pattern a regular expression which all elements of the vector must
#'   match to (ignored if \code{set} is given).
#' @param allow_NA are \code{NA} values allowed? \code{FALSE} is ignored here if
#'   \code{set} contains \code{NA}.
#'
#' @details Properties marked with a * are checked using non-primitive
#'   functions, which increases the reference count of the object. Hence, after
#'   checking the validity of a variable with such a property, it can no longer
#'   be modified in place, i.e the next modification of it will result in a copy
#'   of it being made.
#'
#' @name character
#' @aliases character
#' @family types
NULL

checkPropertiesFun.character <- function(length, min_length, max_length, set,
                                         pattern, allow_NA) {
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
    stopifnot(is.character(set))
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
    if (!allow_NA && hasValue("set") && any(is.na(set))) {
      warning("Ignoring 'allow_NA = FALSE' since 'set' contains 'NA'.")
      rm(allow_NA)
    }
  }

  args2list()
}

checkTypeFun.character <- function(x, length, min_length, max_length, set,
                                   pattern, allow_NA) {
  if (!is.character(x)) {
    markInvalidWError("Wrong type: ", typeof(x))
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
