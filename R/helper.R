getVarNames <- function(x, sx, .character) {
  if (.character) {
    # do not test for positive length here; would break default behaviour of
    # remove/rm
    if (is.character(x)) {
      return(x)
    } else {
      stop("If '.character' is TRUE, x must be a character vector.")
    }
  } else {
    if (missing(x)) {
      stop("Argument 'x' is missing (with no default).")
    }
    if (!is.name(sx)) {
      stop("Invalid variable name: ", deparse(sx))
    }
    return(as.character(sx))
  }
}

checkPropertiesNames <- function(x) {
  stopifnot(is.list(x))
  if (length(x) > 0) {
    if (is.null(names(x)) || any(is.na(names(x))) || any(names(x) == "") ||
        length(unique(names(x))) != length(names(x))) {
      stop("All additional properties/arguments must have unique names.",
           call. = FALSE)
    }
  }
}

throwInvalidTypeError <- function(..., call = sys.call(-1)) {
  cond <- structure(class = c("invalid_type_error", "error", "condition"),
                    list(message = paste0(...), call = call))
  stop(cond)
}
