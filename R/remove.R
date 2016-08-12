#' A Replacement for base \code{\link[base]{remove}}/\code{\link[base]{rm}} with
#' Support for Typed Variables
#'
#' These functions differ from the base versions only in that they (optionally)
#' untype typed variables.
#'
#' @param ... the objects to be removed, as names (unquoted) or character
#'   strings (quoted).
#' @param list a character vector naming objects to be removed.
#' @param pos where to do the removal. By default, uses the current environment.
#'   See ‘Details’ in the documentation of the base functions for other
#'   possibilities.
#' @param envir the environment to use. See ‘Details’ in the documentation of
#'   the base functions for other possibilities.
#' @param inherits should the enclosing frames of the environment be inspected?
#' @param .untype should typed variables be untyped?
#'
#' @details See `Details` in base
#'   \code{\link[base]{remove}}/\code{\link[base]{rm}}.
#'
#' @seealso \link{typed}
#'
#' @export
remove <- function(..., list = character(), pos = -1,
                   envir = as.environment(pos), inherits = FALSE,
                   .untype = TRUE) {
  dots <- match.call(expand.dots = FALSE)$...
  if (length(dots) && !all(vapply(dots, function(x) is.symbol(x) ||
                                  is.character(x), NA, USE.NAMES = FALSE)))
    stop("... must contain names or character strings")
  names <- vapply(dots, as.character, "")
  if (length(names) == 0L)
    names <- character()

  list <- .Primitive("c")(list, names)

  # force evaluation of envir-argument to make the default as.environment(pos)
  # work, since as.environment(-1) is relative to the calling environment; this
  # is also the reason why we must NOT use force() for this
  envir

  vars2untype <- character()
  if (!inherits) {
    # only untype actually existing variables and not e.g. uninitalized declared
    # ones
    for (varname in list) {
      if (exists(varname, envir = envir, inherits = FALSE)) {
        vars2untype <- c(vars2untype, varname)
      }
    }
  } else {
    # if inherits = TRUE, untype only considers existing variables anyway
    vars2untype <- list
  }
  suppressWarnings(untype(vars2untype, env = envir, inherits = inherits,
                          .character = TRUE))

  base::remove(list = list, envir = envir, inherits = inherits)
}

#' @rdname remove
#' @export
# nocov start
rm <- function(..., list = character(), pos = -1,
               envir = as.environment(pos), inherits = FALSE, .untype = TRUE) {
  dots <- match.call(expand.dots = FALSE)$...
  if (length(dots) && !all(vapply(dots, function(x) is.symbol(x) ||
                                  is.character(x), NA, USE.NAMES = FALSE)))
    stop("... must contain names or character strings")
  names <- vapply(dots, as.character, "")
  if (length(names) == 0L)
    names <- character()

  list <- .Primitive("c")(list, names)

  # force evaluation of envir-argument to make the default as.environment(pos)
  # work, since as.environment(-1) is relative to the calling environment; this
  # is also the reason why we must NOT use force() for this
  envir

  vars2untype <- character()
  if (!inherits) {
    # only untype actually existing variables and not e.g. uninitalized declared
    # ones
    for (varname in list) {
      if (exists(varname, envir = envir, inherits = FALSE)) {
        vars2untype <- c(vars2untype, varname)
      }
    }
  } else {
    # if inherits = TRUE, untype only considers existing variables anyway
    vars2untype <- list
  }
  suppressWarnings(untype(vars2untype, env = envir, inherits = inherits,
                          .character = TRUE))

  base::remove(list = list, envir = envir, inherits = inherits)
}
# nocov end
