#' Get the Current Values of the Formal Arguments of a Function as a List
#'
#' This function returns the \emph{current} values of the formal arguments of
#' the calling function as a list.
#'
#' @param include_ellipsis a logical value indicating if the ellipsis (...)
#'   should be included in the result.
#' @param simplify a logical value indicating if the ellipsis should be included
#'   in the result as a list with the name "..." (\code{FALSE}) or if the
#'   elements hereof should be directly included in the list returned
#'   (\code{TRUE}).
#' @param include_missing a logical value indicating if missing formal arguments
#'   should be included in the result. An argument might be missing because it
#'   was not provided when the function was called \emph{and} it does not have a
#'   default value, or because it was deleted in the body of the function before
#'   \code{args2list} was called. If \code{TRUE}, the respective elements of the
#'   list are empty names.
#' @param ... ignored. Just here to get rid of the R CMD check note "... may be
#'   used in an incorrect context: 'list(...)'".
#'
#' @return a list, named for all formal arguments included and possibly unnamed
#'   for the ellipsis, depending on whether the additional arguments were named
#'   or not.
#'
#' @keywords internal
#' @export
#'
#' @examples f <- function(a, b, c = 3, ...) {
#'   a <- 7
#'   rm(b)
#'   d <- 0
#'   e <- 99
#'   args2list()
#' }
#'
#' f(a = 1, b = 2, d = 9)  # a == 7, b not included, c == 3, d == 9 (not 0!)
#' f()  # a == 7, c == 3
#' f(a = 1, b = 2, c = 4, 5)  # result unnamed for ellipsis
args2list <- function(include_ellipsis = TRUE, simplify = TRUE,
                      include_missing = FALSE, ...) {
  if (sys.parent() == 0) {
    stop("'args2list' must not be called outside of a closure.")
  }

  formal_args <- formals(sys.function(sys.parent()))

  if (include_ellipsis && "..." %in% names(formal_args)) {
    ellipsis <- evalq(list(...), parent.frame())
  } else {
    ellipsis <- list()
  }

  formal_args <- formal_args[names(formal_args) != "..."]

  if (length(formal_args) > 0) {
    formal_args <- lapply(formal_args, function(arg) quote(expr = ))

    formal_arg_has_value <- unlist(lapply(names(formal_args), hasValue,
                                          env = parent.frame()))
    formal_args_with_value <- mget(names(formal_args)[formal_arg_has_value],
                                   envir = parent.frame())
    if (include_missing) {
      formal_args[formal_arg_has_value] <- formal_args_with_value
    } else {
      formal_args <- formal_args_with_value
    }
  }

  if (simplify) {
    return(c(formal_args, ellipsis))
  } else if (length(ellipsis) > 0) {
    return(c(formal_args, list(... = ellipsis)))
  } else {
    return(formal_args)
  }
}

getVarNames <- function(x, sx, .character = FALSE, .single = FALSE) {
  if (.character) {
    # do not test for positive length here; would break default behaviour of
    # remove/rm
    if (is.character(x)) {
      if (.single && length(x) != 1) {
        signal(
          stackError(
            paste0("'x' is not a character string, ",
                   "i.e. a character vector of length 1."),
            base_class = "lazyTyperError"
          )
        )
      }
      return(x)
    } else {
      signal(
        stackError(
          "'x' is not a character vector.",
          base_class = "lazyTyperError"
        )
      )
    }
  } else {
    if (identical(sx, quote(expr = ))) {
      signal(
        stackError(
          "Argument 'x' is missing (with no default).",
          base_class = "lazyTyperError"
        )
      )
    }
    if (!is.name(sx) && !is.character(sx)) {
      signal(
        stackError(
          paste0("Invalid variable name: ", deparse(sx)),
          base_class = "lazyTyperError"
        )
      )
    }
    return(as.character(sx))
  }
}

`%\\%` <- function(x, y) {
  paste0(x, y)
}
