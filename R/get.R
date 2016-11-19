#' Get Value of a Typed Variable
#'
#' Return the value of a typed variable if it is valid, otherwise throw an
#' error.
#'
#' @param x the variable of which the value should be returned.
#' @param env the environment in which to look for \code{x}. Defaults to the
#'   current environment.
#' @param inherits a logical value indicating if the enclosing frames of the
#'   environment should be inspected.
#' @param .character a logical value indicating if \code{x} is the variable to
#'   get or a character string with the name of the variable to get.
#'
#' @return x
#'
#' @seealso \link{typed}, \code{\link{\%<-\%}} for securely assigning to a typed
#'   variable
#'
#' @export
g <- function(x, env = parent.frame(), inherits = FALSE, .character = FALSE) {
  setErrorContext(
    "syntaxError",
    base_class = "lazyTyperError"
  )

  varname <- getVarNames(x, sx = substitute(x), .character = .character,
                         .single = TRUE)

  if (!is.environment(env)) {
    signal(
      stackError(
        "'env' must be an environment.",
        base_class = "lazyTyperError"
      )
    )
  }
  if (inherits) {
    this_env <- getEnvOfObject(varname, env = env)
  } else {
    this_env <- env
  }

  setErrorContext(
    "getError",
    c(
      modifiedConstantError = paste0("Failed to get the constant '", varname,
                                     "'."),
      paste0("Failed to get the variable '", varname, "'.")
    ),
    base_class = "lazyTyperError"
  )

  if (!exists(varname, envir = this_env, inherits = FALSE)) {
    signal(
      stackError(
        "No such object.",
        "notExistingError",
        base_class = "lazyTyperError"
      )
    )
  }

  checkType(varname, env = this_env)

  get(varname, envir = this_env, inherits = FALSE)
}
