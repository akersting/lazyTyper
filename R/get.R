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
  varname <- getVarNames(x, sx = substitute(x), .character = .character)
  if (length(varname) != 1) {
    stop("If '.character = TRUE', 'x' must be a character string, i.e. a ",
         "character vector of length 1.")
  }

  if (inherits) {
    this_env <- getEnvOfObject(varname, env = env)
  } else {
    this_env <- env
  }

  if (!exists(varname, envir = this_env, inherits = FALSE)) {
    stop("No such object: ", varname)
  }

  valid <- checkType(varname, env = this_env)

  if (!valid) {
    stop("Variable '", varname, "' is not valid:\n", attr(valid, "error"))
  }

  get(varname, envir = this_env, inherits = FALSE)
}
