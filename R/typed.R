#' Typed Variables
#'
#' Declare variables of or cast variables to a specific type, test typed
#' variables for validity and untype them again.
#'
#' @param x variable to be typed or tested.
#' @param type the desired type of \code{x} (as a character string).
#' @param ... further properties of \code{x} to guarantee (as named arguments).
#'   The availability of additional properties depends on \code{type}. Both
#'   positional and partial matching are used on these arguments.
#' @param env the environment in which to look for \code{x} / where \code{x}
#'   should be declared. Defaults to the current environment.
#' @param inherits a logical value indicating if the enclosing frames of the
#'   environment should be inspected.
#' @param .character a logical value indicating if \code{x} is the object to
#'   operate on or a character vector with the names of objects to operate on.
#'   \code{cast} always only operates on a single object.
#'
#' @details Use \code{declare} to declare one or more non-existing variable in a
#'   specific environment (the actual objects will NOT be created). Use
#'   \code{cast} to type an already existing variable.
#'
#'   \code{const} is a special type of cast, which turns an existing untyped
#'   variable into a pseudo-constant: assigning to it (using
#'   \code{\link{\%<-\%}}) will fail if this would actually change the object
#'   and getting the value of it (using \code{\link{g}}) will fail if it was
#'   modified after casting \code{x} to a constant.
#'
#'   \code{untype} untypes a formerly declare or casted variable. To untype a
#'   non-existing variable, e.g. before initialization of a declared variable or
#'   after calling \code{\link{remove}} with option \code{.untype = FALSE}, the
#'   environment in which to untype \code{x} must be explicitly given, i.e. set
#'   \code{inherits = FALSE}. This is because \code{untype} with \code{inherits
#'   = TRUE} first searches for \code{x} and then untypes it, which will either
#'   fail (with a warning) if a variable \code{x} does not exist anywhere or the
#'   wrong \code{x} will be untyped.
#'
#'   \code{is.type} tests whether the variable \code{x} is typed, whereas
#'   \code{is.valid} tests whether the current value of \code{x} is valid given
#'   it type.
#'
#' @return For \code{untype} an invisible character vector with the names of the
#'   variables which were to be untyped. This allows shorthands of the following
#'   form for redeclaring/recasting variables: \code{cast(untype(var),
#'   "numeric", .character = TRUE)}.
#'
#'   For \code{is.typed} either \code{TRUE} or \code{FALSE}, the former with two
#'   additional attributes: "type" (the type of \code{x} as a character string)
#'   and "properties" (a possibly empty list of additional properties of
#'   \code{x}).
#'
#'   For \code{is.valid} either \code{TRUE} or \code{FALSE}, the latter with an
#'   additional attribute: "error" (a character vector describing why the
#'   variable is not valid). Calling \code{is.valid} on an untyped variable will
#'   always return \code{FALSE}.
#'
#' @seealso \code{\link{\%<-\%}} for securely assigning to a typed variable,
#'   \code{g} for securely getting the value of a typed variable,
#'   \code{\link{remove}}  for a replacement for base \code{\link[base]{remove}}
#'   with support for typed variables
#'
#' @examples
#' declare(var, "character", length = 2)
#' var %<-% letters[1:2]
#'
#' \dontrun{
#' var %<-% c("a", "b", "c")  # error: wrong length}
#'
#' @name typed
NULL

#' @rdname typed
#' @export
cast <- function(x, type, ..., env = parent.frame(), inherits = TRUE,
                 .character = FALSE) {
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

  if (existsInLazyTyperEnv(varname, env = this_env)) {
    stop("Variable '", varname,  "' already declared/typed. To retype this ",
         "variable, untype it first.")
  }

  if (!exists(varname, envir = this_env, inherits = FALSE)) {
    stop("Variable '", varname, "' does not exist and hence cannot be ",
         "casted. Use 'declare' instead.")
  }

  assignToLazyTyperEnv(varname, type = type, properties = list(...),
                       env = this_env)

  valid <- checkType(varname, env = this_env)

  if (!valid) {
    removeFromLazyTyperEnv(varname, env = this_env)
    stop("Variable ", varname, " is not of type '", type, "' or it does not ",
         "have the desired properties:\n", attr(valid, "error"))
  }
}

#' @rdname typed
#' @export
const <- function(x, env = parent.frame(), inherits = TRUE,
                  .character = FALSE) {
  varname <- getVarNames(x, sx = substitute(x), .character = .character)
  if (length(varname) != 1) {
    stop("If '.character = TRUE', 'x' must be a character string, i.e. a ",
         "character vector of length 1.")
  }
  cast(varname, type = "const", value = x, env = env, inherits = inherits,
       .character = TRUE)
}

#' @rdname typed
#' @export
declare <- function(x, type, ..., env = parent.frame(), .character = FALSE) {
  varnames <- getVarNames(x, sx = substitute(x), .character = .character)

  for (varname in varnames) {
    if (exists(varname, envir = env, inherits = FALSE)) {
      stop("Variable '", varname, "' already exists and hence cannot be ",
           "declared. Use 'cast' instead.")
    }

    if (existsInLazyTyperEnv(varname, env = env)) {
      stop("Variable '", varname,  "' already declared. To redeclare this ",
           "variable, untype it first.")
    }
  }

  for (varname in varnames) {
    assignToLazyTyperEnv(varname, type = type, properties = list(...),
                         env = env)
  }
}

#' @rdname typed
#' @export
untype <- function(x, env = parent.frame(), inherits = FALSE,
                   .character = FALSE) {
  varnames <- getVarNames(x, sx = substitute(x), .character = .character)

  for (varname in varnames) {
    if (inherits) {
      this_env <- getEnvOfObject(varname, env = env)
    } else {
      this_env <- env
    }
    removeFromLazyTyperEnv(varname, env = this_env)
  }

  return(invisible(varnames))
}

#' @rdname typed
#' @export
is.typed <- function(x) {
  varname <- getVarNames(x, sx = substitute(x), .character = FALSE)
  lazyTyperList <- getFromLazyTyperEnv(varname, env = parent.frame())
  if (!is.null(lazyTyperList)) {
    res <- TRUE
    attr(res, "type") <- lazyTyperList[["type"]]
    attr(res, "properties") <- lazyTyperList[["properties"]]
    return(res)
  } else {
    return(FALSE)
  }
}

#' @rdname typed
#' @export
is.valid <- function(x) {
  varname <- getVarNames(x, sx = substitute(x), .character = FALSE)
  if (!exists(varname, envir = parent.frame(), inherits = FALSE)) {
    stop("No such object: ", varname)
  }
  checkType(varname = varname, env = parent.frame())
}
