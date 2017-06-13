#' Typed Variables
#'
#' Declare variables of or cast variables to a specific type, test typed
#' variables for validity and untype them again.
#'
#' @param x variable to be typed or tested.
#' @param type the desired type of \code{x} (as a character string).
#' @param ... further properties of \code{x} to guarantee. The availability of
#'   additional properties depends on \code{type}. Both positional and partial
#'   matching are used on these arguments. If a value is an object of class
#'   "DynamicProperty", this property specification is re-evaluated in the
#'   environment of the variable \code{x} every time \code{x} is checked. Hence,
#'   such a value should be a language object, e.g. created by \code{DP}.
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
#' @return For \code{declare} the empty name (invisibly) and for \code{cast} and
#'   \code{const} \code{NULL} (also invisibly).
#'
#'   For \code{untype} an invisible character vector with the names of the
#'   variables which were to be untyped. This allows shorthands of the following
#'   form for redeclaring/recasting variables: \code{cast(untype(var),
#'   "numeric", .character = TRUE)}.
#'
#'   For \code{DP} a language object of class "DynamicProperty".
#'
#'   For \code{is.typed} either \code{TRUE} or \code{FALSE}, the former with two
#'   additional attributes: "type" (the type of \code{x} as a character string)
#'   and "properties" (a possibly empty list of additional properties of
#'   \code{x}).
#'
#'   For \code{is.valid} either \code{TRUE} or \code{FALSE}, the latter with an
#'   additional attribute: "errors" (a list of condition objects indicating why
#'   the variable is not valid). Calling \code{is.valid} on an untyped variable
#'   is always an error.
#'
#' @seealso \code{\link{\%<-\%}} and \code{\link{\%<-s\%}} for securely
#'   assigning to a typed variable, \code{\link{g}} for securely getting the
#'   value of a typed variable, \code{\link{remove}}  for a replacement for base
#'   \code{\link[base]{remove}} with support for typed variables
#'
#' @examples
#' declare(var, "character", length = 2)
#' var %<-% .(letters[1:2])
#'
#' \dontrun{
#' var %<-% .(c("a", "b", "c"))  # error: wrong length}
#'
#' x <- 1:10
#' declare("w", "numeric", length = DP(length(x)))
#' w %<-% .(1:10)
#' x <- 1:20
#' is.valid(w)  # FALSE; should be of length 20 now
#'
#' @name typed
NULL

#' @rdname typed
#' @export
cast <- function(x, type, ..., env = parent.frame(), inherits = TRUE,
                 .character = FALSE) {
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
    "castError",
    paste0("Could not cast variable '", varname, "'."),
    base_class = "lazyTyperError"
  )

  if (existsInLazyTyperEnv(varname, env = this_env)) {
    signal(
      stackError(
        paste0("This variable was already declared/typed. To retype this ",
               "variable, untype it first."),
        "alreadyTypedError",
        base_class = "lazyTyperError"
      )
    )
  }

  if (!exists(varname, envir = this_env, inherits = FALSE)) {
    signal(
      stackError(
        "No such object. Maybe you meant to use 'declare' instead!?",
        "notExistingError",
        base_class = "lazyTyperError"
      )
    )
  }

  assignToLazyTyperEnv(varname, type = type, properties = list(...),
                       env = this_env)

  withCallingHandlers(
    checkType(varname, env = this_env),
    dynamicPropertiesError = function(e) {
      removeFromLazyTyperEnv(varname, env = this_env)
    },
    invalidTypeError = function(e) {
      removeFromLazyTyperEnv(varname, env = this_env)
    }
  )

  invisible(NULL)
}

#' @rdname typed
#'
#' @param hash logical. Should hashing be used to detect if a constant was
#'   modified?
#'
#' @export
const <- function(x, hash = FALSE, env = parent.frame(), inherits = TRUE,
                  .character = FALSE) {
  setErrorContext(
    "syntaxError",
    base_class = "lazyTyperError"
  )

  varname <- getVarNames(x, sx = substitute(x), .character = .character,
                         .single = TRUE)

  setErrorContext(
    base_class = "lazyTyperError"
  )

  cast(varname, type = "const", value = x, hash = hash, env = env,
       inherits = inherits, .character = TRUE)
}

#' @rdname typed
#' @export
declare <- function(x, type, ..., env = parent.frame(), .character = FALSE) {
  setErrorContext(
    "syntaxError",
    base_class = "lazyTyperError"
  )

  varnames <- getVarNames(x, sx = substitute(x), .character = .character)

  if (!is.environment(env)) {
    signal(
      stackError(
        "'env' must be an environment.",
        base_class = "lazyTyperError"
      )
    )
  }

  setErrorContext(
    "declareError",
    paste0("Failed to declare the variable(s)."),
    base_class = "lazyTyperError"
  )

  for (varname in varnames) {
    if (exists(varname, envir = env, inherits = FALSE)) {
      signal(
        stackError(
          paste0("Variable '", varname, "' already exists and hence cannot be ",
                 "declared. Use 'cast' instead."),
          "alreadyExistsError",
          base_class = "lazyTyperError"
        )
      )
    }

    if (existsInLazyTyperEnv(varname, env = env)) {
      signal(
        stackError(
          paste0("Variable '", varname,  "' already declared. To redeclare ",
                 "this variable, untype it first."),
          "alreadyTypedError",
          base_class = "lazyTyperError"
        )
      )
    }
  }

  for (varname in varnames) {
    assignToLazyTyperEnv(varname, type = type, properties = list(...),
                         env = env)
  }

  # marks vars for removal if we are in SCOPE
  if (sys.nframe() >= 3) {
    scope_frame <- sys.frame(-2)
    scope <- attr(scope_frame, "lazyTyper_scope", exact = TRUE)
    if (isTRUE(scope) && identical(env, parent.frame())) {
      attr(scope_frame, "lazyTyper_vars2remove") <-
        c(attr(scope_frame, "lazyTyper_vars2remove"), varnames)
    }
  }

  invisible(quote(expr = ))
}

#' @rdname typed
#' @export
untype <- function(x, env = parent.frame(), inherits = FALSE,
                   .character = FALSE) {
  setErrorContext(
    "syntaxError",
    base_class = "lazyTyperError"
  )

  varnames <- getVarNames(x, sx = substitute(x), .character = .character)

  if (!is.environment(env)) {
    signal(
      stackError(
        "'env' must be an environment.",
        base_class = "lazyTyperError"
      )
    )
  }
  for (varname in varnames) {
    if (inherits) {
      this_env <- getEnvOfObject(varname, env = env)
    } else {
      this_env <- env
    }
    removeFromLazyTyperEnv(varname, env = this_env)

    # remove variable from vars2remove of SCOPE
    for (i in rev(seq_len(sys.nframe() - 1))) {
      scope_frame <- sys.frame(i)
      scope <- attr(scope_frame, "lazyTyper_scope", exact = TRUE)
      if (isTRUE(scope) && identical(this_env, sys.frame(sys.parents()[i]))) {
        attr(scope_frame, "lazyTyper_vars2remove") <-
          attr(scope_frame, "lazyTyper_vars2remove")[
            attr(scope_frame, "lazyTyper_vars2remove") != varname
            ]
        break
      }
    }
  }

  return(invisible(varnames))
}

#' @rdname typed
#'
#' @param expr any syntactically valid R expression.
#'
#' @export
DP <- function(expr) {
  structure(substitute(expr), class = "DynamicProperty")
}

#' @rdname typed
#' @export
is.typed <- function(x) {
  setErrorContext(
    "syntaxError",
    base_class = "lazyTyperError"
  )

  varname <- getVarNames(x, sx = substitute(x), .character = FALSE)
  env <- parent.frame()

  lazyTyperList <- getFromLazyTyperEnv(varname, env = env)
  if (!is.null(lazyTyperList)) {
    res <- TRUE
    attr(res, "type") <- lazyTyperList[["type"]]

    properties <- lazyTyperList[["properties"]]
    if (!attr(properties, "dynamic_properties")) {
      properties <- sapply(properties, eval, envir = env, simplify = FALSE)
    }
    attr(properties, "dynamic_properties") <- NULL
    attr(res, "properties") <- properties

    return(res)
  } else {
    return(FALSE)
  }
}

#' @rdname typed
#' @export
is.valid <- function(x) {
  setErrorContext(
    "syntaxError",
    base_class = "lazyTyperError"
  )

  varname <- getVarNames(x, sx = substitute(x), .character = FALSE)

  setErrorContext(
    "validationError",
    c(
      invalidTypeError = paste0("The variable '", varname, "' is invalid."),
      paste0("Could not validate variable '", varname, "'.")
    ),
    base_class = "lazyTyperError"
  )

  if (!exists(varname, envir = parent.frame(), inherits = FALSE)) {
    signal(
      stackError(
        "No such object.",
        "notExistingError",
        base_class = "lazyTyperError"
      )
    )
  }

  valid <- TRUE
  errors <- NULL
  withCallingHandlers(
    checkType(varname = varname, env = parent.frame()),
    invalidTypeError = function(e) {
      valid <<- FALSE
      errors <<- c(errors, list(e))
      ignoreError(e)
    })
  if (!is.null(errors)) {
    attr(valid, "errors") <- errors
  }

  valid
}
