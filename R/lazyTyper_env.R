# lazyTyper stores the type information in the environment 'lazyTyper_env' as an
# attribute to the environment of the respective object; this file contains
# helper functions for managing these environments

existsInLazyTyperEnv <- function(x, env) {
  lazyTyper_env <- attr(env, "lazyTyper_env", exact = TRUE)
  if (is.null(lazyTyper_env)) {
    FALSE
  } else {
    exists(x, envir = lazyTyper_env)
  }
}

getFromLazyTyperEnv <- function(x, env) {
  lazyTyper_env <- attr(env, "lazyTyper_env", exact = TRUE)
  if (is.null(lazyTyper_env)) {
    NULL
  } else {
    get0(x, envir = lazyTyper_env, inherits = FALSE, ifnotfound = NULL)
  }
}

assignToLazyTyperEnv <- function(x, type, properties, env) {
  lazyTyper_env <- attr(env, "lazyTyper_env", exact = TRUE)
  if (is.null(lazyTyper_env)) {
    lazyTyper_env <- attr(env, "lazyTyper_env") <- new.env(parent = emptyenv())
  }

  properties <- checkProperties(type, properties)
  checkTypeFun <- getCheckFun(type, "checkTypeFun")

  value <- list(type = type, properties = properties,
                checkTypeFun = checkTypeFun)

  assign(x, value, envir = lazyTyper_env)
}

removeFromLazyTyperEnv <- function(x, env) {
  lazyTyper_env <- attr(env, "lazyTyper_env", exact = TRUE)
  if (!is.null(lazyTyper_env)) {
    if (existsInLazyTyperEnv(x, env)) {
      base::remove(list = x, envir = lazyTyper_env)
    } else {
      warning("Variable '", x, "' was not typed.", call. = FALSE)
    }
  } else {
    warning("Variable '", x, "' was not typed.", call. = FALSE)
  }
}
