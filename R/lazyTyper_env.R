# lazyTyper stores the type information in the environment '.lazyTyper_env'
# underneath the environment of the respective object; this file contains helper
# functions for managing these environments

existsInLazyTyperEnv <- function(x, env) {
  lazyTyper_env <- get0(".lazyTyper_env", envir = env, inherits = FALSE,
                        ifnotfound = emptyenv())
  if (exists(x, envir = lazyTyper_env)) {
    TRUE
  } else {
    FALSE
  }
}

getFromLazyTyperEnv <- function(x, env) {
  lazyTyper_env <- get0(".lazyTyper_env", envir = env, inherits = FALSE,
                       ifnotfound = emptyenv())
  get0(x, envir = lazyTyper_env, inherits = FALSE, ifnotfound = NULL)
}

assignToLazyTyperEnv <- function(x, type, properties, env) {
  lazyTyper_env <- get0(".lazyTyper_env", envir = env, inherits = FALSE)
  if (is.null(lazyTyper_env)) {
    assign(".lazyTyper_env", new.env(parent = emptyenv()), envir = env)
  }
  lazyTyper_env <- get0(".lazyTyper_env", envir = env)

  properties <- checkProperties(type, properties)
  checkTypeFun <- getCheckFun(type, "checkTypeFun")

  value <- list(type = type, properties = properties,
                checkTypeFun = checkTypeFun)

  assign(x, value, envir = lazyTyper_env)
}

removeFromLazyTyperEnv <- function(x, env) {
  lazyTyper_env <- get0(".lazyTyper_env", envir = env, inherits = FALSE)
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
