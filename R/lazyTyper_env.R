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
  lazyTyper_env <- get0(".lazyTyper_env", envir = env, inherits = FALSE,
                        ifnotfound = NULL)
  if (is.null(lazyTyper_env)) {
    lazyTyper_env <- new.env(parent = emptyenv())
    assign(".lazyTyper_env", lazyTyper_env, envir = env)
  }

  dynamic_properties <- any(unlist(lapply(properties, is.language)))
  properties <- checkProperties(type, properties,
                                skip_check = dynamic_properties)
  checkPropertiesFun <- attr(properties, "checkPropertiesFun")
  attr(properties, "checkPropertiesFun") <- NULL

  checkTypeFun <- getCheckFun(type, "checkTypeFun")

  value <- list(type = type, properties = properties,
                checkPropertiesFun = checkPropertiesFun,
                checkTypeFun = checkTypeFun)

  assign(x, value, envir = lazyTyper_env)
}

removeFromLazyTyperEnv <- function(x, env) {
  lazyTyper_env <- get0(".lazyTyper_env", envir = env, inherits = FALSE,
                        ifnotfound = NULL)
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
