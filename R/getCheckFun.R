getCheckFun <- function(type, fun_type, find_hidden = FALSE) {
  fun_type <- match.arg(fun_type, c("checkPropertiesFun", "checkTypeFun"))

  # set env to the parent frame of the highest level call to a lazyTyper
  # function (e.g. to the parent frame of the call to declare, const or
  # registerCustomAlias)
  n <- 1
  repeat {
    env <- parent.frame(n)
    if (!identical(parent.env(env), getNamespace("lazyTyper"))) {
      break
    }
    n <- n + 1
  }

  # search for a custom types env which contains type; stop the search at the
  # first namespace or at the global env
  repeat {
    custom_types <- get0(".lazyTyper_custom_types", envir = env,
                         inherits = FALSE, ifnotfound = emptyenv())
    checkFun <- get0(type, envir = custom_types)[[fun_type]]

    if (!is.null(checkFun) || isNamespace(env) || identical(env, .GlobalEnv)) {
      break
    }
    env <- parent.env(env)
  }

  # maybe it is a built-in type?
  if (is.null(checkFun)) {
    checkFun <- get0(type, envir = types)[[fun_type]]
  }
  if (is.null(checkFun) || (!find_hidden && substr(type, 1, 1) == ".")) {
    signal(
      stackError(
        paste0("Neither a built-in nor a registered custom type: ", type),
        "noTypeError",
        base_class = "lazyTyperError"
      )
    )
  }

  checkFun
}
