# !diagnostics suppress=types, custom_types
checkType <- function(varname, env) {
  lazyTyperList <- getFromLazyTyperEnv(varname, env)

  if (!is.null(lazyTyperList)) {
    named <- getNamed(varname, env)
    on.exit(setNamed(varname, env, named))

    conditionR::setErrorContext("invalidTypeError",
                                base_class = "lazyTyperError")

    do.call(lazyTyperList[["checkTypeFun"]],
            c(x = list(get(varname, envir = env, inherits = FALSE)),
              lazyTyperList[["properties"]]))
  } else {
    conditionR::signal(
      conditionR::stackError("This is not a typed variable.",
                             "notTypedError",
                             base_class = "lazyTyperError")
    )
  }
}

getCheckTypeFun <- function(type) {

  checkTypeFun <- get0(type, envir = custom_types,
                          ifnotfound = NULL)[["checkTypeFun"]]
  if (is.null(checkTypeFun)) {
    checkTypeFun <- get0(type, envir = types,
                            ifnotfound = NULL)[["checkTypeFun"]]
    if (is.null(checkTypeFun)) {
      # we should actually never end here since getCheckPropertiesFun() is
      # always called first
      stop("Neither a built-in nor a registered custom type: ", type)
    }
  }

  checkTypeFun
}
