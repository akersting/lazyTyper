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
