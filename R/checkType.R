# !diagnostics suppress=types, custom_types
checkType <- function(varname, env) {
  lazyTyperList <- getFromLazyTyperEnv(varname, env)

  if (!is.null(lazyTyperList)) {
    named <- getNamed(varname, env)
    on.exit(setNamed(varname, named, env))

    setErrorContext("invalidTypeError",
                                base_class = "lazyTyperError")

    do.call(lazyTyperList[["checkTypeFun"]],
            c(x = list(enquote(simpleGet(varname, env))),
              lazyTyperList[["properties"]]))
  } else {
    signal(
      stackError("This is not a typed variable.",
                             "notTypedError",
                             base_class = "lazyTyperError")
    )
  }
}
