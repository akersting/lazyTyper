# !diagnostics suppress=types, custom_types
checkType <- function(varname, env) {
  lazyTyperList <- getFromLazyTyperEnv(varname, env)

  if (!is.null(lazyTyperList)) {
    named <- getNamed(varname, env)
    on.exit(setNamed(varname, named, env))

    type <- lazyTyperList[["type"]]
    properties <- lazyTyperList[["properties"]]
    checkPropertiesFun <- lazyTyperList[["checkPropertiesFun"]]
    checkTypeFun <- lazyTyperList[["checkTypeFun"]]

    dynamic_properties <- any(unlist(lapply(properties, is.language)))
    if (dynamic_properties) {
      contextualize(
        properties <- sapply(properties, eval, envir = env, simplify = FALSE),
        error = list(
          class = "dynamicPropertiesError",
          message = "Error during the evaluation of the dynamic properties.",
          base_class = "lazyTyperError")
      )

      setErrorContext(
        c("dynamicPropertiesError", "invalidPropertyError"),
        paste0("Invalid dynamic property specification for type '", type, "'."),
        base_class = "lazyTyperError"
      )
      properties <- do.call(checkPropertiesFun, properties, quote = TRUE)
    }

    setErrorContext("invalidTypeError",
                    base_class = "lazyTyperError")

    do.call(checkTypeFun, c(x = list(simpleGet(varname, env)),
                            properties), quote = TRUE)
  } else {
    signal(
      stackError("This is not a typed variable.",
                 "notTypedError",
                 base_class = "lazyTyperError")
    )
  }
}
