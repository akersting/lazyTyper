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

    dynamic_properties <- attr(properties, "dynamic_properties")
    if (dynamic_properties) {
      contextualize(
        properties <- sapply(properties, function(x) {
          if (inherits(x, "DynamicProperty")) {
            eval(x, envir = env)
          } else {
            x
          }
        }, simplify = FALSE),
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
      properties <- lapply(properties, enquote)
    }

    setErrorContext("invalidTypeError",
                    base_class = "lazyTyperError")

    do.call(checkTypeFun, c(x = list(enquote(simpleGet(varname, env))),
                            properties), quote = FALSE)
  } else {
    signal(
      stackError("This is not a typed variable.",
                 "notTypedError",
                 base_class = "lazyTyperError")
    )
  }
}
