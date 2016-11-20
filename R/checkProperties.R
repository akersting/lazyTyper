# !diagnostics suppress=types, custom_types
checkProperties <- function(type, properties) {
  checkPropertiesFun <- getCheckFun(type, "checkPropertiesFun")

  setErrorContext(
    "invalidPropertyError",
    paste0("Invalid property specification for type '", type, "'."),
    base_class = "lazyTyperError"
  )

  contextualize(
    match.call(checkPropertiesFun, as.call(c("dummyFUN", properties)),
               expand.dots = FALSE),
    error = list(message = "Unsupported properties set.",
                 class = c("unsupportedPropertyError"),
                 base_class = "lazyTyperError")
  )

  return(do.call(checkPropertiesFun, properties))
}
