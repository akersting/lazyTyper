# !diagnostics suppress=types, custom_types
checkProperties <- function(type, properties, skip_check = FALSE) {
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

  if (!skip_check) {
    properties <- do.call(checkPropertiesFun, properties)
  }
  attr(properties, "checkPropertiesFun") <- checkPropertiesFun

  properties
}
