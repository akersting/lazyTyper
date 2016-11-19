# !diagnostics suppress=types, custom_types
checkProperties <- function(type, properties) {
  checkPropertiesFun <- getCheckFun(type, "checkPropertiesFun")

  contextualize(
    match.call(checkPropertiesFun, as.call(c("dummyFUN", properties)),
               expand.dots = FALSE),
    error = list(message = paste0("Invalid properties for type '", type, "'."),
                 class = c("invalidPropertyError"),
                 base_class = "lazyTyperError")
  )

  return(do.call(checkPropertiesFun, properties))
}
