# !diagnostics suppress=types, custom_types
checkProperties <- function(type, properties) {
  checkPropertiesFun <- getCheckPropertiesFun(type)

  conditionR::contextualize(
    match.call(checkPropertiesFun, as.call(c("dummyFUN", properties)),
               expand.dots = FALSE),
    error = list(message = paste0("Invalid properties for type '", type, "'."),
                 class = c("invalidPropertyError"),
                 base_class = "lazyTyperError")
  )

  return(do.call(checkPropertiesFun, properties))
}

getCheckPropertiesFun <- function(type, find_hidden = FALSE) {
  stopifnot(is.character(type), length(type) == 1)

  if (!find_hidden && substr(type, 1, 1) == ".") {
    stop("Neither a built-in nor a registered custom type: ", type)
  }

  checkPropertiesFun <-
    get0(type, envir = custom_types)[["checkPropertiesFun"]]
  if (is.null(checkPropertiesFun)) {
    checkPropertiesFun <-
      get0(type, envir = types)[["checkPropertiesFun"]]
  }
  if (is.null(checkPropertiesFun)) {
    stop("Neither a built-in nor a registered custom type: ", type)
  }

  return(checkPropertiesFun)
}
