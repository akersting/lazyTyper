# !diagnostics suppress=types, custom_types
checkProperties <- function(type, properties) {
  checkPropertiesNames(properties)

  checkPropertiesFun <- getCheckPropertiesFun(type)

  checkPropertiesFun(properties)
}

getCheckPropertiesFun <- function(type) {
  stopifnot(is.character(type), length(type) == 1)

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
