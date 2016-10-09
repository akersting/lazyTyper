# nocov start
.onLoad <- function(libname, pkgname) {
  types <- new.env(parent = emptyenv())
  assign("types", types, envir = getNamespace("lazyTyper"))

  custom_types <- new.env(parent = emptyenv())
  assign("custom_types", custom_types, envir = getNamespace("lazyTyper"))

  registerType("any", checkPropertiesFun.any, checkTypeFun.any)
  registerType("const", checkPropertiesFun.const, checkTypeFun.const)
  registerType("numeric", checkPropertiesFun.numeric, checkTypeFun.numeric)
  registerType("character", checkPropertiesFun.character,
              checkTypeFun.character)
}
# nocov end
