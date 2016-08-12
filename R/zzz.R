# nocov start
.onLoad <- function(libname, pkgname) {
  types <- new.env(parent = emptyenv())
  assign("types", types, envir = getNamespace("lazyTyper"))

  custom_types <- new.env(parent = emptyenv())
  assign("custom_types", custom_types, envir = getNamespace("lazyTyper"))

  registerType("any", checkPropertiesFun.any, check_type_expr.any)
  registerType("const", checkPropertiesFun.const, check_type_expr.const)
  registerType("numeric", checkPropertiesFun.numeric, check_type_expr.numeric)
  registerType("character", checkPropertiesFun.character,
               check_type_expr.character)
}
# nocov end
