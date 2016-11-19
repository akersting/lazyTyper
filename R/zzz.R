# nocov start
types <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  registerType("any", checkPropertiesFun.any, checkTypeFun.any)

  registerType("const", checkPropertiesFun.const, checkTypeFun.const)

  registerType(".vector", checkPropertiesFun.vector, checkTypeFun.vector)

  registerAlias("numeric", ".vector",
                fixed = alist(type = "numeric", pattern = ),
                defaults = list())

  registerAlias("character", ".vector",
                fixed = alist(type = "character", min = , max = ,
                              whole = , allow_NaN = ),
                defaults = list())

  registerAlias("logical", ".vector",
                fixed = alist(type = "logical", min = , max = ,
                              whole = , set = , pattern = , allow_NaN = ),
                defaults = list())

  registerAlias("integer", "numeric",
                fixed = alist(set = , whole = TRUE),
                defaults = list())

  registerAlias("count", "numeric",
                fixed = alist(set = , min = 0, whole = TRUE),
                defaults = list())

  registerAlias("scalar", "numeric",
                fixed = alist(length = 1, min_length = , max_length = ),
                defaults = alist(allow_NA = FALSE, allow_NaN = FALSE))

  registerAlias("string", "character",
                fixed = alist(length = 1, min_length = , max_length = ),
                defaults = alist(allow_NA = FALSE))
}
# nocov end
