# !diagnostics suppress=types, custom_types
registerType <- function(type, checkPropertiesFun, checkTypeFun,
                         env = types) {
  stopifnot(is.character(type), length(type) == 1)
  stopifnot(is.function(checkPropertiesFun))
  stopifnot(is.function(checkTypeFun))

  assign(type, list(checkPropertiesFun = checkPropertiesFun,
                    checkTypeFun = checkTypeFun),
         envir = env)
}

#'Register Custom Types
#'
#'@param type a character string with the name of the type to register.
#'@param checkPropertiesFun a function to check the validity of the additional
#'  properties passed to either declare or cast when creating a typed variable.
#'  This function must accept these additional properties as (named) parameters
#'  and it should throw an error if properties used are invalid. Afterwards, it
#'  must return -- as a (named) list -- the set of properties which should
#'  finally be stored. This set can be different from the original arguments to
#'  this function, e.g. because one of two inconsistent properties was removed.
#'  There is the helper function \code{\link{args2list}}, which returns the
#'  *current* values of the arguments of the calling function as a list.
#'@param checkTypeFun a function used for checking whether a variable is of type
#'  \code{type}. This function must accept \code{x}, which is the variable to
#'  test, as well as all the additional properties supported by this type as
#'  (named) parameters.
#'
#'@seealso useful helper functions: \code{\link{markInvalidWError}},
#'  \code{\link{args2list}}, \code{\link{hasValue}}
#'
#'@export
registerCustomType <- function(type, checkPropertiesFun, checkTypeFun) {
  registerType(type = type, checkPropertiesFun = checkPropertiesFun,
               checkTypeFun = checkTypeFun, env = custom_types)
}
