# !diagnostics suppress=types, custom_types
getPathsToVariable <- function(x, varname = ".XXX.", paths = list(),
                               current = integer()) {
  if (is.name(x) && as.character(x) == varname) {
    paths[[length(paths) + 1]] <- current
    return(paths)
  } else if (is.call(x) || is.pairlist(x)) {
    paths <- unlist(lapply(seq_along(x), function(i) {
      getPathsToVariable(x[[i]], varname = varname, paths = paths,
                           current = c(current, i))
    }), recursive = FALSE)
    return(paths)
  } else {
    return(NULL)
  }
}

registerType <- function(type, checkPropertiesFun, check_type_expr,
                         varname = ".XXX.", env = types) {
  stopifnot(is.character(type), length(type) == 1)
  stopifnot(is.function(checkPropertiesFun))
  stopifnot(is.language(check_type_expr))
  stopifnot(is.character(varname), length(varname) == 1)

  assign(type, list(checkPropertiesFun = checkPropertiesFun,
                    check_type_expr = check_type_expr,
                    check_type_expr_paths =
                      getPathsToVariable(check_type_expr, varname)),
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
#'@param check_type_expr a quoted expression to be used for checking whether a
#'  variable is of type \code{type}. Within this expression, refer to the
#'  variable to check by what is specified in the \code{varname}-argument (as a
#'  name, i.e. without the quotes). The default is \code{.XXX.}. The list of
#'  additional properties is available as \code{.lazyTyper_properties}.
#'
#'  This expression should never fail, i.e. throw an error. To indicate that the
#'  variable under consideration is not of type \code{type}, the logical
#'  variable \code{.lazyTyper_valid} should be set to \code{FALSE}. In addition,
#'  the attribute "error" of \code{.lazyTyper_valid} (a character vector),
#'  should describe the reason(s) why the variable is invalid. The preferred way
#'  to both set \code{.lazyTyper_valid} to \code{FALSE} and to add an
#'  (additional) message to its attribute "error", is to call
#'  \code{markInvalidWError}.
#'@param varname a character string with the name of the placeholder-variable
#'  used in \code{check_type_expr} to be replaced with the actual variable to
#'  validate.
#'
#'@seealso useful helper functions: \code{\link{markInvalidWError}},
#'  \code{\link{args2list}}, \code{\link{hasValue}}
#'
#'@export
registerCustomType <- function(type, checkPropertiesFun, check_type_expr,
                               varname = ".XXX.") {
  registerType(type = type, checkPropertiesFun = checkPropertiesFun,
               check_type_expr = check_type_expr, varname = varname,
               env = custom_types)
}
