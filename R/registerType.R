# !diagnostics suppress=types, custom_types
registerType <- function(type, checkPropertiesFun, checkTypeFun,
                         env = types) {
  stopifnot(is.character(type), length(type) == 1)
  stopifnot(is.function(checkPropertiesFun))
  stopifnot(is.function(checkTypeFun))

  environment(checkPropertiesFun) <-
    new.env(parent = environment(checkPropertiesFun))

  assign(type, list(checkPropertiesFun = checkPropertiesFun,
                    checkTypeFun = checkTypeFun),
         envir = env)
}

#' @importFrom compiler cmpfun
registerAlias <- function(alias, type, fixed, defaults, env = types) {
  stopifnot(is.character(alias), length(alias) == 1)
  stopifnot(is.character(type), length(type) == 1)
  stopifnot(alias != type)
  stopifnot(is.list(defaults))
  stopifnot(is.list(defaults))

  checkPropertiesFun <- getCheckFun(type, "checkPropertiesFun",
                                    find_hidden = TRUE)
  formals_properties <- formals(checkPropertiesFun)
  checkTypeFun <- getCheckFun(type, "checkTypeFun", find_hidden = TRUE)
  formals_type <- formals(checkTypeFun)

  if (length(fixed) > 0 &&
      (is.null(names(fixed)) ||
       length(unique(names(fixed))) != length(fixed)) ||
       any(!names(fixed) %in% names(formals_properties))) {
    stop("'fixed' must be a named list whose names correspond to the ",
         "non-fixed properties of type 'type'.")
  }

  if (length(defaults) > 0 &&
      (is.null(names(defaults)) ||
       length(unique(names(defaults))) != length(defaults)) ||
       any(!names(defaults) %in% names(formals_properties))) {
    stop("'defaults' must be a named list whose names correspond to the ",
         "non-fixed properties of type 'type'.")
  }

  if (any(names(defaults) %in% names(fixed))) {
    stop("defaults in fixed")
  }

  fixed_previous <- as.list(environment(checkPropertiesFun), all.names = TRUE)
  checkPropertiesFun_env <-
    list2env(fixed_previous,
             parent = parent.env(environment(checkPropertiesFun)))
  list2env(fixed, envir = checkPropertiesFun_env)

  formals_properties <-
    formals_properties[!names(formals_properties) %in% names(fixed)]
  formals_properties[names(defaults)] <- defaults
  formals(checkPropertiesFun,
          envir = checkPropertiesFun_env) <- formals_properties

  formals_type[names(fixed)] <- fixed
  formals(checkTypeFun) <- formals_type

  checkPropertiesFun <- compiler::cmpfun(checkPropertiesFun)
  checkTypeFun <- compiler::cmpfun(checkTypeFun)

  registerType(alias, checkPropertiesFun, checkTypeFun, env = env)
}

#'Register Custom Types
#'
#'@param type a character string with the name of the type to register (for
#'  \code{registerCustomType}) or with the name of the type from which the alias
#'  should inherit (for \code{registerCustomAlias}).
#'@param checkPropertiesFun a function to check the validity of the additional
#'  properties passed to either declare or cast when creating a typed variable.
#'  This function must accept these additional properties as (named) parameters
#'  and it should throw an error if properties used are invalid. Afterwards, it
#'  must return -- as a (named) list -- the set of properties which should
#'  finally be stored. This set can be different from the original arguments to
#'  this function, e.g. because one of two inconsistent properties was removed.
#'  There is the helper function \code{\link{args2list}}, which returns the
#'  \emph{current} values of the arguments of the calling function as a list.
#'@param checkTypeFun a function used for checking whether a variable is of type
#'  \code{type}. This function must accept \code{x}, which is the variable to
#'  test, as well as all the additional properties supported by this type as
#'  (named) parameters.
#'
#'@seealso useful helper functions: \code{\link{args2list}},
#'  \code{\link{hasValue}}
#'
#'@export
registerCustomType <- function(type, checkPropertiesFun, checkTypeFun) {

  custom_types <- get0(".lazyTyper_custom_types", inherits = FALSE,
                       envir = parent.frame())
  if (is.null(custom_types)) {
    custom_types <- new.env(parent = emptyenv())
    assign(".lazyTyper_custom_types", custom_types, envir = parent.frame())
  }

  registerType(type = type, checkPropertiesFun = checkPropertiesFun,
               checkTypeFun = checkTypeFun, env = custom_types)
}

#' @rdname registerCustomType
#'
#' @param alias a character string with the name of the alias to register.
#' @param defaults a (possibly empty) named (pair) list of properties of the
#'   type \code{type}. This changes the default values of the respective
#'   properties.
#' @param fixed a (possibly empty) named (pair) list of properties of the type
#'   \code{type} (distinct from \code{defaults}. Properties set here are fixed
#'   for the alias and can no longer be set when using the type \code{alias}.
#'
#' @examples
#' # type "myScalar" is equivalent to the (base) type "numeric" with the
#' # property "length" fixed to 1 and the properties "min_length" and
#' # "max_length" fixed to the missing argument
#' registerCustomAlias("myScalar", type = "numeric",
#'                     defaults = alist(length = 1, min_length = ,
#'                     max_length = ))
#'
#' # alternative way to fix properties to the missing argument
#' registerCustomAlias("myScalar", type = "numeric",
#'                     defaults = list(length = 1, min_length = quote(expr = ),
#'                     max_length = quote(expr = )))
#'
#' \dontrun{
#' # error since min_length is fixed and hence connot be set
#' declare(a, "myScalar", min_length = 1)}
#'
#' @export
registerCustomAlias <- function(alias, type, fixed = list(),
                                defaults = list()) {

  custom_types <- get0(".lazyTyper_custom_types", inherits = FALSE,
                       envir = parent.frame())
  if (is.null(custom_types)) {
    custom_types <- new.env(parent = emptyenv())
    assign(".lazyTyper_custom_types", custom_types, envir = parent.frame())
  }

  registerAlias(alias, type, fixed, defaults, env = custom_types)
}
