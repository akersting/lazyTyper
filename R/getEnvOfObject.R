#' Get the Environment Where an Object is Defined
#'
#' This recursive function returns the environment where the object with a
#' particular name is defined, starting with the search in a given environment.
#' It used R's standard scoping rules, with the exception that it does not
#' recurse beyond the global environment into the packages' namespaces.
#'
#' @param x the name of the object to find as a character string.
#' @param env the environment where to start the search.
#'
#' @return the first environment found where an object with the name \code{x} is
#'   defined, or the empty environment if the search is not successful.
#'
#' @keywords internal
getEnvOfObject <- function(x, env) {
  if (identical(env, globalenv()) || identical(env, emptyenv())) {
    if (exists(x, env, inherits = FALSE))
      return(env)
    else
      return(emptyenv())
  }

  if (exists(x, env, inherits = FALSE)) {
    return(env)
  } else {
    getEnvOfObject(x, parent.env(env))
  }
}

