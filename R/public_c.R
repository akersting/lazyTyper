#' Does an Argument (or Another Object) Exist and have a Value?
#'
#' This function returns \code{FALSE} if an object with a given name does not
#' exist or if it is identical to the empty name, which is used to represent
#' \emph{missing arguments} to a function \emph{without a default value}.
#' Otherwise it returns \code{TRUE}.
#'
#' @param x a character vector with the names of the objects to test.
#' @param env the environment of the objects to test. Defaults to the calling
#'   frame of this function.
#'
#' @note If an object is a promise \code{TRUE} is returned independently of the
#'   value of the promise.
#'
#' @return a named logical vector.
#'
#' @seealso \code{\link[base]{exists}}, which returns \code{TRUE} also for
#'   missing arguments and \code{\link[base]{missing}}, which returns
#'   \code{TRUE} also for missing arguments which have a default value.
#'
#' @keywords internal
#' @export
hasValue <- function(x, env = parent.frame()) {
  .Call(C_hasValue, x = x, env = env)
}

#' Enhanced Mode/Type Determination
#'
#' Get the high-level mode/type of an object.
#'
#' @param x any R object.
#'
#' @return
#' \describe{
#'   \item{"ordered"}{for ordered factors}
#'   \item{"factor"}{for (unordered) factors}
#'   \item{"data.frame"}{for lists which inherit from "data.frame"}
#'   \item{"\emph{mode}Matrix"}{for matrices, where \emph{mode} is
#'                              \code{mode(x)}}
#'   \item{"\emph{mode}Array"}{for (other) arrays, where \emph{mode} is
#'                             \code{mode(x)}}
#'   \item{"namespace"}{for namespaces}
#'   \item{typeof(x)}{for everything else; types "integer" and "double" are
#'                    returned as "numeric" and types "special" and "builtin"
#'                    are returned as "function"}
#' }
#'
#' @export
iMode <- function(x) {
  .Call(C_iMode, x = x)
}
