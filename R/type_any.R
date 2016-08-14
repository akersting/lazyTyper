#' Any -- a "Fits-All" Type
#'
#' This type does not accept any additional arguments passed as \code{...} to
#' \code{\link{declare}} and \code{\link{cast}}.
#'
#' @details Any R object is a valid instance of this type.
#'
#' @name any
#' @aliases any
#' @family types
NULL

checkPropertiesFun.any <- function() {
  args2list()
}

check_type_expr.any <- quote({})
