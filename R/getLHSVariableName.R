#' Get Variable Name From Left-Hand Side of Assignment
#'
#' \code{getLHSVariableName} is a recursive function which returns the name of
#' the variable which would be created or modified by a \emph{valid} assignment.
#'
#' @param x the left-hand side of an assignment as a language object.
#' @param allow_character a logical value indicating if \code{"abc"} or even
#'   \code{"123"} (but not \code{""}) are acceptable left-hand sides.
#'   \code{TRUE} mimics the behavior of the base assignment operator.
#'
#' @return a character string.
#'
#' @keywords internal
#'
#' @examples
#' assignment <- quote(names(var)[1:3] <- letters[1:3])
#' lhs <- assignment[[2]]  # left-hand side of assignment
#' lazyTyper:::getLHSVariableName(lhs)  # "var"
getLHSVariableName <- function(x, allow_character = TRUE) {
  if (is.name(x)) {
    return(as.character(x))
  } else if (is.call(x) && length(x) > 1 && is.name(x[[2]])) {
    return(as.character(x[[2]]))
  } else if (is.call(x) && (identical(x[[1]], quote(`[`)) ||
                            identical(x[[1]], quote(`[[`)) ||
                            identical(x[[1]], quote(`$`)))) {
    return(getLHSVariableName(x[[2]], allow_character = FALSE))
  } else if (allow_character && is.character(x) && nchar(x) > 0) {
    return(x)
  } else {
    stop("Invalid left-hand side to assignment.")
  }
}
