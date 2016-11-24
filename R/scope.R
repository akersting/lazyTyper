#' Scopes for Limited Variable Lifetime
#'
#' Variables which are \code{\link{declare}}d in a scope are untyped and deleted
#' at the end of it.
#'
#' @param SCOPE an object with the name "SCOPE". It is not evaluated and only
#'   its name matters. Such a dummy object is exported by this package to enable
#'   proper syntax highlighting and code diagnostics in editors/IDEs like
#'   RStudio.
#' @param expr an expression to evaluate in the calling environment of
#'   \code{\%->\%}. Variables \code{\link{declare}}d \emph{directly} herein are
#'   untyped and deleted at the end of the scope.
#'
#' @note Scopes can be nested.
#'
#' @section RStudio Addin: This package ships with an addin for RStudio (>=
#'   v0.99.1111) for quickly inserting \code{SCOPE \%->\% \{\}}. Go to Tools >
#'   Addins > Browse Addins... > Keyboard Shortcuts... to assign a keyboard
#'   shortcut for this.
#'
#' @return whatever \code{expr} returns.
#'
#' @name SCOPE
#'
#' @examples
#' declare(a, "numeric")
#' SCOPE %->% {
#'   declare(b, "numeric")
#'   b <- 2
#'   a %<-% .(b * 3)
#'   d <- 99
#' }
#' a
#' d
#' exists("b")
#' is.typed(b)
NULL

#' @usage SCOPE \%->\% expr
#' @rdname SCOPE
#' @export
`%->%` <- function(SCOPE, expr) {
  if (!substitute(SCOPE) == "SCOPE") {
    stop("Invalid use of '%->%'.")
  }
  env <- environment()
  attr(env, "lazyTyper_scope") <- TRUE
  attr(env, "lazyTyper_vars2remove") <- character()
  on.exit({
    # remove must come before untype since the latter removes vars from
    # vars2remove
    suppressWarnings(base::remove(list = attr(env, "lazyTyper_vars2remove"),
                            envir = parent.frame()))
    suppressWarnings(untype(attr(env, "lazyTyper_vars2remove"),
                            env = parent.frame(), .character = TRUE))
  })

  expr
}

#' @rdname SCOPE
#' @export
SCOPE <- "Use me as the left hand side to the scope operator '%->%'."
