#' Typed Assignment
#'
#' Assignment operators which perform type checking when modifying a previously
#' \code{\link{declare}}d or \code{\link{cast}}ed variable. An error will be
#' thrown if the assignment violates the variable's type or if the variable is
#' untyped.
#'
#' @param x the name of a typed variable, an expression defining the part of a
#'   typed variable to be replaced (e.g. \code{z[[1]]}) or the call of a
#'   (subsetting) replacement function taking a typed variable as the first
#'   argument (e.g. \code{names(z)} or \code{names(z)[1:3]}).
#' @param value a \emph{call} to \code{.()}.
#' @param rhs the value to be assigned to \code{x}, i.e. an arbitrary (valid)
#'   right hand side of an assignment.
#'
#' @details The right hand side of a typed assignment must be wrapped in
#'   \code{.()}. This is used to "overrule" the high precedence of custom
#'   operators like \code{\%<-\%} and \code{\%<-s\%} (see \link[base]{Syntax})
#'   in an explicit way (compared to using parentheses, which might easily be
#'   forgotton). If used as intended, the function \code{.()} is actually never
#'   really called and it is only included in this package to enable proper
#'   syntax highlighting and code diagnostics in editors/IDEs like RStudio.
#'   Hence, it is probably best to not think of \code{.()} as a separate
#'   function but rather as part of the syntax of typed assignments:
#'
#'   \code{x \%<-\% .(rhs)}\cr \code{x \%<-s\% .(rhs)}
#'
#'   Both operators are lazy in the sense that they first perform the assignment
#'   and only afterwards check if the just created/modified variable is (still)
#'   valid. Hence also the name of the package.
#'
#'   The difference between the two operators is that \code{\%<-s\%} first
#'   creates a backup of the variable to be modified, which it then restores if
#'   the modification invalidated the variable. \code{\%<-\%}, on the other
#'   hand, does not create a backup, such that the variable might be left in an
#'   invalid state after the assignment. The advantage of \code{\%<-\%} over
#'   \code{\%<-s\%} however is that the former allows modification in place
#'   while the latter always has to create a copy of the variable, which for
#'   large objects can have a significant negative impact on performance.
#'
#' @note As base \code{\link{<-}}, \code{\%<-\%} and \code{\%<-s\%} use
#'   positional matching of arguments.
#'
#' @section RStudio Addin: This package ships with addins for RStudio (>=
#'   v0.99.1111) for quickly inserting \code{\%<-\% .()} and \code{\%<-s\% .()}.
#'   Go to Tools > Addins > Browse Addins... > Keyboard Shortcuts... to assign
#'   keyboard shortcuts for this.
#'
#' @return Both operators invisiblty return \code{rhs} evaluated, just as does
#'   base \code{\link{<-}}. Calling \code{.()} always throws an error.
#'
#' @name typedAssignOps
#'
#' @seealso \link{typed}, \code{\link{g}} for securely getting the value of a
#'   typed variable
#'
#' @examples
#' declare(var, "numeric")
#' var %<-% .(c(1,2,3))
#'
#' \dontrun{
#' var %<-% .("Hello World!")  # error: assignment of wrong type}
#' var
#'
#' names(var)[2] %<-% .("two")
#' var
#'
#' \dontrun{
#' class(var) %<-% .("character")  # error: would change type of var
#'
#' a <- 1
#' a %<-% .(2)  # error: a is untyped
#' b %<-% .(9)  # same here}
NULL

#' @usage x \%<-\% value
#' @rdname typedAssignOps
#' @export
`%<-%` <- function(x, value) {
  cl <- sys.call()
  varname <- getLHSVariableName(cl[[2]])

  if (length(cl[[3]]) != 2 || !identical(cl[[3]][[1]], quote(.))) {
    stop("The right hand side of a typed assignment must be a single ",
         "expression wrapped in '.()'.")
  }

  # do the actual assignment
  cl[[1]] <- quote(`<-`)
  cl[3] <- cl[[3]][2]  # replace RHS of assignment with argument of .()
  eval(cl, envir = parent.frame())

  valid <- checkType(varname, env = parent.frame())

  if (!valid) {
    stop("This assignment invalidated the variable '", varname,"'. Reason:\n",
         attr(valid, "error"))
  }

  invisible(NULL)
}

#' @usage x \%<-s\% value
#' @rdname typedAssignOps
#' @export
`%<-s%` <- function(x, value) {
  cl <- sys.call()
  varname <- getLHSVariableName(cl[[2]])

  if (length(cl[[3]]) != 2 || !identical(cl[[3]][[1]], quote(.))) {
    stop("The right hand side of a typed assignment must be a single ",
         "expression wrapped in '.()'.")
  }

  # make a backup of the current value of x if this assignment is not the
  # initialization of x
  init <- !exists(varname, envir = parent.frame(), inherits = FALSE)
  if (!init) {
    eval(parse(text = paste0(".lazyTyper_backup <- ", varname)),
         envir = parent.frame())
    on.exit(base::remove(list = ".lazyTyper_backup", envir = parent.frame()))
  }

  # the actual assignment
  cl[[1]] <- quote(`<-`)
  cl[3] <- cl[[3]][2]  # replace RHS of assignment with argument of .()
  eval(cl, envir = parent.frame())

  valid <- checkType(varname, env = parent.frame())

  if (!valid) {
    # make sure we do not end of with an invalid x if the above assignment
    # was the initialization of x
    base::remove(list = varname, envir = parent.frame())
    if (!init) {
      # restore from backup
      eval(parse(text = paste0(varname, " <- .lazyTyper_backup")),
           envir = parent.frame())
    }
    throwInvalidTypeError("Typed assignment failed for variable '", varname,
                          "'. Reason:\n", attr(valid, "error"))
  }
}

#' @rdname typedAssignOps
#' @export
. <- function(rhs) {
  stop("This function must only be used as the right hand side of a typed ",
       "assignment.")
}
