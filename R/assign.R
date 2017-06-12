#' Typed Assignment
#'
#' Assignment operators which perform type checking when modifying a previously
#' \code{\link{declare}}d or \code{\link{cast}}ed variable. An error will be
#' thrown if the assignment violates the variable's type or if the variable is
#' untyped.
#'
#' @param x an arbitrary (valid) left hand side of an assignment implying the
#'   creation/modification of a \emph{typed} variable, i.e. typically the name
#'   of a typed variable, an expression defining the part of a typed variable to
#'   be replaced (e.g. \code{z[[1]]}), the call of a (subsetting) replacement
#'   function taking a typed variable as the first argument (e.g.
#'   \code{names(z)} or \code{names(z)[1:3]}). More uncommon LHSs like
#'   \code{"z"}, \code{is.na(z[1:3])} or \code{f(g(h(z))[1:3])} are also
#'   supported.
#' @param value a \emph{call} to \code{.()}.
#' @param rhs the value to be assigned to \code{x}, i.e. an arbitrary (valid)
#'   right hand side of an assignment.
#'
#' @details The right hand side of a typed assignment must be wrapped in
#'   \code{.()}. This is used to "overrule" the high precedence of custom
#'   operators like \code{\%<-\%} and \code{\%<-s\%} (see \link[base]{Syntax})
#'   in an explicit way (compared to using parentheses, which might easily be
#'   forgotten). If used as intended, the function \code{.()} is actually never
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
#' @section Options: If the \emph{local} variable
#'   \code{.lazyTyper_warning2error} exists and \code{\link[base]{isTRUE}}, then
#'   the assignment will fail if the base assignment operator, which is used
#'   internally for the actual assignment, issues a warning, e.g. the famous
#'   "number of items to replace is not a multiple of replacement length".
#'   \emph{Local} here means in the environment of the assignment, or in an
#'   enclosing environment up to (and including) the first namespace or the
#'   global environment. If \emph{additionally} the local variable
#'   \code{.lazyTyper_hard_bounds} \code{\link[base]{isTRUE}} then the
#'   \code{\link[base]{option}} "check.bounds" is enabled for the time of this
#'   assignment and hence extending a vector (atomic or list) by something like
#'   \code{x <- 1:3; x[5] \%<-\% .(6)} will fail.
#'
#' @note As base \code{\link{<-}}, \code{\%<-\%} and \code{\%<-s\%} use
#'   positional matching of arguments.
#'
#' @section RStudio Addin: This package ships with addins for RStudio (>=
#'   v0.99.1111) for quickly inserting \code{\%<-\% .()} and \code{\%<-s\% .()}.
#'   Go to Tools > Addins > Browse Addins... > Keyboard Shortcuts... to assign
#'   keyboard shortcuts for this.
#'
#' @return Both operators invisibly return \code{rhs} evaluated, just as does
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

  # rules out usage as 'what' in do.call() or 'FUN' in *apply()
  if (!(identical(cl[[1]], quote(`%<-%`)) ||
        identical(cl[[1]], call("::", as.name("lazyTyper"), as.name("%<-%"))) ||
        identical(cl[[1]],
                  call(":::", as.name("lazyTyper"), as.name("%<-%")))) ||
      length(cl) != 3) {
    stop("Invalid usage of typed assignment operator.")
  }

  varname <- getLHSVariableName(cl[[2]])

  if (length(cl[[3]]) != 2 || !identical(cl[[3]][[1]], quote(.))) {
    stop("The right hand side of a typed assignment must be a single ",
         "expression wrapped in '.()'.")
  }


  # do the actual assignment ---------------------------------------------------
  # replace %<-% with <-
  cl[[1]] <- quote(`<-`)

  warning2error <- isTRUE(
    getLocal0(".lazyTyper_warning2error", parent.frame(), FALSE)
  )
  hard_bounds <- isTRUE(
    getLocal0(".lazyTyper_hard_bounds", parent.frame(), FALSE)
  )
  if (hard_bounds & !warning2error) {
    signal(stackWarning(
      paste0("'hard bounds checking' is enabled but not effective since ",
             "'warning2error' is disabled."),
      "hardBoundsNotEffectiveWarning",
      "lazyTyperWarning"
    ))
  }

  if (!warning2error) {
    # default case
    cl[3] <- cl[[3]][2]  # RHS is wrapped in .()
    eval2Reference(cl, reference_name = "rhs", eval_env = parent.frame())
  } else {
    # warning2error (and hard_bounds) must only be effective for the actual
    # assignment but not for the evaluation of the RHS; that is why we do the
    # latter seperately here:
    rhs <- cl[[3]][[2]]
    eval2Reference(rhs, reference_name = "rhs", eval_env = parent.frame())

    # we do not place the evaluated RHS directly in cl but rather a call to
    # simpleGet; this ensures that we do not increase NAMED unnecessarily
    cl[[3]][[1]] <- call(":::", quote(lazyTyper), quote(simpleGet))
    cl[[3]][[2]] <- "rhs"
    cl[[3]][[3]] <- environment()

    opt_bak <- options(check.bounds = hard_bounds)
    on.exit(options(opt_bak))

    # don't use tryCatch here since we want to complete the assignment even if
    # there is a warning
    warning_obj <- NULL
    withCallingHandlers(
      eval2Reference(cl, reference_name = "rhs", eval_env = parent.frame()),
      warning = function(w) {
        warning_obj <<- w
        invokeRestart("muffleWarning")
      }
    )
    options(opt_bak)
    on.exit()

    if (!is.null(warning_obj)) {
      signal(stackError(
        message = paste0("A warning occured during the assignment. | ",
                         conditionMessage(warning_obj)),
        class = c("typedAssignmentError", "assignmentWarningError"),
        base_class = "lazyTyperError"
      ))
    }
  }

  # do the type checking -------------------------------------------------------
  setErrorContext(
    "typedAssignmentError",
    c(
      notTypedError = paste0("Cannot check type of variable '",
                             varname, "' after assignment."),
      dynamicPropertiesError = paste0("Cannot check type of variable '",
                                      varname, "' after assignment."),
      invalidTypeError = paste0("This assignment invalidated the variable '",
                                varname, "'.")
    ), base_class = "lazyTyperError"
  )
  checkType(varname, env = parent.frame())

  # rhs was set by eval2Reference()
  # we must use simpleGet() since rhs might be the empty name
  invisible(simpleGet("rhs"))
}

#' @usage x \%<-s\% value
#' @rdname typedAssignOps
#' @export
`%<-s%` <- function(x, value) {
  cl <- sys.call()
  parent_frame <- parent.frame()

  # rules out usage as 'what' in do.call() or 'FUN' in *apply()
  if (!(identical(cl[[1]], quote(`%<-s%`)) ||
        identical(cl[[1]],
                  call("::", as.name("lazyTyper"), as.name("%<-s%"))) ||
        identical(cl[[1]],
                  call(":::", as.name("lazyTyper"), as.name("%<-s%")))) ||
      length(cl) != 3) {
    stop("Invalid usage of typed assignment operator.")
  }

  varname <- getLHSVariableName(cl[[2]])

  if (length(cl[[3]]) != 2 || !identical(cl[[3]][[1]], quote(.))) {
    stop("The right hand side of a typed assignment must be a single ",
         "expression wrapped in '.()'.")
  }

  # make a backup of the current value of x if this assignment is not the
  # initialization of x
  init <- !exists(varname, envir = parent_frame, inherits = FALSE)
  if (!init) {
    backup <- get(varname, envir = parent_frame)
  }

  # do the actual assignment ---------------------------------------------------
  # replace %<-s% with <-
  cl[[1]] <- quote(`<-`)

  warning2error <- isTRUE(
    getLocal0(".lazyTyper_warning2error", parent.frame(), FALSE)
  )
  hard_bounds <- isTRUE(
    getLocal0(".lazyTyper_hard_bounds", parent.frame(), FALSE)
  )
  if (hard_bounds & !warning2error) {
    signal(stackWarning(
      paste0("'hard bounds checking' is enabled but not effective since ",
             "'warning2error' is disabled."),
      "hardBoundsNotEffectiveWarning",
      "lazyTyperWarning"
    ))
  }

  # see %<-% for more comments
  if (!warning2error) {
    cl[3] <- cl[[3]][2]  # RHS is wrapped in .()
    eval2Reference(cl, reference_name = "rhs", eval_env = parent.frame())
  } else {
    rhs <- cl[[3]][[2]]
    eval2Reference(rhs, reference_name = "rhs", eval_env = parent.frame())
    cl[[3]][[1]] <- call(":::", quote(lazyTyper), quote(simpleGet))
    cl[[3]][[2]] <- "rhs"
    cl[[3]][[3]] <- environment()

    opt_bak <- options(check.bounds = hard_bounds)
    on.exit(options(opt_bak))

    # use tryCatch here since we do not want to complete the assignment if there
    # is a warning; eval2Reference returns NULL
    warning_obj <- tryCatch(
      eval2Reference(cl, reference_name = "rhs", eval_env = parent.frame()),
      warning = function(w) {
        w
      }
    )
    options(opt_bak)
    on.exit()

    if (!is.null(warning_obj)) {
      signal(stackError(
        message = paste0("A warning occured during the assignment. | ",
                         conditionMessage(warning_obj)),
        class = c("typedAssignmentError", "assignmentWarningError"),
        base_class = "lazyTyperError"
      ))
    }
  }


  # do the type checking -------------------------------------------------------
  setErrorContext(
    "typedAssignmentError",
    c(
      notTypedError = paste0("This assignment is invalid."),
      dynamicPropertiesError = paste0("This assignment is invalid."),
      invalidTypeError = paste0("This assignment would invalidate the ",
                                "variable '", varname, "'.")
    ), base_class = "lazyTyperError"
  )

  withCallingHandlers(
    checkType(varname, env = parent_frame),
    lazyTyperError = function(e) {
      # make sure we do not end of with an invalid x if the above assignment
      # was the initialization of x
      base::remove(list = varname, envir = parent_frame)
      if (!init) {
        # restore from backup
        assign(varname, backup, envir = parent_frame)
      }
    }
  )

  # rhs was set by eval2Reference()
  # we must use simpleGet() since rhs might be the empty name
  invisible(simpleGet("rhs"))
}

#' @rdname typedAssignOps
#' @export
. <- function(rhs) {
  stop("This function must only be used as the right hand side of a typed ",
       "assignment.")
}
