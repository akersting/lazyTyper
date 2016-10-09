# !diagnostics suppress=types, custom_types
checkType <- function(varname, env) {
  lazyTyperList <- getFromLazyTyperEnv(varname, env)

  if (!is.null(lazyTyperList)) {
    named <- getNamed(varname, env)
    on.exit(setNamed(varname, env, named))

    valid <- TRUE
    do.call(lazyTyperList[["checkTypeFun"]],
            c(x = list(get(varname, envir = env, inherits = FALSE)),
              lazyTyperList[["properties"]]))
    valid
  } else {
    valid <- FALSE
    attr(valid, "error") <- "not a typed variable"
    valid
  }
}

getCheckTypeFun <- function(type) {

  checkTypeFun <- get0(type, envir = custom_types,
                          ifnotfound = NULL)[["checkTypeFun"]]
  if (is.null(checkTypeFun)) {
    checkTypeFun <- get0(type, envir = types,
                            ifnotfound = NULL)[["checkTypeFun"]]
    if (is.null(checkTypeFun)) {
      # we should actually never end here since getCheckPropertiesFun() is
      # always called first
      stop("Neither a built-in nor a registered custom type: ", type)
    }
  }

  checkTypeFun
}

#' Mark the Variable Currently Under Validation as Invalid
#'
#' @param ... one or more character strings to be pasted together to an error
#'   message (without a separator), which describes why a variable is not of the
#'   declared type.
#'
#' @keywords internal
#' @export
markInvalidWError <- function(...) {
  valid <- get("valid", envir = parent.frame(2))

  msg <- paste0(...)

  valid[1] <- FALSE
  attr(valid, "error") <- c(attr(valid, "error"), msg)

  assign("valid", valid, envir = parent.frame(2))
}
