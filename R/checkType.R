# !diagnostics suppress=types, custom_types
checkType <- function(varname, env) {
  lazyTyperList <- getFromLazyTyperEnv(varname, env)

  if (!is.null(lazyTyperList)) {
    assign(".lazyTyper_properties", lazyTyperList[["properties"]], envir = env)
    assign(".lazyTyper_valid", TRUE, envir = env)

    eval(lazyTyperList[["check_type_expr"]], envir = env)

    get(".lazyTyper_valid", envir = env)
  } else {
    valid <- FALSE
    attr(valid, "error") <- "not a typed variable"
    valid
  }
}

getCheckTypeExpr <- function(type, varname) {

  check_type_expr <- get0(type, envir = custom_types,
                          ifnotfound = NULL)[["check_type_expr"]]
  if (is.null(check_type_expr)) {
    check_type_expr <- get0(type, envir = types,
                            ifnotfound = NULL)[["check_type_expr"]]
    if (is.null(check_type_expr)) {
      stop("Neither a built-in nor a registered custom type: ", type)
    } else {
      check_type_expr_paths <- get(type, envir = types,
                                   inherits = FALSE)[["check_type_expr_paths"]]
    }
  } else {
    check_type_expr_paths <- get(type, envir = custom_types,
                                 inherits = FALSE)[["check_type_expr_paths"]]
  }

  for (path in check_type_expr_paths) {
    check_type_expr[[path]] <- as.name(varname)
  }

  check_type_expr
}

#' @param ... one or more character strings to be pasted together to an error
#'   message (without a separator), which describes why a variable is not of the
#'   declared type.
#'
#' @rdname registerCustomType
#' @keywords internal
#' @export
markInvalidWError <- function(...) {
  valid <- get(".lazyTyper_valid", envir = parent.frame())

  msg <- paste0(...)

  valid[1] <- FALSE
  attr(valid, "error") <- c(attr(valid, "error"), msg)

  assign(".lazyTyper_valid", valid, envir = parent.frame())
}
