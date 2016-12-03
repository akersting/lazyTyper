#' Type-Based Assertions
#'
#' Assert that the return value of an expression is of the desired type.
#'
#' @param expr the (unquoted) expression whose return value to check.
#' @param type the type to check the return value of \code{expr} against (as a
#'   character string).
#' @param ... further properties of the return value of \code{expr} to
#'   guarantee. The availability of additional properties depends on
#'   \code{type}. Both positional and partial matching are used on these
#'   arguments.
#'
#' @return expr (evaluated and invisibly).
#'
#' @export
assertType <- function(expr, type, ...) {
  if (missing(expr)) {  # prevent a hard to understand error in C-level eval
    stop("Argument 'expr' is missing (with no default).")
  }

  setErrorContext(
    "assertError",
    "Failed to assert the type of the expression.",
    base_class = "lazyTyperError"
  )

  assignToLazyTyperEnv("value", type = type, properties = list(...),
                       env = environment())

  unsetErrorContext("lazyTyperError")
  contextualize(
    evalPromiseCode("expr", reference_name = "value"),
    error = list(message = "Failed to evaluate 'expr'.",
                 class = "assertError",
                 base_class = "lazyTyperError")
  )

  setErrorContext(
    "assertError",
    "The return value of the expression is not of the specified type.",
    base_class = "lazyTyperError"
  )
  checkType("value", environment())

  invisible(simpleGet("value"))
}
