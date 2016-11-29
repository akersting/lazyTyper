#' @useDynLib lazyTyper

getNamed <- function(varname, env) {
  .Call("getNamed", as.symbol(varname), env, PACKAGE = "lazyTyper")
}

setNamed <- function(varname, env, named) {
  invisible(.Call("setNamed", as.symbol(varname), env, as.integer(named),
                  PACKAGE = "lazyTyper"))
}

# evaluate expr in eval_env and assign the result to rhs_name in rhs_env without
# increasing NAMED; compared to eval() this does not "break" if expr is
# something along the lines of "n <- sys.nframe()", i.e. sys.nframe() does not
# count the call to evalAssign but it would count the call to eval
evalAssign <- function(expr, eval_env, rhs_name = "rhs",
                       rhs_env = parent.frame()) {
  .Call("evalAssign", expr, eval_env, as.symbol(rhs_name), rhs_env,
        PACKAGE = "lazyTyper")
}

# evaluate the code of a promise in the appropriate environment and create
# a reference to the return value in the specified env without increasing
# NAMED
evalPromiseCode <- function(promise_name, promise_env = parent.frame(),
                            reference_name = promise_name,
                            reference_env = promise_env) {
  .Call("evalPromiseCode", as.symbol(promise_name), promise_env,
        as.symbol(reference_name), reference_env, PACKAGE = "lazyTyper")
}

simpleGet <- function(varname, env = parent.frame()) {
  .Call("simpleGet", as.symbol(varname), env, PACKAGE = "lazyTyper")
}
