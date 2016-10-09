#' @useDynLib lazyTyper

getNamed <- function(varname, env) {
  .Call("getNamed", as.symbol(varname), env, PACKAGE = "lazyTyper")
}

setNamed <- function(varname, env, named) {
  invisible(.Call("setNamed", as.symbol(varname), env, as.integer(named),
                  PACKAGE = "lazyTyper"))
}
