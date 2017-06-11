getLocal0 <- function(x, envir = parent.frame(), ifnotfound = NULL) {
  .Call(C_getLocal0, varname = x, env = envir, ifnotfound = ifnotfound)
}
