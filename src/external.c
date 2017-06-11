#include "external.h"
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

SEXP getLocal0(SEXP varname, SEXP env, SEXP ifnotfound) {
  static SEXP(*fun)(SEXP, SEXP, SEXP) = NULL;
  if (fun == NULL)
    fun = (SEXP(*)(SEXP, SEXP, SEXP)) R_GetCCallable("conditionR", "getLocal0");
  return fun(varname, env, ifnotfound);
}

