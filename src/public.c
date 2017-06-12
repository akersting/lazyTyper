/*
 * Public Low-Level C Functions
 *
 * The documentation of these functions is in R/public_c.R.
 */

#include "public.h"
#include <Rinternals.h>

SEXP hasValue(SEXP x, SEXP env) {
  if (TYPEOF(x) != STRSXP) {
    error("'x' is not a character vector");
  }
  if (TYPEOF(env) != ENVSXP) {
    error("'env' is not an environment");
  }

  R_xlen_t x_len = XLENGTH(x);
  SEXP res = PROTECT(allocVector(LGLSXP, x_len));

  SEXP varname;
  SEXP varsym;
  SEXP var;
  int *res_lgl = LOGICAL(res);
  for (R_xlen_t i = 0; i < x_len; i++) {
    varname = STRING_ELT(x, i);
    if (varname == NA_STRING) {
      res_lgl[i] = NA_LOGICAL;
      continue;
    }

    varsym = installChar(varname);
    var = findVarInFrame3(env, varsym, TRUE);

    if (var == R_UnboundValue || var == R_MissingArg) {
      res_lgl[i] = 0;
    } else {
      res_lgl[i] = 1;
    }
  }

  setAttrib(res, R_NamesSymbol, x);
  UNPROTECT(1);

  return res;
}

const char *Mode(SEXP x) {
  SEXPTYPE type = TYPEOF(x);
  switch (type) {
  case REALSXP:
  case INTSXP: return "numeric";
  case CLOSXP:
  case BUILTINSXP:
  case SPECIALSXP: return "function";
  default: return type2char(type);
  }
}

SEXP iMode(SEXP x) {
  if (isOrdered(x)) {
    return mkString("ordered");
  } else if (isFactor(x)) {
    return mkString("factor");
  } else if (R_IsNamespaceEnv(x)) {
    return mkString("namespace");
  } else if (isVectorList(x) && inherits(x, "data.frame")) {
    return mkString("data.frame");
  } else if (isMatrix(x) || isArray(x)) {
    const char *mode = Mode(x);
    const char *type = (isMatrix(x) ? "Matrix" : "Array");
    int len = snprintf(NULL, 0, "%s%s", mode, type);
    char imode[len + 1];
    sprintf(imode, "%s%s", mode, type);
    return mkString(imode);
  } else {
    return mkString(Mode(x));
  }
}
