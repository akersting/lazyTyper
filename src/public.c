/*
 * Public Low-Level C Functions
 *
 * The documentation of these functions is in R/public_c.R.
 */

#include <Rinternals.h>

SEXP hasValue(SEXP x, SEXP env) {
  if (TYPEOF(x) != STRSXP || length(x) != 1) {
    error("'x' is not a character string");
  }
  if (TYPEOF(env) != ENVSXP) {
    error("'env' is not an environment");
  }

  SEXP varsym = installChar(STRING_ELT(x, 0));
  SEXP var = findVarInFrame3(env, varsym, TRUE);

  if (var == R_UnboundValue || var == R_MissingArg) {
    return ScalarLogical(0);
  }

  return ScalarLogical(1);
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
