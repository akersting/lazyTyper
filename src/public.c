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
