#include <Rinternals.h>

SEXP getNamed(SEXP varname, SEXP env) {
  SEXP named = PROTECT(allocVector(INTSXP, 1));
  SEXP var = findVarInFrame3(env, varname, TRUE);
  INTEGER(named)[0] = NAMED(var);
  UNPROTECT(1);
  return named;
}

SEXP setNamed(SEXP varname, SEXP env, SEXP named) {
  SEXP var = findVarInFrame3(env, varname, TRUE);
  SET_NAMED(var, INTEGER(named)[0]);
  return R_NilValue;
}

SEXP evalAssign(SEXP expr, SEXP eval_env, SEXP rhs_name, SEXP rhs_env) {
  SEXP rhs = eval(expr, eval_env);
  Rf_defineVar(rhs_name, rhs, rhs_env);

  return R_NilValue;
}
