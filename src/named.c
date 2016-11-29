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

SEXP evalPromiseCode(SEXP promise_name, SEXP promise_env, SEXP reference_name,
                     SEXP reference_env) {
  SEXP promise = findVarInFrame3(promise_env, promise_name, TRUE);
  SEXP reference = eval(PRCODE(promise), PRENV(promise));
  Rf_defineVar(reference_name, reference, reference_env);

  return R_NilValue;
}

SEXP simpleGet(SEXP varname, SEXP env) {
  SEXP var = findVarInFrame3(env, varname, TRUE);
  if (TYPEOF(var) == PROMSXP) {
    PROTECT(var);
    var = eval(var, env);
    UNPROTECT(1);
  }

  return var;
}
