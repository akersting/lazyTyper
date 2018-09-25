/*
 * Internal Low-Level C Functions
 *
 * The documentation of these functions is in R/internal_c.R.
 */

#include "internal.h"
#include <Rinternals.h>

SEXP getNamed(SEXP varname, SEXP env) {
  if (TYPEOF(varname) != STRSXP || length(varname) != 1) {
    error("'varname' is not a character string");
  }
  if (TYPEOF(env) != ENVSXP) {
    error("'env' is not an environment");
  }

  SEXP varsym = installChar(STRING_ELT(varname, 0));
  // protect in case varsym is an active binding
  SEXP var = PROTECT(findVarInFrame3(env, varsym, TRUE));

  if (var == R_UnboundValue) {
    error("object '%s' not found", CHAR(PRINTNAME(varsym)));
  }

  SEXP named = PROTECT(ScalarInteger(NAMED(var)));
  UNPROTECT(2);

  return named;
}

SEXP setNamed(SEXP varname, SEXP named, SEXP env) {
  if (TYPEOF(varname) != STRSXP || length(varname) != 1) {
    error("'varname' is not a character string");
  }
  if (TYPEOF(env) != ENVSXP) {
    error("'env' is not an environment");
  }

  if (!(TYPEOF(named) == INTSXP || TYPEOF(named) == REALSXP) ||
      length(named) != 1) {
    error("'named' is not a single number");
  }
  int named_int = asInteger(named);
  if (named_int < 0 || named_int > NAMEDMAX) {
    error("'named' cannot be coerced to 0L, ..., NAMEDMAX");
  }

  SEXP varsym = installChar(STRING_ELT(varname, 0));
  // protect in case varsym is an active binding
  SEXP var = PROTECT(findVarInFrame3(env, varsym, TRUE));

  SET_NAMED(var, named_int);
  UNPROTECT(1);

  return R_NilValue;
}

SEXP eval2Reference(SEXP expr, SEXP reference_name, SEXP eval_env,
                    SEXP reference_env) {
  if (TYPEOF(eval_env) != ENVSXP) {
    error("'eval_env' is not an environment");
  }
  if (TYPEOF(reference_name) != STRSXP || length(reference_name) != 1) {
    error("'reference_name' is not a character string");
  }
  if (TYPEOF(reference_env) != ENVSXP) {
    error("'reference_env' is not an environment");
  }

  SEXP rhs = PROTECT(eval(expr, eval_env));

  SEXP reference_sym = installChar(STRING_ELT(reference_name, 0));;
  defineVar(reference_sym, rhs, reference_env);
  UNPROTECT(1);

  return R_NilValue;
}

SEXP evalPromiseCode(SEXP promise_name, SEXP promise_env, SEXP reference_name,
                     SEXP reference_env) {
  if (TYPEOF(promise_name) != STRSXP || length(promise_name) != 1) {
    error("'promise_name' is not a character string");
  }
  if (TYPEOF(promise_env) != ENVSXP) {
    error("'promise_env' is not an environment");
  }
  if (TYPEOF(reference_name) != STRSXP || length(reference_name) != 1) {
    error("'reference_name' is not a character string");
  }
  if (TYPEOF(reference_env) != ENVSXP) {
    error("'reference_env' is not an environment");
  }

  SEXP promise_sym = installChar(STRING_ELT(promise_name, 0));;
  // protect in case promise_sym is an active binding
  SEXP promise = PROTECT(findVarInFrame3(promise_env, promise_sym, TRUE));

  SEXP reference;
  if (TYPEOF(promise) == PROMSXP) {
    if (!isNull(PRENV(promise))) {
      reference = PROTECT(eval(PRCODE(promise), PRENV(promise)));
    } else {
      // promise had already been forced
      reference = PRVALUE(promise);
    }
  } else {
    /*
     * the byte-compiler might replace promises with constants, e.g. in
     * f() {g(x = 1)}; f <- cmpfun(f)
     * x in g() will be 1 and not a promise to 1
     */
    reference = promise;
  }
  SEXP reference_sym = installChar(STRING_ELT(reference_name, 0));;
  defineVar(reference_sym, reference, reference_env);

  if (TYPEOF(promise) == PROMSXP && !isNull(PRENV(promise))) {
    UNPROTECT(2);
  } else {
    UNPROTECT(1);
  }

  return reference;
}

SEXP simpleGet(SEXP varname, SEXP env) {
  if (TYPEOF(varname) != STRSXP || length(varname) != 1) {
    error("'varname' is not a character string");
  }
  if (TYPEOF(env) != ENVSXP) {
    error("'env' is not an environment");
  }

  SEXP varsym = installChar(STRING_ELT(varname, 0));
  // protect in case varsym is an active binding
  SEXP var = PROTECT(findVarInFrame3(env, varsym, TRUE));

  if (var == R_UnboundValue) {
    error("object '%s' not found", CHAR(PRINTNAME(varsym)));
  }

  // force promise
  if (TYPEOF(var) == PROMSXP) {
    var = PROTECT(eval(var, env));
    UNPROTECT(1);
  }
  UNPROTECT(1);

  return var;
}
