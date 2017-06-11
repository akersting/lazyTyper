#ifndef LAZYTYPER_INTERNAL_H
#define LAZYTYPER_INTERNAL_H

#include <Rinternals.h>

SEXP eval2Reference(SEXP, SEXP, SEXP, SEXP);
SEXP evalPromiseCode(SEXP, SEXP, SEXP, SEXP);
SEXP getNamed(SEXP, SEXP);
SEXP setNamed(SEXP, SEXP, SEXP);
SEXP simpleGet(SEXP, SEXP);

#endif
