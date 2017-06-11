#include "internal.h"
#include "public.h"
#include "external.h"
#include <R_ext/Rdynload.h>

void R_init_lazyTyper(DllInfo *info) {
  static const R_CallMethodDef callMethods[]  = {
    // internal
    {"eval2Reference", (DL_FUNC) &eval2Reference, 4},
    {"evalPromiseCode", (DL_FUNC) &evalPromiseCode, 4},
    {"getNamed", (DL_FUNC) &getNamed, 2},
    {"setNamed", (DL_FUNC) &setNamed, 3},
    {"simpleGet", (DL_FUNC) &simpleGet, 2},

    // public
    {"iMode", (DL_FUNC) &iMode, 1},
    {"hasValue", (DL_FUNC) &hasValue, 2},

    // external
    {"getLocal0", (DL_FUNC) &getLocal0, 3},
    {NULL, NULL, 0}
  };

  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
  R_forceSymbols(info, TRUE);
}
