/* ID: R_init_india.c, last updated 2025-05-01, F.Osorio */

#include "base.h"
#include <R_ext/Rdynload.h>

#define CALLDEF(name, nargs)  {#name, (DL_FUNC) &name, nargs}
#define F77DEF(name, nargs)   {#name, (DL_FUNC) &F77_NAME(name), nargs}

/* approximation of fabs() and its derivatives (i.e. 'residuals' and 'weights') */
extern void resid_and_weights(double *, int *, double *, double *, double *, double *);

/* registering C and symbols */
static const R_CMethodDef CEntries[]  = {
  CALLDEF(resid_and_weights,        6),
  {NULL, NULL, 0}
};

void R_init_india(DllInfo *dll)
{
  R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
