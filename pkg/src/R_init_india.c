/* ID: R_init_india.c, last updated 2026-09-12, F.Osorio */

#include "base.h"
#include <R_ext/Rdynload.h>

#define CALLDEF(name, nargs)  {#name, (DL_FUNC) &name, nargs}
#define F77DEF(name, nargs)   {#name, (DL_FUNC) &F77_NAME(name), nargs}

/* leverages using the proposal by Flores (2015) */
extern void leverages_lad(double *, int *, int *, double *, int *);

/* approximation of fabs() and its derivatives (i.e. 'residuals' and 'weights') */
extern void resid_and_weights(double *, int *, double *, double *, double *, double *);

/* registering C symbols */
static const R_CMethodDef CEntries[]  = {
  CALLDEF(leverages_lad,        5),
  CALLDEF(resid_and_weights,    6),
  {NULL, NULL, 0}
};

void R_init_india(DllInfo *dll)
{
  R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
