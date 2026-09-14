/* ID: interface.c, last updated 2026-09-12, F.Osorio */

#include "base.h"
#include "interface.h"
#include <fastmatrix_API.h>

/* ========================================================================== *
 * basic matrix manipulations
 * ========================================================================== */

void
ax_plus_y(double alpha, double *x, int incx, double *y, int incy, int n)
{ /* y <- alpha * x + y */
  BLAS1_axpy(alpha, x, incx, y, incy, n);
}

void
scale(double *x, int n, int inc, double alpha)
{ /* x <- alpha * x */
  BLAS1_scale(alpha, x, inc, n);
}

double
sum_abs(double *x, int n, int inc)
{ /* sum(abs(x)) */
  return BLAS1_sum_abs(x, inc, n);
}
