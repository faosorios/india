/* ID: base.h, last updated 2026-09-12, F.Osorio */

#ifndef INDIA_BASE_H
#define INDIA_BASE_H

#ifndef  USE_FC_LEN_T
# define USE_FC_LEN_T
#endif
#include <R.h>
#include <Rconfig.h>
#include <Rmath.h>
#include <Rinternals.h>
#include <R_ext/Print.h>
#include <R_ext/BLAS.h>
#include <R_ext/Lapack.h>
#include <R_ext/Linpack.h>
#include <R_ext/Applic.h>
#include <R_ext/Utils.h>

/* some definitions */
#define ABSTOL      1.0e-2
#define NULLP       (void *) 0
#define MAX(a,b)    (((a)>(b)) ? (a) : (b))
#define MIN(a,b)    (((a)<(b)) ? (a) : (b))
#define SQR(x)      R_pow_di(x, 2)
#define DOUBLE_EPS  DBL_EPSILON
#define repeat      for(;;)

/* to compute leverages using linear programming */
#define LP_TOL          1.0e-10
#define LP_TOL_BOUND    1.0e-8
#define LP_MAXIT        10000

#endif /* INDIA_BASE_H */
