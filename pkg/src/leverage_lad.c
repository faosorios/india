/* ID: leverage_lad.c, last updated 2026-09-13, F.Osorio */

#include "base.h"
#include "interface.h"

/* leverage for LAD regression */
static void set_objective(double *, int, int, int, double *, const int *, int, int);
static void build_constraints(const double *, int, int, double *, int *);
static int simplex_max(double *, int, int, int, int *, double *);
/* ..end declarations */

/* ========================================================================== *
 * leverage for LAD regression following the proposal by Flores (2015)
 * ========================================================================== */

static void 
set_objective(double *x, int n, int p, int target, double *tab, const int *basis, int cstr, int nvar)
{ /* rebuild only the objective row for a new target, in canonical form 
   * relative to the current feasible simplex basis. */
  const int rhs = nvar + cstr, ncol = rhs + 1;
  double *obj = tab;

  memset(obj, 0, (size_t) ncol * sizeof(double));

  for (int k = 0; k < p; ++k) {
    const double xik = x[target + (size_t) n * k];
    obj[k] = -xik;       /* -c for g+ */
    obj[p + k] = xik;    /* -c for g- */
  }

  /* Eliminate coefficients of all current basic variables. */
  for (int i = 1; i <= cstr; ++i) {
    const int b = basis[i - 1];
    const double a = obj[b];
    if (fabs(a) > LP_TOL) {
      double *row = tab + (size_t) i * ncol;
      ax_plus_y(-a, row, 1, obj, 1, ncol);
    }
  }
}

static void 
build_constraints(const double *x, int n, int p, double *tab, int *basis)
{ /* build the common constraints once. All target observations share them */
  const int nvar = 2 * p + n, cstr = 2 * n + 1;
  const int rhs = nvar + cstr, ncol = rhs + 1;

  memset(tab, 0, (size_t) (cstr + 1) * ncol * sizeof(double));

  /* Xg+ - Xg- - t <= 0 */
  for (int i = 0; i < n; ++i) {
    const int row = 1 + i;
    double *r = tab + (size_t) row * ncol;
    for (int k = 0; k < p; ++k) {
      const double xik = x[i + (size_t) n * k];
      r[k] = xik;
      r[p + k] = -xik;
    }
    r[2*p + i] = -1.0;
    r[nvar + i] = 1.0;
    basis[row - 1] = nvar + i;
  }

  /* -Xg+ + Xg- - t <= 0 */
  for (int i = 0; i < n; ++i) {
    const int row = 1 + n + i;
    double *r = tab + (size_t)row * ncol;
    for (int k = 0; k < p; ++k) {
      const double xik = x[i + (size_t) n * k];
      r[k] = -xik;
      r[p + k] = xik;
    }
    r[2*p + i] = -1.0;
    r[nvar + n + i] = 1.0;
    basis[row - 1] = nvar + n + i;
  }

  /* sum(t) <= 1 */
  {
    const int row = cstr;
    double *r = tab + (size_t) row * ncol;
  
    for (int i = 0; i < n; ++i)
      r[2 * p + i] = 1.0;
    r[nvar + cstr - 1] = 1.0;
    r[rhs] = 1.0;
    basis[row - 1] = nvar + cstr - 1;
  }
}

static int 
simplex_max(double *tab, int cstr, int nvar, int maxit, int *basis, double *opt)
{
  const int rhs = nvar + cstr, ncol = rhs + 1;

  for (int iter = 0; iter < maxit; ++iter) {
    int enter = -1;

    /* Bland's rule: structural and slack variables are eligible. */
    for (int j = 0; j < nvar + cstr; ++j) {
      if (tab[j] < -LP_TOL) {
        enter = j;
        break;
      }
    }

    if (enter < 0) {
      *opt = tab[rhs];
      return 0;
    }

    int leave = -1;
    double ratio = R_PosInf;

    for (int i = 1; i <= cstr; ++i) {
      double *row = tab + (size_t) i * ncol;
      const double a = row[enter];
      const double b = row[rhs];
      if (a > LP_TOL) {
        const double r = b / a;
        if (r < ratio - LP_TOL) {
          ratio = r;
          leave = i;
        }
      }
    }

    if (leave < 0)
      return 2;

    double *prow = tab + (size_t) leave * ncol;
    const double pivot = prow[enter];
    if (fabs(pivot) <= LP_TOL)
      return 3;

    scale(prow, ncol, 1, 1.0 / pivot);

    for (int i = 0; i <= cstr; ++i) {
      if (i == leave)
        continue;
      double *row = tab + (size_t) i * ncol;
      const double a = row[enter];
      if (fabs(a) > LP_TOL)
        ax_plus_y(-a, prow, 1, row, 1, ncol);
    }

    basis[leave - 1] = enter;
  }

  return 1;
}

void 
leverages_lad(double *x, int *n, int *k, double *gamma, int *info)
{ /* leverages_lad compute the leverages defined by Flores (2015), Eq. (14) 
   * TEST 24, 796-812. doi: 10.1007/s11749-015-0435-5
   * using a linear program by a primal simplex method */
  int nobs = *n, p = *k, *basis;
  double *tab;
 
  /* test the input parameters */
  *info = 0;
  if (nobs < 0) { 
    *info = -2;
  } else if (p < 0) {
    *info = -3;
  } 
  if (*info != 0) return;

  /* quick return if possible */
  if ((nobs == 0) || (p == 0))
    return;

  /* initialization */
  int nvar = 2 * p + nobs, cstr = 2 * nobs + 1, ncol = nvar + cstr + 1;
  int ntab = (cstr + 1) * ncol;
  basis = (int *) R_alloc(cstr, sizeof(int));
  tab   = (double *) R_alloc(ntab, sizeof(double));

  /* common feasible region: construct it only once */
  build_constraints(x, nobs, p, tab, basis);

  for (int i = 0; i < nobs; ++i) {
    /* warm start: retain the previous optimal feasible basis. */
    set_objective(x, nobs, p, i, tab, basis, cstr, nvar);

    double opt = 0.0;
    int status = simplex_max(tab, cstr, nvar, LP_MAXIT, basis, &opt);
    
    if (status != 0) {
      gamma[i] = NA_REAL;
      if (*info == 0)
        *info = status;
      break;
    }

    gamma[i] = opt;

    if ((gamma[i] < -LP_TOL_BOUND) || (gamma[i] > 1.0 + LP_TOL_BOUND)) {
      gamma[i] = NA_REAL;
      if (*info == 0)
        *info = 4;
      break;
    }
    if (gamma[i] < 0.0)
      gamma[i] = 0.0;
    if (gamma[i] > 1.0)
      gamma[i] = 1.0;
  }
}
