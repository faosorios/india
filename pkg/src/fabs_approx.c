/* ID: fabs_approx.c, last updated 2025-05-18, F.Osorio */

#include "base.h"

/* approximation of absolute value */
static double fabs_approx(double, double);
static double fabs_1st(double, double);
static double fabs_2nd(double, double);
static double sech_sqr(double);
/* ..end declarations */

/* ========================================================================== *
 * functions for approximation of the absolute value and its derivatives
 * ========================================================================== */

static double fabs_approx(double x, double eta)
{ /* approximation of absolute value, |x|_eta */
  return x * tanh(x / eta);
}

static double fabs_1st(double x, double eta)
{ /* first derivative of |x|_eta */
  double rel;

  rel = x / eta;
  return tanh(rel) + rel * sech_sqr(rel);
}

static double fabs_2nd(double x, double eta)
{ /* second derivative of |x|_eta */
  double rel;

  rel = x / eta;
  return sech_sqr(rel) * (1.0 - rel * tanh(rel));
}

static double sech_sqr(double x)
{ /* hyperbolic secant squared */
  double val = tanh(x);

  return (1.0 - SQR(val));
}

void resid_and_weights(double *dev, int *n, double *eta, double *fnc, double *resid, double *weights)
{ /* computation of |x|_eta, 'residuals' and 'weights' */
  int nobs = *n;
  double mu = *eta, res;

  for (int i = 0; i < nobs; i++) {
    res        = dev[i];
    fnc[i]     = fabs_approx(res, mu);
    resid[i]   = fabs_1st(res, mu);
    weights[i] = fabs_2nd(res, mu); /* proportional to 2nd derivative */
  }
}
