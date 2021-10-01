/*
** Interface between fortran and gsl for gamma, digamma, beta
** and incomplete beta functions.
*/

#include <gsl/gsl_sf_gamma.h>
#include <gsl/gsl_sf_psi.h>

/* digamma = d ln gamma(x) / dx */
double fpsi_ (double *x) {
  return gsl_sf_psi (*x);
}

/* gamma function */
double fgamma_ (double *x) {
  return gsl_sf_gamma (*x);
}

/* log of gamma function */
double flngamma_ (double *x) {
  return gsl_sf_lngamma (*x);
}

/* Normalized incomplete gamma function */
double fincgamma_ (double *a, double *x) {
  return gsl_sf_gamma_inc_Q (*a, *x);
}

/* Beta function */
double fbeta_ (double *a, double *b) {
  return gsl_sf_beta (*a, *b);
}

/* log beta function */
double flnbeta_ (double *a, double *b) {
  return gsl_sf_lnbeta (*a, *b);
}

/* Normalized incomplete beta function */
double fincbeta_ (double *a, double *b, double *x) {
  return gsl_sf_beta_inc (*a, *b, *x);
}
