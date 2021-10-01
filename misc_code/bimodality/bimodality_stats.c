/* Copyright (C) 2007 Christopher Waters <watersc1@pa.msu.edu> */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <unistd.h>
#include <ctype.h>
#include <gsl/gsl_cdf.h>

#include "bimodality.h"

double mean(double *x, int N) {
  double m = 0;
  int i;

  for (i = 0; i < N; i++) {
    m += x[i];
  }
  return(m / N);
}
double stdev(double *x, int N) {
  double m = mean(x,N);
  double s = 0;
  int i;

  for (i = 0; i < N; i++) {
    s += pow(x[i] - m,2);
  }
  return(sqrt(s / N));
}

int max_index(double *x, int N) {
  double max = -99e99;
  double max_i = -1;
  int i;

  for (i = 0; i < N; i++) {
    if (x[i] > max) {
      max = x[i];
      max_i = i;
    }
  }
  return(max_i);
}
  

double p_unimodal(double logL_unimodal, double logL_bimodal, int df) {
  double lambda = -2.0 * (logL_unimodal - logL_bimodal);
/*   int df = 2; /\* by definition for the 1d case. *\/ */
  if (lambda > 0) {
    return(gsl_cdf_chisq_Q(lambda,df));
  }
  else {
    fprintf(stderr,"Warning, odd lambda value: %g.  Lying....\n",lambda);
    return(0);
  }
}
 
