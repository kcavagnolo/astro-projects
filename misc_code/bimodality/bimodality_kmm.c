/* Copyright (C) 2007 Christopher Waters <watersc1@pa.msu.edu> */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <unistd.h>
#include <ctype.h>


#include "bimodality.h"

double gaussian(double x, double m, double s) {
  return(pow(s * sqrt(2 * M_PI),-1) * exp(-0.5 * pow( (x - m) / s,2)));
}

double kmm_unimodal(bin *B, double *m, double *s) {
  int j;
  double logL;
  
  *m = 0;
  *s = 0;
  logL = 0;
  
  for (j = 0; j < B->N; j++) {
    *m += B->x[j];
  }
  *m /= B->N;

  for (j = 0; j < B->N; j++) {
    *s += pow(B->x[j] - *m,2);
  }
  *s = sqrt(*s / B->N);

  for (j = 0; j < B->N; j++) {
    logL += log(gaussian(B->x[j],*m,*s));
  }

  return(logL);
  
}

void kmm_calculate_P(bin *B, double *P1, double *P2,
		     double pi1, double m1, double s1,
		     double pi2, double m2, double s2) {
  int i;
  double p1,p2;
  double norm;
  double g1,g2;
  for (i = 0; i < B->N; i++) {
    p1 = P1[i];
    p2 = P2[i];
    g1 = gaussian(B->x[i],m1,s1);
    g2 = gaussian(B->x[i],m2,s2);
    norm = (p1 * gaussian(B->x[i],m1,s1) + p2 * gaussian(B->x[i],m2,s2));
    P1[i] = (p1 * gaussian(B->x[i],m1,s1)) / norm;
    P2[i] = (p2 * gaussian(B->x[i],m2,s2)) / norm;
    norm = pi1 * g1 + pi2 * g2;
    P1[i] = (pi1 * g1 / norm);
    P2[i] = (pi2 * g2 / norm); 
  }
}

double kmm_calculate_M(bin *B, double *P1, double *P2,
		       double *pi1, double *m1, double *s1,
		       double *pi2, double *m2, double *s2) {
  int i;
  double w1 = 0,w2 = 0;
  double logL = 0;

  
  for (i = 0; i < B->N; i++) {
    /* This shouldn't be right, but it is. */
    if (!((*pi1 == 0||(*pi2 == 0)))) {
      logL += log(*pi1 * gaussian(B->x[i],*m1,*s1) +
		  *pi2 * gaussian(B->x[i],*m2,*s2));
    }
    else {
      fprintf(stderr,"WARN: bad proportions!\n");
    }


/*     logL += P1[i] * (log(*pi1 * gaussian(B->x[i],*m1,*s1))); */
/*     logL += P2[i] * (log(*pi2 * gaussian(B->x[i],*m2,*s2))); */
/*     printf("%g @ %g : %g %g %g %g : %g %g %g %g\n", */
/* 	   logL,B->x[i], */
/* 	   P1[i],*pi1,*m1,*s1, */
/* 	   P2[i],*pi2,*m2,*s2); */
  }

  *m1 = 0;
  *s1 = 0;
  *m2 = 0;
  *s2 = 0;
  
  for (i = 0; i < B->N; i++) {
    *m1 += B->x[i] * P1[i];
    *m2 += B->x[i] * P2[i];

    w1 += P1[i];
    w2 += P2[i];
  }

  *m1 /= w1;
  *m2 /= w2;

  for (i = 0; i < B->N; i++) {
    *s1 += pow((B->x[i] - *m1),2)* P1[i] ;
    *s2 += pow((B->x[i] - *m2),2)* P2[i] ;
  }

  *s1 = sqrt(*s1 / w1);
  *s2 = sqrt(*s2 / w2);

  *pi1 = w1 / B->N;
  *pi2 = w2 / B->N;

  if (*pi1 != *pi1) {
    *pi1 = 0;
  }
  if (*pi2 != *pi2) {
    *pi2 = 0;
  }
  return(logL);
}

  
  
  
  
