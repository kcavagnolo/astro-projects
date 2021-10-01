/* synchrotron_functions.c                                                  */
/* $Id: synchrotron_functions.c,v 1.1 2009-12-04 22:28:25 cavagnolo Exp $ */
/*                                                                           */
/* Copyright (C) 2003,2004,2005,2006 Christopher Z Waters                    */
/*                                                                           */
/* This program is free software; you can redistribute it and/or modify      */
/* it under the terms of the GNU General Public License as published by      */
/* the Free Software Foundation; either version 2 of the License, or         */
/* (at your option) any later version.                                       */
/*                                                                           */
/* This program is distributed in the hope that it will be useful,           */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of            */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             */
/* GNU General Public License for more details.                              */
/*                                                                           */
/* You should have received a copy of the GNU General Public License         */
/* along with this program; if not, write to the Free Software               */
/* Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_sf_bessel.h>
#include <gsl/gsl_integration.h>
#include <gsl/gsl_chebyshev.h>
#include <gsl/gsl_errno.h>

#include "./synchrotron_functions.h"


/* Calculate the synchrotron F function:                                    */
/* F(x) = x * \int_{x}^{\infty} K_{5 / 3}(z) dz                             */
/* where x = nu / nu_break                                                  */
double synch_F_integrand(double x, void *params) {
  return(gsl_sf_bessel_Knu(5.0 / 3.0,x));
}
double synch_F(double x, void *params) {
  gsl_integration_workspace *wF = gsl_integration_workspace_alloc(1000);
  double results, error;

  gsl_function ifunc;
  ifunc.function = &synch_F_integrand;
  ifunc.params = NULL;
  gsl_integration_qagiu(&ifunc,x,1e-7,1e-7,1000,wF,&results,&error);
  gsl_integration_workspace_free(wF);
  return(x * results);
}

/* Calculate the emission from a isotropic distribution of electrons:      */
/* Ft(x) = \int_{0}^{\pi / 2} \sin^2 \theta * F(x / sin(\theta)) d\theta   */
/* where x = nu / nu_break.                                                */
/* Follows from Pacholczyk Eq(6.45)                                        */
double synch_Ft_integrand(double q, void *params) {
  double x = *(double *) params;
  double result;
  result = pow(sin(q),2) * synch_F(( x / sin(q)),0);
  return(result);
}
double synch_Ft(double x, void *params) {
  double result,error;
  size_t nint;
  gsl_function ifunc;
  ifunc.function = &synch_Ft_integrand;
  ifunc.params = &x;
  gsl_integration_qng(&ifunc,0,M_PI / 2,1e-7,1e-7,&result,&error,&nint);
  return(result);
}

/* To speed up evaluation, we use these functions to create Chebyshev     */
/* approximations of the above functions.  This allows us to only need to */
/* evaluate them once.  These are evaluated in log-log space, unlike the  */
/* actual F and Ft functions.                                             */
double synch_Fcheb(double lx, void *params) {
  double result;
  result = log10(synch_F(pow(10,lx),0));
  return(result);
}
double synch_Ftcheb(double lx, void *params) {
  double result;
  result = log10(synch_Ft(pow(10,lx),0));
  return(result);
}

/* Begin JP Model: */
/* This is the high level function, which accepts the model, and the      */
/* Chebyshev series for Ft, and evaluates the flux at nu.                 */
double JP(double nu, synchrotron_model *M, gsl_cheb_series *series) {
  struct parameters P;
  P.M = M;
  P.series = series;
  return(JP_internal(nu,&P));
}
double JP_internal(double nu, void *params) {
  struct parameters P = *(struct parameters *) params;
  return(pow(10,P.M->amplitude) * synch_B(nu / P.M->nu_break, &P));
}

/* This is the synchrotron B function, which is Pacholczyk Eq(6.49).      */
/* B(x) = x^{0.5 * (1 - gamma)} *                                         */
/*             \int_{x}^{\infty} F(z) z^{-0.5} *                          */
/*                                 (sqrt(z) - sqrt(x))^{gamma - 2} dz     */
/* using Ft instead of F.                                                 */
double synch_B(double x, void *params) {
  struct parameters P = *(struct parameters *) params;
  gsl_integration_workspace *wB = gsl_integration_workspace_alloc(1000);
  double result,error;
  gsl_function ifunc;
  P.x = x;
  ifunc.function = &synch_B_integrand;
  ifunc.params = &P;
  gsl_integration_qagiu(&ifunc,x,1e-7,1e-7,1000,wB,&result,&error);
  gsl_integration_workspace_free(wB);
  return(pow(x,(1 - P.M->gamma)/2.0) * result);
}
double synch_B_integrand(double z, void *params) {
  struct parameters P = *(struct parameters *) params;
  return(pow(10,gsl_cheb_eval(P.series,log10(z))) * pow(z,-0.5) *
	 pow( sqrt(z) - sqrt(P.x),P.M->gamma - 2));
}
  
/* Begin KP Model: */
/* This is the same high level function.                                 */
double KP(double nu, synchrotron_model *M, gsl_cheb_series *series) {
  struct parameters P;
  P.M = M;
  P.series = series;
  return(KP_internal(nu,&P));

}
double KP_internal(double nu, void *params) {
  struct parameters P = *(struct parameters *) params;
  return(pow(10,P.M->amplitude) * synch_Bt(nu / P.M->nu_break, &P));
}
/* This is the Bt function; Pacholczyk Eq(6.53).                         */
/* Bt(x) = \int_{0}^{\pi / 2} (\sin \theta)^{2 * \gamma} *               */
/*                                 B(x * \sin^3 \theta) d\theta          */
/* This uses the standard F.                                             */
double synch_Bt(double x, void *params) {
  struct parameters P = *(struct parameters *) params;
  double result,error;
  size_t nint;
  gsl_function ifunc;
  P.x = x;
  ifunc.function = &synch_Bt_integrand;
  ifunc.params = &P;
  gsl_integration_qng(&ifunc,0,M_PI / 2.0,1e-7,1e-7,&result,&error,&nint);
  return(result);
}
double synch_Bt_integrand(double q, void *params) {
  struct parameters P = *(struct parameters *) params;
  return( pow(sin(q),2.0 * P.M->gamma) *
	  synch_B(P.x * pow(sin(q),3),&P));
}

/* Begin CI Model: */
/* High level function:                                                 */
double logCI(double lognu,synchrotron_model *M, gsl_cheb_series *series) {
  struct parameters P;
  P.M = M;
  P.series = series;
  return(log10(CI_internal(pow(10,lognu),&P)));
}
double CI(double nu, synchrotron_model *M, gsl_cheb_series *series) {
  struct parameters P;
  P.M = M;
  P.series = series;
  return(CI_internal(nu,&P));
}
double CI_internal(double nu, void *params) {
  struct parameters P = *(struct parameters *) params;
  return(pow(10,P.M->amplitude) * synch_C(nu / P.M->nu_break, &P));
}
/* This is the C function: Pacholczyk Eq(6.58).                         */
/* C(x) = (1 / (gamma - 1)) * x^{(1 - gamma) / 2} *                     */
/*        INTEGRAL(0) - INTEGRAL(x)                                     */
/* where                                                                */
/* INTEGRAL(y) = \int_{y}^{\infty} F(z) z^{-0.5} *                      */
/*                     (sqrt(z) - sqrt(y))^{gamma - 1} dz               */
/* This uses the Ft.                                                    */
double synch_C(double x, void *params) {
  struct parameters P = *(struct parameters *) params;
  gsl_integration_workspace *wC = gsl_integration_workspace_alloc(1000);
  double result1,error1;
  double result2, error2;

  gsl_function ifunc;

  P.x = 0.0;
  ifunc.function = synch_C_integrand;
  ifunc.params = &P;
  gsl_integration_qagiu(&ifunc,0.0,1e-7,1e-7,1000,wC,&result1,&error1);

  P.x = x;
  ifunc.function = synch_C_integrand;
  ifunc.params = &P;
  gsl_integration_qagiu(&ifunc,x,1e-7,1e-7,1000,wC,&result2,&error2);

  gsl_integration_workspace_free(wC);

  return( (1 / (P.M->gamma - 1)) * pow(x,(1 - P.M->gamma) / 2.0) *
	  (result1 - result2));
}

double synch_C_integrand(double z, void *params) {
  struct parameters P = *(struct parameters *) params;
  return( pow(10,gsl_cheb_eval(P.series,log10(z))) * pow(z,-0.5) *
	  pow(sqrt(z) - sqrt(P.x),P.M->gamma - 1));
}
  
