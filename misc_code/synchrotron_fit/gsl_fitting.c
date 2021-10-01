/* gsl_fitting.c                                                             */
/* $Id: gsl_fitting.c,v 1.1 2009-12-04 22:28:25 cavagnolo Exp $ */
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
#include <math.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_sf_bessel.h>
#include <gsl/gsl_integration.h>
#include <gsl/gsl_chebyshev.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_multifit_nlin.h>
#include "./read_data.h"
#include "./synchrotron_functions.h"
#include "./gsl_fitting.h"

int gslf(const gsl_vector *x, void *params, gsl_vector *f) {
  fitdata *F = (fitdata *) params;
  int i;
  double V;
  F->M->amplitude = gsl_vector_get(x,0);
  F->M->nu_break  = pow(10,gsl_vector_get(x,1));
  F->M->gamma     = gsl_vector_get(x,2);

  for (i = 0; i < F->D->number; i++) {
    V = (log10(F->D->flux[i]) -
	 log10(F->function(F->D->nu[i],F->M,F->series))) /
      (F->D->errors[i] / (log(10) * F->D->flux[i])); 
    gsl_vector_set(f,i,V);
  }
  return(GSL_SUCCESS);
}
int gsldf(const gsl_vector *x, void *params, gsl_matrix *J) {
  fitdata *F = (fitdata *) params;
  synchrotron_model Mdelta;
  int i;
  double h = 1e-5;
  double dfdA,dfdn,dfdg;
  F->M->amplitude = gsl_vector_get(x,0);
  F->M->nu_break  = pow(10,gsl_vector_get(x,1));
  F->M->gamma     = gsl_vector_get(x,2);

  for (i = 0; i < F->D->number; i++) {
    Mdelta.amplitude = gsl_vector_get(x,0) + h;
    Mdelta.nu_break  = pow(10,gsl_vector_get(x,1));
    Mdelta.gamma     = gsl_vector_get(x,2);

    dfdA = (log10(F->function(F->D->nu[i],F->M,F->series)) -
	    log10(F->function(F->D->nu[i],&Mdelta,F->series))) / h;
    
    Mdelta.amplitude = gsl_vector_get(x,0);
    Mdelta.nu_break  = pow(10,gsl_vector_get(x,1) + h);
    Mdelta.gamma     = gsl_vector_get(x,2);

    dfdn = (log10(F->function(F->D->nu[i],F->M,F->series)) -
	    log10(F->function(F->D->nu[i],&Mdelta,F->series))) / h;

    Mdelta.amplitude = gsl_vector_get(x,0);
    Mdelta.nu_break  = pow(10,gsl_vector_get(x,1));
    Mdelta.gamma     = gsl_vector_get(x,2) + h;

    dfdg = (log10(F->function(F->D->nu[i],F->M,F->series)) - 
	    log10(F->function(F->D->nu[i],&Mdelta,F->series))) / h;

    gsl_matrix_set(J,i,0,dfdA / (F->D->errors[i] / (log(10) * F->D->flux[i])));
    gsl_matrix_set(J,i,1,dfdn / (F->D->errors[i] / (log(10) * F->D->flux[i])));
    gsl_matrix_set(J,i,2,dfdg / (F->D->errors[i] / (log(10) * F->D->flux[i])));
  }
  return(GSL_SUCCESS);
}

int gslfdf(const gsl_vector *x, void *params, gsl_vector *f, gsl_matrix *J) {
  gslf(x,params,f);
  gsldf(x,params,J);
  return(GSL_SUCCESS);
}

void fitting(fitdata *F) {
  const gsl_multifit_fdfsolver_type *T;
  gsl_multifit_fdfsolver *s;
  int status = GSL_CONTINUE;

  gsl_matrix *covar = gsl_matrix_alloc(3,3);
  gsl_multifit_function_fdf f;
  double x_init[3];
  gsl_vector_view x;

  f.f = &gslf;
  f.df = &gsldf;
  f.fdf = &gslfdf;
  f.n = F->D->number;
  f.p = 3;
  f.params = F;

  x_init[0] = F->M->amplitude;
  x_init[1] = log10(F->M->nu_break);
  x_init[2] = F->M->gamma;
  x = gsl_vector_view_array(x_init,3);

  T = gsl_multifit_fdfsolver_lmsder;
  s = gsl_multifit_fdfsolver_alloc(T,F->D->number,3);
  gsl_multifit_fdfsolver_set(s,&f,&x.vector);

  F->iterations = 0;
  while ((status == GSL_CONTINUE)&&(F->iterations < 500)) {
    F->iterations++;
    status = gsl_multifit_fdfsolver_iterate(s);
    if (status) {
      break;
    }
    status = gsl_multifit_test_delta(s->dx,s->x,1e-6,1e-6);
    printf("%d %g %g %g\n",
	   F->iterations,
	   gsl_vector_get(s->x,0),gsl_vector_get(s->x,1),gsl_vector_get(s->x,2));
	  
	   
	   
  }

  gsl_multifit_covar(s->J,0.0,covar);

  F->M->amplitude = gsl_vector_get(s->x,0);
  F->M->nu_break  = pow(10,gsl_vector_get(s->x,1));
  F->M->gamma     = gsl_vector_get(s->x,2);

  F->M->damplitude = sqrt(gsl_matrix_get(covar,0,0));
  F->M->dnu_break  = sqrt(gsl_matrix_get(covar,1,1));
  F->M->dgamma     = sqrt(gsl_matrix_get(covar,2,2));

  F->chi = gsl_blas_dnrm2(s->f);

  gsl_multifit_fdfsolver_free(s);
}
