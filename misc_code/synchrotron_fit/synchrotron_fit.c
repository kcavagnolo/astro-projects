/* synchrotron_fit.c                                                         */
/* $Id: synchrotron_fit.c,v 1.6 2009-12-08 01:14:17 cavagnolo Exp $ */
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

void print_results(fitdata F, char *ofile,char *function) {
  FILE *output;
  int i;
  double lnu;
  output = fopen(ofile,"w");

  fprintf(output,"#%s\n",function);
  fprintf(output,"#Chi        = %g\n",F.chi);
  fprintf(output,"#Iterations = %d\n",F.iterations);
  fprintf(output,"#Gamma      = %g +/- %g\n",F.M->gamma,F.M->dgamma);
  fprintf(output,"#Nu_Break   = %g +/- %g\n",F.M->nu_break,F.M->dnu_break);
  fprintf(output,"#Amplitude  = %g +/- %g\n",F.M->amplitude,F.M->damplitude);
  
  for (i = 0; i < 100; i++) {
    lnu = 6.0 + 13 * ((i * 1.0) / 100.0);
    fprintf(output,"%g %g\n",pow(10,lnu),F.function(pow(10,lnu),F.M,F.series));
  }
  fclose(output);
}

int main (int argc, char *argv[]) {
  fitdata F;

  gsl_cheb_series *Fser = gsl_cheb_alloc(50);
  gsl_cheb_series *Ftser = gsl_cheb_alloc(50);
  gsl_function ffunc;
  
  char ofile[256];

  int i;
  double xmean = 0,ymean = 0,xsq = 0,xy = 0,xmm = 0;
  
  if (argc != 2) {
    fprintf(stderr,"Usage: synchrotron_fit <datafile>\n");
    fprintf(stderr,"\n");
    fprintf(stderr,"Datafile format:   <nu_(Hz)> <flux_(uJy)> <error_in_flux_(uJy)\n");
    exit(10);
  }
  /* Read datafile. */
  F.D = read_data(argv[1]);
  F.M = malloc(sizeof(synchrotron_model));
  if (F.M == NULL) {
    fprintf(stderr,"synchrotron_fit.c : Error allocating `F.M'\n");
  }
  /* Calculate Chebyshev approximations. */
  gsl_set_error_handler_off();
  ffunc.params = NULL;

  ffunc.function = &synch_Fcheb;
  gsl_cheb_init(Fser,&ffunc,-5.5,2.75);

  ffunc.function = &synch_Ftcheb;
  gsl_cheb_init(Ftser,&ffunc,-5.5,2.75);

  /* Estimate gamma from the first five points. */
  for (i = 0; i < 5; i++) {
    xmean += F.D->nu[i];
    ymean += F.D->flux[i];
    xsq   += pow(F.D->nu[i],2);
    xy    += F.D->nu[i] * F.D->flux[i];
  }
  xmean /= 5;
  ymean /= 5;
  for (i = 0; i < 5; i++) {
    xmm += (pow(F.D->nu[i],2)) - (pow(xmean,2));
  }
  F.M->gamma = (xy - (5 * xmean * ymean))/(xmm);
  F.M->gamma = (-2 * F.M->gamma) + 1;

  /* Guess the other two parameters: */
  F.M->amplitude = 1;
  F.M->nu_break  = 100e9;

  /* Solve each model in turn, saving the output to <file>.{JP|KP|CI}     */
  F.series = Ftser;
  F.function = &JP;
  fitting(&F);
  snprintf(ofile,sizeof(ofile),"%s.JP",argv[1]);
  print_results(F,ofile,"JP_model");
  
  F.series = Fser;
  F.function = &KP;
  fitting(&F);
  snprintf(ofile,sizeof(ofile),"%s.KP",argv[1]);
  print_results(F,ofile,"KP_model");
  
  F.series = Ftser;
  F.function = &CI;
  fitting(&F);
  snprintf(ofile,sizeof(ofile),"%s.CI",argv[1]);
  print_results(F,ofile,"CI_model");

  return(0);
}
