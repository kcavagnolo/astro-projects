/* gsl_fitting.h                                                             */
/* $Id: gsl_fitting.h,v 1.1 2009-12-04 22:28:25 cavagnolo Exp $ */
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

typedef struct {
  synchrotron_model *M;
  data *D;

  gsl_cheb_series *series;
  int iterations;
  double chi;

  double (*function)(double , synchrotron_model *, gsl_cheb_series *);
  
} fitdata;

int gslf(const gsl_vector *x, void *params, gsl_vector *f);
int gsldf(const gsl_vector *x, void *params, gsl_matrix *J);
int gslfdf(const gsl_vector *x, void *params, gsl_vector *f, gsl_matrix *J);
void fitting(fitdata *F);
