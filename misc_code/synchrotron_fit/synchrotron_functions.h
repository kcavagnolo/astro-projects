/* synchrotron_functions.h                                                   */
/* $Id: synchrotron_functions.h,v 1.1 2009-12-04 22:28:25 cavagnolo Exp $ */
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
  double gamma;
  double nu_break;
  double amplitude;

  double dgamma;
  double dnu_break;
  double damplitude;

} synchrotron_model;

struct parameters {
  synchrotron_model *M;
  gsl_cheb_series *series;
  double x;
  double z;
};

double synch_F_integrand(double x, void *params);
double synch_F(double x, void *params);

double synch_Ft_integrand(double x, void *params);
double synch_Ft(double x, void *params);

double synch_Fcheb(double lx, void *params);
double synch_Ftcheb(double lx, void *params);

double JP(double nu, synchrotron_model *M, gsl_cheb_series *series);
double JP_internal(double nu, void *params);
double synch_B(double x, void *params);
double synch_B_integrand(double z, void *params);

double KP(double nu, synchrotron_model *M, gsl_cheb_series *series);
double KP_internal(double nu, void *params);
double synch_Bt(double x, void *params);
double synch_Bt_integrand(double q, void *params);

double CI(double nu, synchrotron_model *M, gsl_cheb_series *series);
double CI_internal(double nu, void *params);
double synch_C(double x, void *params);
double synch_C_integrand(double z, void *params);
