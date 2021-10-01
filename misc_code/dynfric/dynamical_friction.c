/* Copyright (C) 2007 Christopher Waters <watersc1@pa.msu.edu>               */
/* This program is free software; you can redistribute it and/or             */
/* modify it under the terms of the GNU General Public License               */
/* as published by the Free Software Foundation; either version 2            */
/* of the License, or (at your option) any later version.                    */

/* This program is distributed in the hope that it will be useful,           */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of            */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             */
/* GNU General Public License for more details.                              */

/* You should have received a copy of the GNU General Public License         */
/* along with this program; if not, write to the Free Software               */
/* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA. */
/* $Id: dynamical_friction.c,v 1.1 2009-01-07 17:23:35 cavagnolo Exp $ */

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <unistd.h>
#include <ctype.h>

#include <stddef.h>
#include <time.h>
#include <limits.h>

#include <gsl/gsl_blas.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_odeiv.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>

/* The Galaxy model for M87 taken from:        */
/* Vesperini, E., Zepf,S.~E., Kundu, A., \& Ashman, K.~M.\ 2003, \apj, 593, 760 */
#define V_R1 5.1
#define V_R2 560
#define V_M1 8.1e11
#define V_M2 7.06e14
#define ALPHA 1.67
#define GRAVITY 4.49867698e-15 /* (parsec / year)^2 (parsec / solar mass) */

double quadrature(double *f) {
  return(sqrt(pow(f[0],2) + pow(f[1],2) + pow(f[2],2)));
}

double mass_enclosed(double r) {
  double r1 = V_R1 * 1000;
  double r2 = V_R2 * 1000;
  double m1 = V_M1 * pow( (r / r1) / (1 + (r / r1)),ALPHA);
  double m2 = V_M2 * (log(1 + (r / r2)) - ( (r / r2) / (1 + (r / r2))));
  return(m1 + m2);
}
double local_density(double r) {
  double r1 = V_R1 * 1000;
  double r2 = V_R2 * 1000;
  double dM = (ALPHA * V_M1 * ( pow(r1 + r,-1) - r * pow(r1 + r,-2)) *
	       pow(r * pow(r1 + r,-1),ALPHA - 1) +
	       (V_M2 * (r * pow(r2 + r,-2))));
    
  double dV = 4 * M_PI * pow(r,2);
  return(dM / dV);
}
double circular_velocity(double r) {
  return(sqrt(GRAVITY * mass_enclosed(r) / r));
}
/* B+T eq (7-13b) */
double Lambda(double r, double M) {
  double b_max = 2000;
  double m = 0.7;
  double V0 = circular_velocity(r);
  return (b_max * pow(V0,2) / (GRAVITY * (M + m)));
}
/* B+T eq (7-17) */
void df_dvdt(double r, double M, double *v,double *dv) {
  double lnL = log(Lambda(r,M));
  double vm = quadrature(v);
  double X = vm / (sqrt(2) * circular_velocity(r)); /* Naive assumption! */

  double dv0 = (- 4 * M_PI * lnL * pow(GRAVITY ,2) *
		local_density(r) * M) /
    pow(vm,3) * (erf(X) - (2 * X * exp(-pow(X,2))) / sqrt(M_PI));

  dv[0] = dv0 * v[0];
  dv[1] = dv0 * v[1];
  dv[2] = dv0 * v[2];
}
int gsl_f (double t, const double q[], double f[], void *params) {
  double x[3];
  double v[3];
  double a[3];
  double r,V;

  double a_gravity;
  double phi;
  double theta;

  double *M = (double *) params;
  double lnL;
  double X;
  double a_dynfric;
  
  x[0] = q[0];  x[1] = q[1];  x[2] = q[2];
  v[0] = q[3];  v[1] = q[4];  v[2] = q[5];
  
  r = quadrature(x);
  a_gravity = -(GRAVITY * mass_enclosed(r) * pow(r,-2));
  phi = atan2(x[1],x[0]);
  theta = atan2(x[2],sqrt(pow(x[1],2) + pow(x[0],2)));

  V = quadrature(v);
  lnL = log(Lambda(r,M[0]));
  X = V / (sqrt(2) * circular_velocity(r));
  if ((V < 1e-20)||(M[1] == 0)||(M[0] == 0)) {
    a_dynfric = 0.0;
  }
  else {
    a_dynfric = (4 * M_PI * lnL * pow(GRAVITY,2) * local_density(r) * M[0]) *
      pow(V,-3) * (erf(X) - (2 * X)/sqrt(M_PI) * exp(-pow(X,2)));
  }
  a[0] = a_gravity * cos(theta) * cos(phi);
  a[1] = a_gravity * cos(theta) * sin(phi);
  a[2] = a_gravity * sin(theta);

  f[0] = v[0];
  f[1] = v[1];
  f[2] = v[2];

  if (M[1] != 0.0) {
    f[3] = a[0] - a_dynfric * v[0]; 
    f[4] = a[1] - a_dynfric * v[1]; 
    f[5] = a[2] - a_dynfric * v[2];
  }
  else {
    f[3] = a[0];
    f[4] = a[1];
    f[5] = a[2];
  }


  f[6] = a_dynfric * v[0];
  f[7] = a_dynfric * v[1];
  f[8] = a_dynfric * v[2];

  return(GSL_SUCCESS);
}

/* RK+PD DiffEQ solver doesn't require this. */
int gsl_df (double t, const double q[], double *dfdy, double dfdr[],
	    void *params) {
  return(GSL_SUCCESS);
}

unsigned int timeseed (void) {
  time_t timeval;
  unsigned char *ptr;
  unsigned seed;
  size_t i;

  timeval = time (NULL);
  ptr = (unsigned char *) &timeval;

  seed = 0;
  for (i = 0; i < sizeof timeval; i++) {
    seed = seed * (UCHAR_MAX + 2U) + ptr[i];
  }
  return(seed);
}

int main (int argc, char *argv[]) {
  double x[3] = {8000,0,0};
  double v[3] = {0,0,0};
  double q[6];
  double opt_t = 1e4;
  double opt_N = 1;
  double opt_D = 0.0;
  double opt_T = 0.0;
  double opt_e = 0.0;
  double tmax,t,ot;
  double r = 0;
  double R_cluster = 0;
  double params[2];
  double M = 0.0;
  double opt_I_sigma = 0;
  char *token;
  char ch;

  const gsl_odeiv_step_type *T = gsl_odeiv_step_rk8pd;
  gsl_odeiv_step *s = gsl_odeiv_step_alloc(T,9);
  gsl_odeiv_control *c = gsl_odeiv_control_y_new(1e-6,0.0);
  gsl_odeiv_evolve *e = gsl_odeiv_evolve_alloc(9);
  gsl_odeiv_system sys;
  double h = 1e-2;
  
  const gsl_rng_type *rT = gsl_rng_default;
  gsl_rng *rng = gsl_rng_alloc(rT);
  
  gsl_rng_set(rng,timeseed());
  
  v[0] = 0;
  v[1] = circular_velocity(8000);
  v[2] = 0;

  while ((ch = getopt(argc,argv,"hx:v:M:T:t:N:r:e:DI")) != -1) {
    switch (ch) {
    case 'h':
      fprintf(stderr,
	      "Usage: dynamical_friction <options>\n"
	      "          [-h]              This help.\n");
      fprintf(stderr,
	      "          [-x x0:y0:z0]     Initial position.\n"
	      "          [-v vx0:vy0:vz0]  Initial velocity.\n"
	      "          [-I]              Use (random) isotropic orbits.\n"
	      "          [-M <mass>]       Initial mass.\n"
	      "          [-D]              Turn on dynamical friction.\n"
	      "          [-N <orbits>]     Number of orbits.\n"
	      "          [-t <tstep>]      Time step.\n"
	      "          [-T <tmax>]       Ignore N-orbits, and eval to time T.\n"
	      "          [-r <radius>]     Set an initial radius.\n"
	      "          [-e <eccentricity>] Set initial eccentricity.\n");
      
      exit(10);
      break;
    case 'x':
      if (r == 0) {
	token = strtok(optarg,":");
	x[0] = (double) atof(token);
	token = strtok(NULL,":");
	x[1] = (double) atof(token);
	token = strtok(NULL,":");
	x[2] = (double) atof(token);
      }
      else {
	fprintf(stderr,"Ignoring -x incantation for -r!\n");
      }
      break;
    case 'v':
      if (r == 0) {
	token = strtok(optarg,":");
	v[0] = (double) atof(token);
	token = strtok(NULL,":");
	v[1] = (double) atof(token);
	token = strtok(NULL,":");
	v[2] = (double) atof(token);
      }
      else {
	fprintf(stderr,"Ignoring -v incantation for -r!\n");
      }
      break;
    case 'I':
      r = quadrature(x);
      opt_I_sigma = circular_velocity(r) /
	sqrt(3.0 - ((log(mass_enclosed(r + 1)) - log(mass_enclosed(r))) /
		    (log(r + 1) - log(r))));
      v[0] = gsl_ran_gaussian(rng,opt_I_sigma);
      v[1] = gsl_ran_gaussian(rng,opt_I_sigma);
      v[2] = gsl_ran_gaussian(rng,opt_I_sigma);
      break;
    case 'M':
      M = (double) atof(optarg);
      opt_D = 1.0;
      break;
    case 't':
      opt_t = atof(optarg);
      break;
    case 'T':
      opt_T = atof(optarg);
      break;    
    case 'N':
      opt_N = atof(optarg);
      break;
    case 'D':
      opt_D = 1.0;
      break;
    case 'r':
      r = (double) atof(optarg);
      x[2] = gsl_ran_flat(rng,-r,r);
      v[1] = gsl_ran_flat(rng,0,2 * M_PI);
      x[0] = sqrt(pow(r,2) - pow(x[2],2)) * cos(v[1]);
      x[1] = sqrt(pow(r,2) - pow(x[2],2)) * sin(v[1]);

      r = quadrature(x);
	
      v[1] = circular_velocity(r);
      v[0] = 0;
      v[2] = 0;
      
      break;
    case 'e':
      opt_e = atof(optarg);
      v[0] = 0;
      v[1] = sqrt(GRAVITY * mass_enclosed(r) * (1 - opt_e) / r);
      v[2] = 0;
      break;
    case '?':
      if (isprint(optopt)) {
	fprintf(stderr, "Unknown option `-%c'.\n",optopt);
      }
      else {
	fprintf(stderr, "Unknown option character `\\x%x'.\n",optopt);
      }
      return(1);
    default:
      fprintf(stderr, "Unknown option `-%c'.\n",optopt);
      return(10);
    }
  }
  R_cluster = pow(M / 124609,1.0 / 3.0);
  printf("#M: %f\n",M);
  printf("#X0: %f %f %f\n",x[0],x[1],x[2]);
  printf("#V0: %f %f %f\n",v[0],v[1],v[2]);

  params[0] = M;
  params[1] = opt_D;
  
  sys.function = &gsl_f;
  sys.jacobian = &gsl_df;
  sys.dimension = 6;
  sys.params = &params;

  gsl_odeiv_evolve_reset(e);
  t = 0.0;
  h = 1e-6;
  q[0] = x[0];
  q[1] = x[1];
  q[2] = x[2];
  q[3] = v[0];
  q[4] = v[1];
  q[5] = v[2];
  q[6] = 0;
  q[7] = 0;
  q[8] = 0;

  r = quadrature(q);
  if (opt_T == 0.0) {
    opt_T = opt_N * (2 * M_PI * r / circular_velocity(r));
  }
  fprintf(stderr,"R:%g v:%.3e N:%g P:%g Tmax:%g Texp:%g M:%.3e R_cluster:%g\n",
	  r,circular_velocity(r),opt_N,(2 * M_PI * r / circular_velocity(r)),opt_T,
	  3.0905e13 / 12 * (circular_velocity(r) * 977813.952) * pow(M,-1) * pow(r/1000,2),
	  M,R_cluster);
  ot = 0;
  /* Yes, I know those aren't really proper accelerations.  I had a format, and needed to fill it. */
  printf("#t          r    v_c(r)   \tx    y     z     \tvx       vy       vz     \tax     ay      az\n");
  for (tmax = 0; tmax <= opt_T; tmax += opt_t) {
    while (t < tmax) {
      gsl_odeiv_evolve_apply(e,c,s,&sys,&t,tmax,&h,q);
      
      x[0] = q[0];
      x[1] = q[1];
      x[2] = q[2];
      r = quadrature(x);
      printf ("%.5e %.2f %g\t%.2f %.2f %.2f\t%.3e %.3e %.3e\t%.3e %.3e %.3e\n",
	      t,r,circular_velocity(r),
	      q[0],q[1],q[2],
	      q[3],q[4],q[5],
	      (q[3] - v[0])/(t - ot),
	      (q[4] - v[1])/(t - ot),
	      (q[5] - v[2])/(t - ot));
      v[0] = q[3];
      v[1] = q[4];
      v[2] = q[5];
      ot = t;
      if (r < R_cluster) {
	t = tmax;
	tmax = opt_T;
      }
    }
  }
   
  return(0);
}
      
    
  
