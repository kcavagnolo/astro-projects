/* Copyright (C) 2007 Christopher Waters <watersc1@pa.msu.edu> */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <unistd.h>
#include <getopt.h>
#include <ctype.h>

#include "bimodality.h"

#define MAX_ITER 1000
#define DEBUG 1
int main(int argc, char *argv[]) {
  bin **B = 0;
  bin *working = 0;
  int Nbins;

  int i,j,k;
  double logL_unimodal;
  double mU,sU;

  double logL_bimodal;
  double pi1 = 0.5,m1 = 0.0,s1 = 1.0;
  double pi2 = 0.5,m2 = 2.75,s2 = 1.0;

  double *P1,*P2;

  double S = 0;
  double dL,oldL;
  /* Command line option parameters                                         */
  int c;
  int opt_binning = 0;
  double opt_y0 = 0, opt_yf = 1.0, opt_dy = 0.1;
  int opt_fixedN = 10;  
  int opt_iter = 1;
  int opt_bumping = 0;
  int opt_scedastic = 1;
  int opt_probability = 0;
  int opt_index = 0;
  /* Bootstrapping values. */
  double *final_L_unimodal;
  double *final_L_bimodal;
  double *final_m1;
  double *final_m2;
  double *final_s1;
  double *final_s2;
  double *final_pi1;
  double *final_pi2;

  double *final_LLR;
  double *final_P;
  int test_run;
  int test_max = 0;
  static struct option long_options[] = {
    {"help", no_argument, 0, 'h'},
    {"fixed-width", no_argument, 0, 'w'},
    {"bin-start",required_argument, 0, 'y'},
    {"bin-end",required_argument, 0, 'Y'},
    {"bin-width",required_argument, 0, 'd'},
    {"fixed-number", no_argument, 0, 'n'},
    {"running-bins", no_argument, 0, 'r'},
    {"bin-number",required_argument, 0, 'N'},
    {"iterations",required_argument, 0, 'i'},
    {"bumping",no_argument, 0, 'B'},
    {"heteroscedastic",no_argument, 0, 't'},
    {"homoscedastic",no_argument, 0, 'm'},
    {"testoscedastic",no_argument, 0, 'T'},
    {"probability",no_argument, 0, 'p'},
    {0, 0, 0, 0}
  };

  /* Parse command line options.                                             */
  while ((c = getopt_long(argc,argv,"hwy:Y:d:nrN:i:BtmT1:2:p",
			  long_options,&opt_index)) != -1) {
    switch (c) {
    case 'h':
      fprintf(stderr,
	      "Usage:  bimodality [OPTIONS] <input data>\n"
	      "\n");
      fprintf(stderr,
	      "  -h, --help               Display this help.\n"
	      "  -w, --fixed-width        Use fixed width bins.\n"
	      "  -y, --bin-start <Y0>     Starting point for fixed width bins.\n"
	      "  -Y, --bin-end <YF>       End point for fixed width bins.\n"
	      "  -d, --bin-width <dY>     Width of bins.\n"
	      "  -n, --fixed-number       Use bins containing a fixed number of points.\n"
	      "  -r, --running-bins       Use running bins containing a fixed number of points.\n"
	      "  -N, --bin-number <N>     Use this many points in each bin.\n");
      fprintf(stderr,
	      "  -i, --iterations <i>     Use this many iterations to calculate errors.\n"
	      "  -B, --bumping            Use the 'bootstrap bumping' method.\n"
	      "  -t, --heteroscedastic    Assume different variances for the two populations.\n"
	      "  -m, --homoscedastic      Assume the same variances for the two populations.\n"
	      "  -T  --testoscedastic     Calculate both, and test scedasticity.\n"
	      "  -p, --probability        Dump a table of the input values with their Z_{ij}.\n"
	      );
      
      return(10);
      break;
    case '1':
      pi1 = atof(strtok(optarg,":"));
      m1  = atof(strtok(NULL,":"));
      s1  = atof(strtok(NULL,":"));
      break;
    case '2':
      pi2 = atof(strtok(optarg,":"));
      m2  = atof(strtok(NULL,":"));
      s2  = atof(strtok(NULL,":"));
      break;
    case 'w':
      opt_binning = 1;
      break;
    case 'n':
      opt_binning = 2;
      break;
    case 'r':
      opt_binning = 3;
      break;
    case 't':
      opt_scedastic = 1;
      break;
    case 'm':
      opt_scedastic = 0;
      break;
    case 'T':
      opt_scedastic = 2;
      test_max = 1;
      break;
    case 'y':
      opt_y0 = atof(optarg);
      break;
    case 'Y':
      opt_yf = atof(optarg);
      break;
    case 'd':
      opt_dy = atof(optarg);
      break;
    case 'N':
      opt_fixedN = atoi(optarg);
      break;
    case 'i':
      opt_iter = atoi(optarg);
      break;
    case 'B':
      opt_bumping = 1;
      break;
    case 'p':
      opt_probability = 1;
      opt_binning = 0;
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

  /* Read data file. */
  if (opt_binning == 0) {
    B = read_data_null(argv[optind],&Nbins);
    B[0]->y = 0;
  }
  else if (opt_binning == 1) {
    B = read_data_fixed_width(argv[optind],opt_y0,opt_yf,opt_dy,
			      &Nbins);
  }
  else if (opt_binning == 2) {
    B = read_data_fixed_number(argv[optind],opt_fixedN,
			       &Nbins);
  }
  else if (opt_binning == 3) {
    B = read_data_running_bin(argv[optind],opt_fixedN,&Nbins);
  }

#ifdef DEBUG
  fprintf(stderr,
	  "#%4s %4s:%4s %5s %5s %5s %7s %7s %5s %5s %4s %4s %4s %4s %3s\n",
	  "i","j","N","y","mU","sU","LU",
	  "LB","m1","m2","s1","s2","pi1","pi2","k");
#endif
  printf("#Y\tmU\tsU\t"
	 "m1 dm1 s1 ds1 pi1 dpi1\t"
	 "m2 dm2 s2 ds2 pi2 dpi2\t"
	 "LU LB Puni N\n");
  for (i = 0; i < Nbins; i++) {
    if (B[i]->N > 0) {
      final_L_unimodal = malloc(opt_iter * sizeof(double));
      final_L_bimodal  = malloc(opt_iter * sizeof(double));
      final_m1         = malloc(opt_iter * sizeof(double));
      final_m2         = malloc(opt_iter * sizeof(double));
      final_s1         = malloc(opt_iter * sizeof(double));
      final_s2         = malloc(opt_iter * sizeof(double));
      final_pi1        = malloc(opt_iter * sizeof(double));
      final_pi2        = malloc(opt_iter * sizeof(double));
      final_LLR        = malloc(opt_iter * sizeof(double));
      final_P          = malloc(opt_iter * sizeof(double));
      
      for (j = 0; j < opt_iter; j++) {
	/* Rebin the data. */
	if (opt_iter > 1) {
	  working = resample_bin(B[i]);
	}
	else {
	  working = B[i];
	}
	/* If you're reading this, then know now that this right here makes */
	/* me sick to my stomach.  However, I can't come up with any other  */
	/* good solution right now.  So, fuck off.                          */
	for (test_run = 0; test_run <= test_max; test_run++) {
	  /* ************************************************************** */
	  /* INITIALIZATION                                                 */
	  /* ************************************************************** */
	  /* Calculate Unimodal form: */
	  logL_unimodal = kmm_unimodal(working,&mU,&sU);
#ifdef DEBUG	
	  fprintf(stderr,"#%4d %4d: %d %5.3g %5.3g %4.3g %7.5g ",
		  i,j,working->N,working->y,mU,sU,logL_unimodal);
#endif
	  /* Initialize bimodal form; */
	  P1 = malloc(working->N * sizeof(double));
	  P2 = malloc(working->N * sizeof(double));
	  
	  /* These should be read from options. */
	  m1 = 0;
	  m2 = 0;
	  s1 = 0;
	  s2 = 0;
	  pi1 = 0;
	  pi2 = 0;
	  for (k = 0; k < working->N; k++) {
	    if (working->x[k] < mU) {
	      P1[k] = 1;
	      P2[k] = 0;
	    }
	    else {
	      P1[k] = 0;
	      P2[k] = 1;
	    }
	    m1 += working->x[k] * P1[k];
	    m2 += working->x[k] * P2[k];
	    
	    pi1 += P1[k];
	    pi2 += P2[k];
	  }
	  m1 /= pi1;
	  m2 /= pi2;
	  
	  for (k = 0; k < working->N; k++) {
	    s1 += pow((working->x[k] - m1),2) * P1[k];
	    s2 += pow((working->x[k] - m2),2) * P2[k];
	  }
	  s1 = sqrt(s1 / pi1);
	  s2 = sqrt(s2 / pi2);
	  
	  pi1 /= working->N;
	  pi2 /= working->N;
	  
	  fflush(stdout);
	  /* *************************************************************** */
	  /* ITERATION                                                       */
	  /* *************************************************************** */
	  dL = 10;
	  oldL = -999;
	  k = 0;
	  logL_bimodal = logL_unimodal;
	  while (((dL > 1e-6)||(k < 3))&&(k < MAX_ITER)) {
	    k++;
	    dL = fabs(logL_bimodal - oldL);
	    oldL = logL_bimodal;
	    
	    kmm_calculate_P(working,P1,P2,
			    pi1,m1,s1,pi2,m2,s2);
	    logL_bimodal = kmm_calculate_M(working,P1,P2,
					   &pi1,&m1,&s1,&pi2,&m2,&s2);
	    
	    if ((opt_scedastic == 0)||
		((opt_scedastic == 2)&&(test_run == 0))) {
	      S = sqrt(pi1 * s1 * s1 + pi2 * s2 * s2);
	      s1 = S;
	      s2 = S;
	    }
	  }
#ifdef DEBUG	
	  fprintf(stderr,"%7.5g %5.3g %5.3g %4.3g %4.3g %4.3g %4.3g %3d\n",
		  logL_bimodal,m1,m2,s1,s2,pi1,pi2,k);
#endif

	  /* *************************************************************** */
	  /* SAVE VALUES                                                     */
	  /* *************************************************************** */
	  if (((opt_scedastic == 0)||(opt_scedastic == 1))||
	      ((opt_scedastic == 2)&&(test_run == 0))) {
	    if ((m1 == m1)&&(m2 == m2)&&(s1 == s1)&&(s2 == s2)) {
	      final_L_unimodal[j] = logL_unimodal;
	      final_L_bimodal[j]  = logL_bimodal;
	      final_m1[j]         = m1;
	      final_m2[j]         = m2;
	      final_s1[j]         = s1;
	      final_s2[j]         = s2;
	      final_pi1[j]        = pi1;
	      final_pi2[j]        = pi2;
	    }
	    else {
	      if (opt_scedastic != 2) {
		j--;
	      }
	    }
	    if (opt_iter > 1) {
	      if (opt_scedastic != 2) {
		free_bin(working);
	      }
	    }
	  }
	  else if ((opt_scedastic == 2)&&(test_run == 1)) {
	    final_LLR[j] = -2.0 * (final_L_bimodal[j] - logL_bimodal);
	    if (j == 0) {
	      final_P[j] = 1;
	    }
	    else {
	      if (final_LLR[j] >= final_LLR[0]) {
		final_P[j] = 1;
	      }
	      else {
		final_P[j] = 0;
	      }
	    }
#ifdef DEBUG
	    fprintf(stderr,"##### %g %g -> %g @ %g -> %g\n",
		    final_L_bimodal[j],logL_bimodal,final_LLR[j],final_LLR[0],
		    mean(final_P,j));
#endif
	    free_bin(working);
	  }
	} /* End tests */
      } /* End bootstrap iterations */
      /* ***************************************************************** */
      /* RETURN THIS BIN'S VALUES.                                         */
      /* ***************************************************************** */
      if (opt_probability == 1) {
	printf("#");
      }
      if (opt_bumping == 1) {
	j = max_index(final_L_bimodal,opt_iter);
	printf("%g\t%g %g\t"
	       "%g %g\t%g %g\t%g %g\t"
	       "%g %g\t%g %g\t%g %g\t"
	       "%g %g %g %d\n",
	       B[i]->y,mU,sU,
	       
	       final_m1[j],stdev(final_m1,opt_iter),
	       final_s1[j],stdev(final_s1,opt_iter),
	       final_pi1[j],stdev(final_pi1,opt_iter),

	       final_m2[j],stdev(final_m2,opt_iter),
	       final_s2[j],stdev(final_s2,opt_iter),
	       final_pi2[j],stdev(final_pi2,opt_iter),

	       mean(final_L_unimodal,opt_iter),final_L_bimodal[j],
	       p_unimodal(mean(final_L_unimodal,opt_iter),
			  final_L_bimodal[j],
			  2 + 2 * opt_scedastic),
	       B[i]->N);	       
      }
      else if (opt_scedastic == 2) {
	printf("%g\t%g %g\t"
	       "%g %g\t%g %g\t%g %g\t"
	       "%g %g\t%g %g\t%g %g\t"
	       "%g %g %g %d %g\n",
	       B[i]->y,mU,sU,
	       
	       mean(final_m1,opt_iter),stdev(final_m1,opt_iter),
	       mean(final_s1,opt_iter),stdev(final_s1,opt_iter),
	       mean(final_pi1,opt_iter),stdev(final_pi1,opt_iter),
	       
	       mean(final_m2,opt_iter),stdev(final_m2,opt_iter),
	       mean(final_s2,opt_iter),stdev(final_s2,opt_iter),
	       mean(final_pi2,opt_iter),stdev(final_pi2,opt_iter),
	       
	       mean(final_L_unimodal,opt_iter),mean(final_L_bimodal,opt_iter),
	       p_unimodal(mean(final_L_unimodal,opt_iter),
			  mean(final_L_bimodal,opt_iter),
			  2 + 2 * opt_scedastic),
	       B[i]->N,mean(final_P,opt_iter));
      }
      else {
	printf("%g\t%g %g\t"
	       "%g %g\t%g %g\t%g %g\t"
	       "%g %g\t%g %g\t%g %g\t"
	       "%g %g %g %d\n",
	       B[i]->y,mU,sU,
	       
	       mean(final_m1,opt_iter),stdev(final_m1,opt_iter),
	       mean(final_s1,opt_iter),stdev(final_s1,opt_iter),
	       mean(final_pi1,opt_iter),stdev(final_pi1,opt_iter),
	       
	       mean(final_m2,opt_iter),stdev(final_m2,opt_iter),
	       mean(final_s2,opt_iter),stdev(final_s2,opt_iter),
	       mean(final_pi2,opt_iter),stdev(final_pi2,opt_iter),
	       
	       mean(final_L_unimodal,opt_iter),mean(final_L_bimodal,opt_iter),
	       p_unimodal(mean(final_L_unimodal,opt_iter),
			  mean(final_L_bimodal,opt_iter),
			  2 + 2 * opt_scedastic),
	       B[i]->N);	       
      }

      if (opt_probability == 1) {
	printf("#x_i\tP1i\tP2i\n");
	for (j = 0; j <= B[i]->N; j++) {
	  printf("%g\t%g\t%g\n",B[i]->x[j],P1[j],P2[j]);
	}
      }		 
      
      free(final_L_unimodal);
      free(final_L_bimodal);
      free(final_m1);
      free(final_m2);
      free(final_s1);
      free(final_s2);
      free(final_pi1);
      free(final_pi2);    
      
    }
  }
  return(0);
}
	

      
