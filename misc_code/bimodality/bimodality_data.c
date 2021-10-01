/* Copyright (C) 2007 Christopher Waters <watersc1@pa.msu.edu> */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <unistd.h>
#include <ctype.h>


#include "bimodality.h"

int random_index(int N) {
  double R = rand() / (RAND_MAX + 1.0);
  return(N * R);
}

bin *allocate_bin(int N) {
  bin *B = malloc(sizeof(bin));
  B->x   = malloc(N * sizeof(double));
  B->N   = N;

  return(B);
}

bin *reallocate_bin(bin *B, int N) {
  bin *new = allocate_bin(N);
  int i;

  for (i = 0; i < B->N; i++) {
    if (i < new->N) {
      new->x[i] = B->x[i];
    }
  }
  new->y = B->y;

  free(B);
  return(new);
}

void free_bin(bin *B) {
  free(B->x);
  free(B);
}

bin *resample_bin(bin *B) {
  bin *out = allocate_bin(B->N);
  int i,j;

  for (i = 0; i < B->N; i++) {
    j = random_index(B->N);
    out->x[i] = B->x[j];
  }

  return(out);  
}


data *read_data_guts(char *filename, int *N) {
  FILE *datafile;
  char line[240];
  int i = 0;
  int v;
  data *F;
  datafile = fopen(filename,"r");
  *N = 10;
  F = malloc(*N * sizeof(data));

  while(fgets(line,240,datafile)) {
    v = sscanf(line,"%lf %lf\n",&(F[i].x),&(F[i].y));
    if (v != -1) {
      if (v == 1) {
	F[i].y = 0.0;
      }
/*       printf("%g %g %d %d\n",F[i].x,F[i].y,i,v); */
      i++;
      if (i >= *N) {
	*N += 10;
	F = realloc(F,*N * sizeof(data));
      }
    }
  }
  *N = i;

  fclose(datafile);
  return(F);
}

int data_cmp (const void *c1, const void *c2) {
  const data *d1 = (const data *)c1;
  const data *d2 = (const data *)c2;

  if (d1->y < d2->y) {
    return(-1);
  }
  else if (d1->y == d2->y) {
    return(0);
  }
  else {
    return(1);
  }
}
  
bin **read_data_null(char *filename, int *k) {
  data *F;
  int i;
  int N;
  bin **B;

  F = read_data_guts(filename,&N);

  B = malloc(1 * sizeof(bin *));
  B[0] = allocate_bin(N);

  B[0]->y = 0;
      
  for (i = 0; i < B[0]->N; i++) {

    B[0]->x[i] = F[i].x;
    B[0]->y += F[i].y;
  }
  free(F);

  *k = 1;
  B[0]->y /= B[0]->N;
  return(B);
}    

bin **read_data_fixed_number(char *filename, int M,
			     int *k) {
  data *F = 0;
  int i,j;
  int N;
  int n;
  bin **B;
  F = read_data_guts(filename,&N);

  /* Allocate the bin structures. */
  *k = N / M + 1;
  B = malloc(*k * sizeof(bin *));
  for (i = 0; i < *k; i++) {
    B[i] = allocate_bin(M);
  }
  printf("here!\n");
  fflush(stdout);
  /* Sort the data. */
  qsort(F,N,sizeof(data),&data_cmp);
  
  /* Fill the bins. */
  for (i = 0; i < *k; i++) {
    n = 0;
    B[i]->y = 0;
    for (j = 0; j < M; j++) {
      if (i * *k + j < N) {
	B[i]->x[j] = F[i * *k + j].x;
	B[i]->y += F[i * *k + j].y;
	n++;
      }
    }
    B[i]->N = n;
    B[i]->y /= n;
  }

  free(F);
  return(B);
}

bin **read_data_fixed_width(char *filename, double yi, double yf, double dy,
			    int *k) {
  data *F = 0;
  int N;
  
  bin **B;
  int *bin_width;
  int *bin_count;
  int i,j;
  
  F = read_data_guts(filename,&N);
  qsort(F,N,sizeof(data),&data_cmp);
  *k = (yf - yi) / dy;
/*   if (((yf - yi) % dy) != 0) { */
/*     *k++; */
/*   } */
  B = malloc(*k * sizeof(bin *));
  bin_width = malloc(*k * sizeof(int));
  bin_count = malloc(*k * sizeof(int));
  for (i = 0; i < *k; i++) {
    bin_width[i] = 10;
    bin_count[i] = 0;
    B[i] = allocate_bin(bin_width[i]);
    B[i]->y = 0;
  }

  for (j = 0; j < N; j++) {
    i = (int) floor( (F[j].y - yi) / dy);
#ifdef DEBUG
    printf("%g %g %d %g\n",F[j].x,F[j].y,i,yi + 1.0 * i * dy);
#endif
    if ((i >= 0)&&(i < *k)) {
      B[i]->x[bin_count[i]] = F[j].x;
      B[i]->y += F[j].y;
      bin_count[i] ++;

      if (bin_count[i] >= bin_width[i]) {
	bin_width[i] += 10;
	B[i] = reallocate_bin(B[i],bin_width[i]);
      }
    }
  }
  for (i = 0; i < *k; i++) {
    if (bin_count[i] == 0) {
      B[i]->y = 0;
    }
    else {
      B[i]->y /= bin_count[i];
    }
    B[i] = reallocate_bin(B[i],bin_count[i]);
#ifdef DEBUG
    printf("%d %d %g %g %g %g\n",i,bin_count[i],B[i]->y,yi,yf,dy);
    for (j = 0; j < B[i]->N; j++) {
      printf("\t%d %g\n",j,B[i]->x[j]);
    }
#endif    
  }

  
  free(F);
  free(bin_width);
  free(bin_count);
#ifdef DEBUG
#ifdef ABORT
  exit(10);
#endif
#endif
  return(B);  
}

bin **read_data_running_bin(char *filename, int M, int *k) {
  data *F = 0;
  int N;

  bin **B;

  int i,j,n;
  int *counts;
  F = read_data_guts(filename,&N);
  qsort(F,N,sizeof(data),&data_cmp);

  *k = N - M + 1;
  B = malloc(*k * sizeof(bin *));
  counts = malloc(*k * sizeof(int));
  for (i = 0; i < *k; i++) {
    B[i] = allocate_bin(M);
    B[i]->y = 0;
    counts[i] = 0;
  }
  
  for (j = 0; j < N; j++) {
    for (i = 0; i < *k; i++) {
      n = j - i;
/* #ifdef DEBUG */
/*       printf("%d %d %d %d %d %g %g\n", */
/* 	     j,i,n,N,*k,F[j].x,F[j].y); */
/* #endif */
      if (n < M) {
	if (n >= 0) {
	  B[i]->x[n] = F[j].x;
	  B[i]->y    += F[j].y;
	  counts[i]++;
	}
	else {
	  i = *k;
	}
      }
    }
  }
  for (i = 0; i < *k; i++) {
    if (counts[i] != 0) {
      B[i]->y /= counts[i];
    }
    else {
      B[i]->y = 0;
    }
  }
  
#ifdef DEBUG
  for (i = 0; i < *k; i++) {
    printf("%d %g\n",i,B[i]->y);
    for (j = 0; j < B[i]->N; j++) {
      printf("\t%d %g\n",j,B[i]->x[j]);
    }
  }
#endif    
  free(counts);
  free(F);
#ifdef DEBUG
#ifdef ABORT
  exit(10);
#endif
#endif
  return(B);
  
}

/* void read_data_fixed_width(char *filename, double y0, double dy, */
/* 			    bin *B, int *k, bin *Y) { */
/*   data F; */
/*   int i,j; */
/*   int N; */
/*   int n; */
  
/*   read_data_guts(filename,F,&N); */
/*   /\* Sort the data. *\/ */
/*   qsort(F,sizeof(data),N,&data_cmp); */
  
/*   *k = ((F[N - 1]->y - y0) / dy) + 1; */
  
/*   /\* Allocate the bin structures. *\/ */
/*   B = malloc(*k * sizeof(bin *)); */
/*   for (i = 0; i < *k; i++) { */
/*     B[i] = allocate_bin(M); */
/*   } */
  
/*   Y = allocate_bin(*k); */
  
/*   /\* Fill the bins. *\/ */
/*   for (i = 0; i < *k; i++) { */
/*     n = 0; */
/*     Y[i] = 0; */
/*     for (j = 0; j < M; j++) { */
/*       if (i * *k + j < N) { */
/* 	B[i]->x[j] = F[i * *k + j]->x; */
/* 	Y[i] += F[i * *k + j]->y; */
/* 	n++; */
/*       } */
/*     } */
/*     B[i]->N = n; */
/*     Y[i] /= n; */
/*   } */

/*   free(F); */
/* } */

