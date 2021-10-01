/* read_data.c                                                               */
/* $Id: read_data.c,v 1.1 2009-12-04 22:28:25 cavagnolo Exp $ */
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

#include "./read_data.h"

data *read_data(char *filename) {
  data *D = malloc(sizeof(data));
  FILE *infile;
  long i;
  char line[81];
  
  infile = fopen(filename,"r");
  if (infile == NULL) {
    fprintf(stderr,"read_data.c : Failed to open file: %s.\n",filename);
  }
  D->number = 10;

  D->nu = malloc(sizeof(double) * D->number);
  if (D->nu == NULL) {
    fprintf(stderr,"read_data.c : Failed to alloc `nu' array.\n");
  }
  D->flux = malloc(sizeof(double) * D->number);
  if (D->flux == NULL) {
    fprintf(stderr,"read_data.c : Failed to alloc `flux' array.\n");
  }
  D->errors = malloc(sizeof(double) * D->number);
  if (D->errors == NULL) {
    fprintf(stderr,"read_data.c : Failed to alloc `errors' array.\n");
  }

  i = 0;
  while (fgets(line,80,infile) != NULL) {
    sscanf(line,"%lf %lf %lf\n",D->nu + i, D->flux + i, D->errors + i);

    if (i >= D->number) {
      D->number += 10;

      D->nu = realloc(D->nu,sizeof(double) * D->number);
      if (D->nu == NULL) {
	fprintf(stderr,"read_data.c : Failed to realloc `nu' array.\n");
      }
      D->flux = realloc(D->flux,sizeof(double) * D->number);
      if (D->flux == NULL) {
	fprintf(stderr,"read_data.c : Failed to realloc `flux' array.\n");
      }
      D->errors = realloc(D->errors,sizeof(double) * D->number);
      if (D->errors == NULL) {
	fprintf(stderr,"read_data.c : Failed to realloc `errors' array.\n");
      }
    }
    i++;

  }
  fclose(infile);
  D->number = i;
  D->nu = realloc(D->nu,sizeof(double) * D->number);
  if (D->nu == NULL) {
    fprintf(stderr,"read_data.c : Failed to realloc `nu' array.\n");
  }
  D->flux = realloc(D->flux,sizeof(double) * D->number);
  if (D->flux == NULL) {
    fprintf(stderr,"read_data.c : Failed to realloc `flux' array.\n");
  }
  D->errors = realloc(D->errors,sizeof(double) * D->number);
  if (D->errors == NULL) {
    fprintf(stderr,"read_data.c : Failed to realloc `errors' array.\n");
  }

  return(D);
}
void free_data(data *D) {
  free(D->nu);
  free(D->flux);
  free(D->errors);
  free(D);
}
  
