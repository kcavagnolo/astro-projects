/* Program to parse the radio FITS header and give the conversion factor */

/* Copyright (C) 2006 Martin Hardcastle

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

(1.1) This version corrects a bug which could be triggered by older
AIPS-format FITS files with multiple BMAJ, BMIN history entries. The
last one to be found is the one used by AIPS and (now) by this
program.

(1.2) This version allows the code to operate on more than one file
specified on the command line, i.e. you can use wild cards.

Compile with:
gcc rheader.c -o rheader -L/usr/local/lib -lfuntools -lm

*/


#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <funtools.h>
#include <string.h>

#define GFACTOR (2.0*sqrt(2.0*log(2.0)))

int main(int argc, char *argv[]) {

  Fun infile;
  FITSHead head;
  FITSCard card;
  double delt,delt2;
  int i;
  int got;
  float bmaj=0, bmin=0, bma=0;
  float freq;
  int file;

  char *units;

  if ((argc<2)) {
    fprintf(stderr,"Syntax: %s filename.fits [filename.fits ...]\n",argv[0]);
    exit(2);
  }

  for(file=1; file<argc; file++) {

    infile=FunOpen(argv[file],"r",NULL);

    if (infile==NULL) {
      fprintf(stderr,"failed to open map file %s\n",argv[file]);
      break;
    }

    printf("--- File %s ---\n",argv[file]);

    /* check the units. If they're not Jy/beam, we shouldn't be doing this */
    
    units=FunParamGets(infile, "BUNIT", 0, NULL, &got);
    
    if (got) {
      printf("    Units of the map are %s\n",units);
    } else {
      printf("    Couldn't get map units! Proceed with caution\n");
    }
    
    freq=FunParamGetd(infile, "CRVAL3", 0, 0.0, &got);
    
    if (got) {
      printf("    Frequency is %g Hz\n",freq);
    } else {
      printf("Couldn't get frequency\n");
    }

    delt=FunParamGetd(infile, "CDELT2", 0, 0.0, &got);
    
    if (got) {
      printf("    Pixel size is %f arcsec\n",delt*3600.0);
    } else {
      printf("    Couldn't get pixel size\n");
      break;
    }
    
    delt2=FunParamGetd(infile, "CDELT1", 0, 0.0, &got);
    
    if (got) {
      if (delt2!=-delt) {
	printf("    Pixels aren't square!\n");
	break;
      }
    } else {
      printf("    Couldn't get second pixel size -- is this image data?\n");
      break;
    }
    
    delt*=3600.0;
    
    /* now parse the headers for AIPS's idiotic encoding of the CLEAN
       beam size */
    
    FunInfoGet(infile, FUN_HEADER, &head, 0);
    
    for(i=1; i<=head->ncard; i++){
      card = ft_cardnth(head,i);
      if( card && card->c && *card->c){
	if (!strncmp(card->c, "HISTORY AIPS   CLEAN BMAJ",25)) {
	  /*       	printf("%.80s\n",card->c); */
	  sscanf(card->c+26,"%f",&bmaj);
	  sscanf(card->c+44,"%f",&bmin);
	  sscanf(card->c+61,"%f",&bma);
	  bmaj*=3600;
	  bmin*=3600;
	}
      }
    }
    
    if (bmaj>0.0) {
      printf("    Beam major axis %f arcsec, minor axis %f arcsec, angle %f\n",bmaj,bmin,bma);
      printf("    Beam area is %f pixels\n",2.0*PI*(bmaj*bmin)/(GFACTOR*GFACTOR*delt*delt));
    } else {
      printf("    Failed to find CLEAN beam parameters -- is this a radio map?\n");
    }
    
    FunClose(infile);
  
  }
  return 0;
}
  
