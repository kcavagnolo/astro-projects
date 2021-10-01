/* compy
/* This program reads in the simulation outputs and calculates 
/* the line-of-sight integral of the Compton-y.
/* This program is set up to work on Andrey's ART simulations.
/*
/* Last updated: Nov 12, 2001 */

#include "cpgplot.h"
#include "localdefs.h"

main(argc,argv)
        int argc;
        char *argv[];
{
        int     i,j,k,ix,jx,kx,*nx,nsize;
        char    rhofile[200],tfile[200],outfile[200],trash[200],los;
        float   aexpn,*dx,***rho,***t,***f3tensor();
	float   fe,compy,norm_compy,yave,rout,rr,rc;
	FILE *inp,*out,*pip;
	void    free_f3tensor(),free_matrix(),printerror();
	void    Read_Cube();

	/* Input parameters */
        if (argc<5) {printf("Input <rho file> <temp file> <out file> <Axis of l.o.s. integral (i.e., x,y,z) >\n");exit(0);}
	sprintf(rhofile,"%s",argv[1]);
	sprintf(tfile,"%s",argv[2]);
	sprintf(outfile,"%s",argv[3]);
	sscanf(argv[4],"%s",&los);

	/* Define vectors & matrix */
	nsize = 256;
	nx=(int *)calloc(3,sizeof(int));
	dx=(float *)calloc(3,sizeof(float));
        rho=f3tensor(1,nsize,1,nsize,1,nsize);
	t=f3tensor(1,nsize,1,nsize,1,nsize);	

	/* Read in data */
	Read_Cube ( rhofile, rho, &aexpn, nx, dx );
	Read_Cube ( tfile, t, &aexpn, nx, dx );

	/* Print input data */
	printf("Om %5.3f Ol %5.3f Ob %5.3f h %5.3f \n",OmegaM,OmegaL,OmegaB,h);
	printf("aexpn %f nx %d ny %d nz %d \n",aexpn,nx[0],nx[1],nx[2]);
	printf("dx %f dy %f dz %f \n",dx[0],dx[1],dx[2]);
	puts("Finished reading Simulation Binary data!");
	puts(" ");

	/* Output the Compton y map to Binary datafile  */
	/* C-compatible array format with the last argument changing the fastest */
	printf("Generate the Compton-y map:  %s \n",&outfile);
	out=fopen(outfile,"wb");
	/* Print header */
	/* fprintf(out,"%f %d %d %d \n",aexpn,nx[0],nx[1],nx[2]); 
	   fprintf(out,"%e %e %e \n",dx[0],dx[1],dx[2]); 
	   fprintf(out,"%f %f %f %f \n",OmegaM,OmegaL,OmegaB,h); */
	fwrite(&OmegaM,sizeof(float),1,out); 
	fwrite(&OmegaL,sizeof(float),1,out); 
	fwrite(&OmegaB,sizeof(float),1,out); 
	fwrite(&h,sizeof(float),1,out);
	fwrite(&aexpn,sizeof(aexpn),1,out);
	fwrite(nx,sizeof(int),3,out);
	fwrite(dx,sizeof(float),3,out); 
	yave=0;
	rc=(nsize+1)/2.0;
	rout=dx[0]*nsize/2.0;
	rout=10.0;
	/* Normalization constant for the Compton-y */
	fe=(1.0-0.5*YNOW);
	norm_compy=(fe*MSOLAR/M_P)/Squ(MPC_2_CM)*THOMSON*kB/(M_E*Squ(C*1.0e+05));
	/* Calculate the Compton-y map by integrating along l.o.s. */
	printf("l.o.s. integral along %c-axis \n",los);
	if (los=='x' || los=='X') {
	  for (j=1;j<=nsize;j++) for (k=1;k<=nsize;k++) {
	    compy=0.0;
	    for (i=1;i<=nsize;i++) {
	      /* Integrate along x-axis (or i-axis) [Msol Mpc^-2 Kelvin] */
	      rr = dx[0]*sqrt(Squ(i-rc)+Squ(j-rc)+Squ(k-rc)); /* [comoving Mpc/h] */
	      if ( rr < rout ) { compy+=rho[i][j][k]*t[i][j][k]*(dx[0]/h); }
	      /* if ( rr < rout ) printf("%d %d %d %f %f %f\n",i,j,k,rc,rr,rout); */	      
	    }
	    /* Print Compy in dimensionless units */
	    compy*=norm_compy;
	    fwrite(&compy,sizeof(float),1,out); 
	    /* fprintf(out,"%e ",compy); */ 
	    yave+=compy/Squ(nsize);
	  }
	}
	else if (los=='y' || los=='Y') { 
	  for (k=1;k<=nsize;k++) for (i=1;i<=nsize;i++) {
	    compy=0.0;
	    for (j=1;j<=nsize;j++) {
	      /* Integrate along y-axis (or j-axis) [Msol Mpc^-2 Kelvin] */
	      rr = dx[0]*sqrt(Squ(i-rc)+Squ(j-rc)+Squ(k-rc)); /* [comoving Mpc/h] */
	      if ( rr < rout ) { compy+=rho[i][j][k]*t[i][j][k]*(dx[1]/h); }
	    }
	    /* Compton-y is now made dimensionless */  
	    compy*=norm_compy;
	    fwrite(&compy,sizeof(float),1,out); 
	    /* fprintf(out,"%e ",compy); */ 
	    yave+=compy/Squ(nsize);
	  }
	}
	else if (los=='z' || los=='Z') {
	  for (i=1;i<=nsize;i++) for (j=1;j<=nsize;j++) {
	    compy=0.0;
	    for (k=1;k<=nsize;k++) {
	      /* Integrate along z-axis (or k-axis) [Msol Mpc^-2 Kelvin] */
	      rr = dx[0]*sqrt(Squ(i-rc)+Squ(j-rc)+Squ(k-rc)); /* [comoving Mpc/h] */
	      if ( rr < rout ) { compy+=rho[i][j][k]*t[i][j][k]*(dx[2]/h); }
	    }
	    /* Compton-y is now made dimensionless */  
	    compy*=norm_compy;
	    fwrite(&compy,sizeof(float),1,out); 
	    /* fprintf(out,"%e ",compy); */ 
	    yave+=compy/Squ(nsize);
	  }
	}
   	else { puts("<Axis for the l.o.s. integral?>"); exit(0); }
        fclose(out);
	printf("<y> %e \n",yave); 

	free_f3tensor(rho,1,nsize,1,nsize,1,nsize);
	free_f3tensor(t,1,nsize,1,nsize,1,nsize);
	return (0);
}
