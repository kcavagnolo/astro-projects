/* profile2d.c
/* This program reads in the simulation outputs and calculates 
/* the 3D radial profiles of the X-ray surface brightness, 
/* emission weighted temperature, entropy, thermal energy per unit volume.
/* This program is set up to work on Andrey's ART simulations.
/*
/* Last updated: Nov 12, 2001 */

#include "cpgplot.h"
#include "localdefs.h"

main(argc,argv)
        int argc;
        char *argv[];
{
	int     i,j,k,ix,jx,kx,ip,np,*nx,nmax,l,lbase,flag;
	int     n,nrmax,*ivector();
	char    infile[200],outfile[200],command[200],devout[200];
	char    devin[200],prompt[200];
	double  *lx,*dvector();
	float   xc,yc,a1,a2,dr,dlnr,r,*rbin,*pbin,*cntbin,mapvalue;
	float   zsim,sum,psum,*pbin_i,*cntbin_i;
	float   *xx,*yy,*zz,rp,pp,gx,gy,ran1();
        float   aexpn,junk,*dx,vol1,vol2,min(),max(),logx(),*vector();
	float   **map,**matrix(),*vector(),rmin,rmax,fmin=1.0e+30,fmax=-1.0e+30;
	float   zobs,photons,exposure;
	FILE    *inp,*out,*pip;
	void    free_matrix(),printerror(),free_vector(),free_dvector(),free_ivector();
	void    fiddle_menu(),sort_image(),show_image(),show_image_nowedge();
	time_t seed; 
        long iseed;


	/* Input parameters & datafile */
	Flag_zoom=0;
	strcpy(prompt,"Input <map binary file> <# radial bins [pix]> <xc> <yc> \n");
	if (argc==5) { /* Manually specify the cluster center */
	  sprintf(infile,"%s",argv[1]);
	  sscanf(argv[2],"%d",&nrmax);
	  sscanf(argv[3],"%f",&xc);
	  sscanf(argv[4],"%f",&yc);
	} 
	else if (argc==3) { /* Identify the max peak as a cluster center */
	  sprintf(infile,"%s",argv[1]);
	  sscanf(argv[2],"%d",&nrmax);
	  xc=yc=NGRID/2;
	}
	else {
	  printf("%s",prompt);
	  exit(0);
	}

	/* Set Flags */
	if (!strncmp(&(infile[0]),"xsbp",4)) {
	  if (!strncmp(&(infile[strlen(infile)-4]),"cnts",4)) flag=XCNTS;
	  else if (!strncmp(&(infile[strlen(infile)-7]),"chandra",7)) flag=Chandra;
	  else if (!strncmp(&(infile[strlen(infile)-4]),"pspc",4)) flag=PSPC;
	  else flag=XCGS; 
	}
	else if (!strncmp(&(infile[0]),"tew",3)) flag=TEW;
	else if (!strncmp(&(infile[0]),"zew",3)) flag=ZEW;
	else if (!strncmp(&(infile[0]),"compy",4)) {
	  if (!strncmp(&(infile[strlen(infile)-4]),"spt",3)) flag=SPT;
	  else if (!strncmp(&(infile[strlen(infile)-4]),"sza",3)) flag=SZA;
	  else flag=COMPY;
	}
	else { puts("Check your input file name!");  exit(0); }	

	/* Define vectors */
	nx=(int *)calloc(3,sizeof(int));
	dx=(float *)calloc(3,sizeof(float));

	/* Read in 2D maps */
	if (flag==TEW) 
	  printf("Read in Tew :  %s \n",&infile);
	if (flag==ZEW) 
	  printf("Read in Zew :  %s \n",&infile);
	else if (flag==COMPY || flag==SPT || flag==SZA) 
	  printf("Read in Compton-y :  %s \n",&infile);
	else 
	  printf("Read in XSBP :  %s \n",&infile);
	inp=fopen(infile,"rb");
	/* Read and Print header */
	fscanf(inp,"%f %f %f %f \n",&OmegaM,&OmegaL,&OmegaB,&h);
	fread(&aexpn,sizeof(aexpn),1,inp);
	fread(nx,sizeof(int),3,inp);
	fread(dx,sizeof(float),3,inp);	
	if (aexpn>1.0) aexpn=1.0;
	/* Print input data */
	printf("%5.3f %5.3f %5.3f %5.3f \n",OmegaM,OmegaL,OmegaB,h);
	printf("aexpn %f nx %d ny %d nz %d \n",aexpn,nx[0],nx[1],nx[2]);
	printf("dx %f dy %f dz %f \n",dx[0],dx[1],dx[2]);
	if (flag==Chandra || flag==PSPC) {
	  fread(&zobs,sizeof(float),1,inp);
	  fread(&photons,sizeof(float),1,inp);
	  fread(&exposure,sizeof(float),1,inp);
	  puts("X-ray observation Info.");
	  printf(" Redshift : z=%5.3f\n",zobs);
	  printf(" Photons detected : %d \n",(int)photons);
	  printf(" Exposure time    : %d ksec\n",(int)(exposure/1000));
	}
	/* Set size of the image (256/512) */
	if (nx[0]==nx[1] && nx[0]==nx[2]) nmax=nx[0];
	else { puts("Image is not symmetric!"); exit(0); }
	/* Define 2D matrix */
	map=matrix(0,nmax-1,0,nmax-1);
	/* Read in 2D image */
	for (i=0;i<nmax;i++) for (j=0;j<nmax;j++) 
	  fread(&map[i][j],sizeof(float),1,inp);
	fclose(inp);
	
	/* Calculating radial profiles */
	/* Total number of particles to throw down */
        np=500000;
        
        /* Define vectors and matrix */
        xx=vector(1,np);
        yy=vector(1,np);
        zz=vector(1,np);

	/* Define 1D vector for the radial bins */
	rbin=vector(1,nrmax);
	pbin=vector(1,nrmax);
	pbin_i=vector(1,nrmax);
	cntbin=vector(1,nrmax);
	cntbin_i=vector(1,nrmax);
	lx=dvector(1,nrmax);
	/* Initialize bins */
	for (i=1;i<=nrmax;i++) { 
	  rbin[i]=pbin[i]=cntbin[i]=0.0;
	  pbin_i[i]=cntbin_i[i]=lx[i]=0.0;
	} 

	/* Set up the radial shells */
	a1=min(xc,yc); a2=min(nmax-xc,nmax-yc);
	rmax=min(a1,a2);
	rmin=1.0;
	dlnr=(log(rmax)-log(rmin))/(nrmax-1);

	/* *********************************************** */
	/* ******* Compute radial profiles : p(r) ******** */
	/* *********************************************** */

	/* Throw particles uniformly in radius */
        seed=-1.0*(time(NULL)%100000);
        iseed=(long)seed;
        for (ip=1;ip<=np;ip++) {
          rp=(xc-1.0)*ran1(&iseed);
          pp=2.0*PI*ran1(&iseed);
          /* Find positions of particles in rectangular coordinates */
          xx[ip]=rp*cos(pp)+xc;
          yy[ip]=rp*sin(pp)+yc;
        }

	/* Compute radial profiles : p(r) */
        for (ip=1;ip<=np;ip++) {          
          gx=xx[ip]-xc; ix=(int)xx[ip];
          gy=yy[ip]-yc; jx=(int)yy[ip];
          r=sqrt(Squ(gx)+Squ(gy));  
	  n=(int)((log(r)-log(rmin))/dlnr)+1;
	  if (n<1) n=1;
	  /* Assign values in appropriate radial bins */
	  if (n<=nrmax) {
	    pbin[n]+=map[ix][jx];
	    cntbin[n]+=1.0; 
	  }
        }

	/* *********************************************** */
	/* ** Compute integrated radial profile : p(<r) ** */
	/* *********************************************** */

	for (ix=0;ix<nmax;ix++) for (jx=0;jx<nmax;jx++) {
	  gx=(float)ix-xc; 
          gy=(float)jx-yc; 
          r=sqrt(Squ(gx)+Squ(gy));  
	  n=(int)((log(r)-log(rmin))/dlnr)+1;
	  if (n<1) n=1;
	  /* Assign values in appropriate radial bins */
	  if (n<=nrmax) { 
	    pbin_i[n]+=map[ix][jx];
	    cntbin_i[n]+=1.0; 
	    /* if (n==1) printf("%d %d %d %f %f %e %e \n",n,ix,jx,r,cntbin_i[n],map[ix][jx],pbin_i[n]); */
	  }
	}

	/* *********************************************** */
	/* ** Construct radial profiles : p(r) & p(<r) *** */
	/* *********************************************** */

	for (i=1;i<nrmax;i++) { 
	  /* Radius in [h^-1 Mpc] */
	  rbin[i]=pow(exp(1),log(rmin)+(i-1)*dlnr)*dx[0];
	  /* Compute radial quantity : p(r) */
	  pbin[i]=pbin[i]/cntbin[i];
	  /* Compute integrated radial quantity : p(<r) */
	  if (i>1) {
	    pbin_i[i]+=pbin_i[i-1];
	    cntbin_i[i]+=cntbin_i[i-1];
	  }
	  if (!strncmp(&(infile[0]),"xsbp",4)) { 
	    /* Compute X-ray luminosity [h^-2 erg/s] */
	    zsim=0.01;
	    lx[i]=pbin_i[i]*Squ(dx[0]*MPC_2_CM);
	    lx[i]=lx[i]*(4.0*PI*Quad(1+zsim))/(4.0*Squ(PI/21600));
	  }
	}

	for (i=1;i<nrmax;i++) 
	  pbin_i[i]/=cntbin_i[i];

	/* *********************************************** */
	/* *************** Output profiles *************** */
	/* *********************************************** */

	/* Loop over the radial bin */
	fmax=-1.0e+30;
	/* Define outfile */
	strcpy(outfile,infile);
	strcat(outfile,"_pro.dat");
	printf("%s \n",&outfile);
        out=fopen(outfile,"w");
	fprintf(out,"%f %f %f %f \n",OmegaM,OmegaL,OmegaB,h);
	fprintf(out,"%f \n",aexpn);
	fprintf(out,"%d %d %d \n",nx[0],nx[1],nx[2]);
	fprintf(out,"%f %f %f \n",dx[0],dx[1],dx[2]);
	for (i=1;i<nrmax;i++) { 
	  /* Write out data */
	  if (!strncmp(&(infile[0]),"xsbp",4)) 
	    fprintf(out,"%f %e %le \n",rbin[i],pbin[i],lx[i]);
	  else 
	    fprintf(out,"%f %e %e \n",rbin[i],pbin[i],pbin_i[i]);
	}
	fclose(out);
	puts(" Calculated the 2D radial profile!");
	puts(" ");

	/* *********************************************** */
	/* ************* Controlling accuracy ************ */
	/* *********************************************** */
	
	/* Output file for monitoring accuracy */
	if (!strncmp(&(infile[0]),"xsbp",4)) {
	  strcpy(outfile,"pro_accuracy.");
	  if (!strncmp(&(infile[strlen(infile)-1]),"x",1))
	    strcat(outfile,"x");
	  else if (!strncmp(&(infile[strlen(infile)-1]),"y",1))
	    strcat(outfile,"y");
	  else if (!strncmp(&(infile[strlen(infile)-1]),"z",1))
	    strcat(outfile,"z");
	  strcat(outfile,".dat");
	  printf("Making :  %s \n",&outfile);
          out=fopen(outfile,"w");
	  for (i=1;i<nrmax;i++) 	  
	    fprintf(out,"%f %f %f \n",rbin[i],cntbin_i[i],PI*Squ(rbin[i]));
	  fclose(out);
	  puts(" Calculated the 2D radial profile!");
	  puts(" ");
	}

	free_matrix(map,0,nmax-1,0,nmax-1);
	free(nx);
        free(dx);
	free_vector(xx,1,np);
        free_vector(yy,1,np);
        free_vector(zz,1,np);
        free_vector(rbin,1,nrmax);
        free_vector(pbin,1,nrmax);
	free_vector(cntbin,1,nrmax);
	free_vector(pbin_i,1,nrmax);
	free_vector(cntbin_i,1,nrmax);
	free_dvector(lx,1,nrmax);
	return (0);
}
