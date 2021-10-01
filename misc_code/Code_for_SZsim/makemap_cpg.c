/* makemap_cpg
/* This program reads in 2D maps and use CPGPLOT to make its color map.
/* This program is set up to work on the data cubes.
/*
/* Note: need to run "/usr/local/pgplot/pgxwin_server"
/*
/* Last updated: Nov 12, 2001 */

#include "cpgplot.h"
#include "localdefs.h"

main(argc,argv)
        int argc;
        char *argv[];
{
        int     i,j,k,*nx,c1,c2,nc,nmax,l,nxl,nyl,nxr,nyr,lbase,flag,iscale,iwedge;
	int     xi,xf,yi,yf,pgplot_is_open();
        char    infile[200],command[200],devout[200],devin[200],prompt[200];
        float   aexpn,*dx,min(),max(),logx(),zobs,photons,exposure;
	float   *map,*lmap,*vector(),fmin,fmax,lfmin,lfmax;
	FILE    *inp,*out,*pip;
	void    free_matrix(),printerror(),free_vector(),drawbox();
	void    fiddle_menu(),sort_image(),show_image(),show_image_nowedge();
	void    gauss_smooth(),Plot2D();
	void    show_wedge(),palett();
	static float tr[6] = {0.0, 1.0, 0.0, 0.0, 0.0, 1.0}; 
	int  fiddle();
	float zi,zf,dA_zi_zf();
	
	/* iwedge = 0/1 (no/yes) */
	iwedge=1;

	/* Input parameters & datafile */
	Flag_zoom=0;
	strcpy(prompt,"Input <map binary file> <device type (xw, xs, ps, cps or null)> <xl> <yl> <xr> <yr> \n");
        if (argc<2) {
	  printf("%s",prompt);
	  exit(0);
	}
	else if (argc<3) {
	  sprintf(infile,"%s",argv[1]);
	  strcpy(devin,"?");
	}
	else if (argc<4){
	  sprintf(infile,"%s",argv[1]);
	  sprintf(devin,"%s",argv[2]);
	}
	else if (argc==7) {
	  sprintf(infile,"%s",argv[1]);
	  sprintf(devin,"%s",argv[2]);
	  sscanf(argv[3],"%d",&Nxl);
	  sscanf(argv[4],"%d",&Nxr);
	  sscanf(argv[5],"%d",&Nyl);
	  sscanf(argv[6],"%d",&Nyr);
	  Flag_zoom=1;
	}
	else {
	  printf("%s",prompt);
	  exit(0);
	}

	/* Set Flags */
	if (!strncmp(&(infile[0]),"tm",2)) flag=TM;
	else { flag=OTHERS; }

	/* Define vectors */
	nx=(int *)calloc(3,sizeof(int));
	dx=(float *)calloc(3,sizeof(float));

	/* Read in 2D maps */
	if (flag==TM) printf("Read in Tm :  %s \n",&infile);
	inp=fopen(infile,"rb");
	/* Read and Print header */
	/* fscanf(inp,"%f %f %f %f \n",&OmegaM,&OmegaL,&OmegaB,&h); */ 
	fread(&OmegaM,sizeof(float),1,inp); 
	fread(&OmegaL,sizeof(float),1,inp); 
	fread(&OmegaB,sizeof(float),1,inp); 
	fread(&h,sizeof(float),1,inp);  
	fread(&aexpn,sizeof(aexpn),1,inp);
	fread(nx,sizeof(int),3,inp);
	fread(dx,sizeof(float),3,inp);
	if (aexpn>1.0) aexpn=1.0;
	/* Print input data */
	printf("%5.3f %5.3f %5.3f %5.3f \n",OmegaM,OmegaL,OmegaB,h);
	printf("aexpn %f nx %d ny %d nz %d \n",aexpn,nx[0],nx[1],nx[2]);
	printf("dx %f dy %f dz %f \n",dx[0],dx[1],dx[2]);
		
	/* Set size of the image (256/512) */
	if (nx[0]==nx[1] && nx[0]==nx[2]) nmax=nx[0];
	else { puts("Image is not symmetric!"); 
	       puts("May want to try using : makemap_cpg_old ");
	       exit(0); }
	/* Define 1D vector */
	map=vector(0,Squ(nmax)-1);
	lmap=vector(0,Squ(nmax)-1);

	/* Read in 2D maps */
	for (i=0;i<nmax;i++) for (j=0;j<nmax;j++) {
	  /* Read in y-axis first */
	  k=i*nx[1]+j;
	  fread(&map[k],sizeof(float),1,inp);
	  map[k] = map[k];
	  /* printf("%f \n",map[k]); */
	}
	fclose(inp);
	puts("Finished reading data in binary mode!");
	puts(" ");


	/* CPGPLOT routines */
	/* Check requested device type : xwindow or output to postscript file? */
	if (!strncmp(&(devin[strlen(devin)-2]),"xw",2)) {
	  strcpy(devout,"/");
	  strcat(devout,devin); 
        }
	else if (!strncmp(&(devin[strlen(devin)-2]),"xs",2)) {
          strcpy(devout,"/");
	  strcat(devout,devin); 
	}
	else if (!strncmp(&(devin[strlen(devin)-3]),"cps",3)) { 
	  strcpy(devout,infile);
	  strcat(devout,".ps/VCPS");
	}
	else if (!strncmp(&(devin[strlen(devin)-2]),"ps",2)) {
	  strcpy(devout,infile);
	  strcat(devout,".ps/VPS");
	}
	else if (!strncmp(&(devin[strlen(devin)-1]),"?",1)) 
	  strcpy(devout,"?");

	/* Call PGBEG to initiate PGPLOT and open the output device; PGBEG */
	/* will prompt the user to supply the device name and type.        */  
	if(cpgbeg(0, devout, 1, 1) != 1) { printf("cpgbeg returned 0\n"); exit(0); }
	
	/* Set color index : color map type=2 (rainbow) */
	cpgqcir(&c1,&c2);
	if (c2>c1) nc = c2-c1+1;
	else nc=0;
	printf("Number of color indices used for image: %d \n",nc);
	if (nc<8) puts("Not enough colors available on this device");
	P=2;
	SIGN=+1.0;
	
	if (flag==TM) { BRIGHT=0.590729; CONTRA=2.827258;  }
	else { BRIGHT=0.50; CONTRA=1.0; }
        palett(P, CONTRA, BRIGHT);	

	/* Show menu for the fiddle colors */
	if ( !strncmp(&(devin[strlen(devin)-2]),"xw",2) || 
	     !strncmp(&(devin[strlen(devin)-2]),"xs",2) || 
	     !strncmp(&(devin[strlen(devin)-1]),"?",1) ) fiddle_menu();
	else printf("Output file : %s \n",devout);

	/* Sort images for CPGPLOT */
	iscale = LOG10;
	sort_image(map,lmap,&lfmin,&lfmax,nmax,iscale);
	printf("  Check fmin=%e fmax=%e \n",lfmin,lfmax);
        
	/* Hard-wire in lfmax & lfmin */
	if (flag==TM) { fmin=1.0e+06; fmax=1.1e+08; } 
	
	/* Set scale (LINEAR/LOG2/LOG10) */
	if (iscale==LOG10) {
	  lbase=10;  
	  lfmin=logx(fmin,lbase); 
	  lfmax=logx(fmax,lbase); 
	}
	if (iscale==LINEAR) {
	  lfmin=fmin;
	  lfmax=fmax;
	}
	printf("  Set log-color between fmin=%e fmax=%e \n",fmin,fmax);

	/* Show image */
	show_image_nowedge(lmap,lfmin,lfmax,nmax,devout,flag);
 
	/* Draw a wedge only */
	if (iwedge==1) show_wedge(lfmin,lfmax,flag); 
       
	/* Finally, call PGEND to terminate things properly. */
	cpgebuf();
	cpgend();

	free_vector(lmap,0,Squ(nmax)-1);
	free_vector(map,0,Squ(nmax)-1);
	return (0);
}

