/* SZmap_binary2fits
/* This program reads in the Binary data file of the simulated
/* cluster and the output FITS file (for simint).
/* This program is set up to work with 2D maps.
/*
/* Last updated: July 15, 2003 */

#include "localdefs.h"
#include "/usr/local/include/fitsio.h"

main(argc,argv)
        int argc;
        char *argv[];
{
        int     i,j,k,*nx,nsize,nmax;
        char    infile1[200],infile2[200],outfile[200];
        float   aexpn,zobs,xfreq,*dx,*map1,*map2,f_tsz(),*vector();
	FILE    *inp,*out;
	void    writeimage(),free_vector();

	/* FITS image parameters */
	fitsfile *fptr;       /* pointer to the FITS file, defined in fitsio.h */
	int status, ii, jj;
	long  fpixel, nelements, exposure;
	int bitpix; 
	long naxis, naxes[2];   /* image is 300 pixels wide by 200 rows */

	/* Input datafile */
        if (argc<6) {printf("Input <compy map> <ksz map> <out filename> <zobs> <frequency [GHz]>\n");exit(0);}
	sprintf(infile1,"%s",argv[1]);
	sprintf(infile2,"%s",argv[2]);
	sprintf(outfile,"%s",argv[3]);
	sscanf(argv[4],"%f",&ZOBS);  /* Observing redshift */
	sscanf(argv[5],"%f",&FREQ);  /* Frequency [GHz] */

	/* Define vectors */
	nx=(int *)calloc(3,sizeof(int));
	dx=(float *)calloc(3,sizeof(float));
	
	/* Read in C-Binary datafile 1 */
        inp=fopen(infile1,"rb");
        /* Read and Print header */
        fscanf(inp,"%f %f %f %f \n",&OmegaM,&OmegaL,&OmegaB,&h);
        fread(&aexpn,sizeof(aexpn),1,inp);
        fread(nx,sizeof(int),3,inp);
        fread(dx,sizeof(float),3,inp);
        if (aexpn>1.0) aexpn=1.0;
        /* Print input data */
        printf("Om %5.3f Ol %5.3f Ob %5.3f h %5.3f \n",OmegaM,OmegaL,OmegaB,h);
        printf("aexpn %f nx %d ny %d nz %d \n",aexpn,nx[0],nx[1],nx[2]);
        printf("dx %f dy %f dz %f \n",dx[0],dx[1],dx[2]);
        
        /* Set size of the image (256/512) */
        if (nx[0]==nx[1] && nx[0]==nx[2]) nmax=nx[0];
        else { puts("Image is not symmetric!"); exit(0); }
        /* Define 1D vector */
        map1=vector(0,Squ(nmax)-1);
       
        /* Read in compton-y maps */
        for (i=0;i<nmax;i++) for (j=0;j<nmax;j++) {
          /* Read in y-axis first */
          k=i*nx[1]+j;
          fread(&map1[k],sizeof(float),1,inp);
        }
        fclose(inp);
        puts("Finished reading compton-y map in binary mode!");
        puts(" ");


	/* Read in C-Binary datafile 2 */
	printf("%s \n",infile2);
        inp=fopen(infile2,"rb");
        /* Read and Print header */
        fscanf(inp,"%f %f %f %f \n",&OmegaM,&OmegaL,&OmegaB,&h);
        fread(&aexpn,sizeof(aexpn),1,inp);
        fread(nx,sizeof(int),3,inp);
        fread(dx,sizeof(float),3,inp);
        if (aexpn>1.0) aexpn=1.0;
        /* Print input data */
        printf("Om %5.3f Ol %5.3f Ob %5.3f h %5.3f \n",OmegaM,OmegaL,OmegaB,h);
        printf("aexpn %f nx %d ny %d nz %d \n",aexpn,nx[0],nx[1],nx[2]);
        printf("dx %f dy %f dz %f \n",dx[0],dx[1],dx[2]);
        
        /* Set size of the image (256/512) */
        if (nx[0]==nx[1] && nx[0]==nx[2]) nmax=nx[0];
        else { puts("Image is not symmetric!"); exit(0); }
        /* Define 1D vector */
        map2=vector(0,Squ(nmax)-1);
       
        /* Read in ksz maps */
        for (i=0;i<nmax;i++) for (j=0;j<nmax;j++) {
          /* Read in y-axis first */
          k=i*nx[1]+j;
          fread(&map2[k],sizeof(float),1,inp);
        }
        fclose(inp);
        puts("Finished reading kSZ map in binary mode!");
        puts(" ");

	
	/* convert compton-y and kSZ-b parameters to temperature in Kelvin */
	/* at a given frequency [GHz] */
	xfreq = FREQ * 1.0e9 * hP / ( kB * T_CMB );
        for (i=0;i<nmax;i++) for (j=0;j<nmax;j++) {
          /* Read in y-axis first */
          k=i*nx[1]+j;
	  map1[k] = f_tsz(xfreq) * map1[k] * T_CMB;  /* thermal SZE */
	  map2[k] = -1.0 * map2[k] * T_CMB;         /* kinetic SZE */
	  map1[k] = map1[k] + map2[k];  /* thermal + kinetic SZE */
        }
	puts("Computing thermal + kinetic SZE in units of Kelvin ");
	printf("  f_tsz(FREQ=%4.1f GHz) = %7.5f \n",FREQ,f_tsz(xfreq)); 
	puts(" ");

	/* Output FITS datafile */
	AEXPN = aexpn;
	ZSIM = 1.0 / aexpn - 1.0; 
	PIXSIZE = dx[0];         /* Pixel size [/h Mpc] */
	IMGSIZE = nmax * dx[0];  /* Image size [/h Mpc] */
	writeimage( outfile, map1, nmax );

	free_vector(map1,0,Squ(nmax)-1);
	free_vector(map2,0,Squ(nmax)-1);
	return(0);
}

/******************************************************/
/* Create a FITS primary array containing a 2-D image */
/******************************************************/

void writeimage( char *outfile, float *map, int nsize )
{
    
    /* FITS image parameters */
    fitsfile *fptr;       /* pointer to the FITS file, defined in fitsio.h */
    int status, ii, jj, kk;
    int bitpix, flag; 
    long  fpixel, nelements;
    long naxis, naxes[2]; 
    float zsim, degpix,fov,xfreq,f_tsz();
    void printerror();

    /* Header values for simint input */
    float dA(),dAobs;
    float crval1, crval2, crpix1, crpix2, cdelt1, cdelt2, crota1, equinox;


    /* initialize FITS image parameters */
    bitpix = FLOAT_IMG;  /* 32-bit floating pooiint pixel values */
    naxis = 2;           /* 2D image */
    naxes[0]=naxes[1]=nsize; /* image is nsize x nsize square images */
    
    remove(outfile);    /* Delete old file if it already exists */
    status = 0;         /* initialize status before calling fitsio routines */
    
    printf("Output FITS datafile :  %s \n",outfile);
	
    if (fits_create_file(&fptr, outfile, &status)) /* create new FITS file */
	printerror( status );   /* call printerror if error occurs */

    if ( fits_create_img(fptr, bitpix, naxis, naxes, &status) )
	printerror( status ); 

    fpixel = 1;                               /* first pixel to write      */
    nelements = naxes[0] * naxes[1];          /* number of pixels to write */

    /* write the array of unsigned integers to the FITS file */
    if ( fits_write_img(fptr, TFLOAT, fpixel, nelements, map, &status) )
	printerror( status );
    
    /* Header for the simulation parameters */        
    if ( fits_write_date(fptr, & status) ) 
	printerror( status );   

    if ( fits_write_comment(fptr, "   ", & status) ) 
	printerror( status );   

    if ( fits_write_comment(fptr, "  ART simulation parameters : ", & status) ) 
	printerror( status );   

    if ( fits_update_key(fptr, TFLOAT, "OmegaM", &OmegaM,
			 "Omega Matter", &status) )
	printerror( status );           

    if ( fits_update_key(fptr, TFLOAT, "OmegaL", &OmegaL,
			 "Omega Lambda", &status) )
	printerror( status );           

    if ( fits_update_key(fptr, TFLOAT, "OmegaB", &OmegaB,
			 "Omega Baryon", &status) )
	printerror( status );           

    if ( fits_update_key(fptr, TFLOAT, "hubble", &h,
			 "hubble parameter in units of 100h [km/s/Mpc]", &status) )
	printerror( status );           

    if ( fits_update_key(fptr, TFLOAT, "aexpn", &AEXPN,
			 "the scale factor of the simulation output", &status) )
	printerror( status );           

    if ( fits_update_key(fptr, TFLOAT, "zsim", &ZSIM,
			 "the redshift of the simulation output", &status) )
	printerror( status );           

    if ( fits_update_key(fptr, TFLOAT, "imgsize", &IMGSIZE,
			 "image size [1/h Mpc] of the simulation output", &status) )
	printerror( status );  

    if ( fits_update_key(fptr, TFLOAT, "pixsize", &PIXSIZE,
			 "pixel size [1/h Mpc] of the simulation output", &status) )
	printerror( status );           


    /* Header for simint (mock SZ observation) */
    crval1 = 0.000;   /* reference pixel value */
    crval2 = 90.000;  /* reference pixel value */
    crpix1 = 128.50;  /* reference pixel */
    crpix2 = 128.50;  /* reference pixel */
    dAobs = dA(ZOBS); /* angular diameter distance [Mpc] for given zobs */
    degpix = atan(PIXSIZE/h/dAobs)*(180.0/PI); /* degrees / pixel */
    fov    = degpix * nsize; /* F.O.V. in degrees */
    cdelt1 = degpix;  /* degrees / pixel */
    cdelt2 = degpix;         /* degrees / pixel */
    crota1 = 0.00000;   /* rotation in degrees */
    equinox = 2000.00;  /* Equinox of coordinates */
   
    printf("Thermal + kinetic SZE data in Kelvin at %5.2f GHz \n",FREQ);
    printf("  dA(zobs=%5.3f) = %7.3f [Mpc] \n",ZOBS,dAobs);
    printf("  FOV = %f [arcmin] \n",fov*60.0);
    printf("  Pixel size = %f [arcsec] \n",degpix*3600.0);
    
    
    if ( fits_write_comment(fptr, "   ", & status) ) 
	printerror( status );   

    if ( fits_write_comment(fptr, "  Parameters for the mock interferometric observation: ", & status) ) 
	printerror( status );   

    if ( fits_update_key(fptr, TSTRING, "telescop", "Simulated thermal + kinetic SZ map",
			 " ", &status) )
	printerror( status ); 

    if ( fits_update_key(fptr, TSTRING, "bunit", "K",
			 "temperature [Kelvin]", &status) )
      printerror( status ); 
    
    if ( fits_update_key(fptr, TFLOAT, "freq", &FREQ,
			 "observing frequency [GHz]", &status) )
      printerror( status ); 

    if ( fits_update_key(fptr, TFLOAT, "zobs", &ZOBS, 
			 "Redshift of the cluster observed", &status) )
	printerror( status );           

    if ( fits_update_key(fptr, TFLOAT, "dAobs", &dAobs, 
			 "Angular diameter distance [Mpc]", &status) )
	printerror( status );           

    if ( fits_update_key(fptr, TSTRING, "ctype1", "RA--TAN",
			 "X-axis type", &status) )
	printerror( status );           

    if ( fits_update_key(fptr, TSTRING, "ctype2", "DEC--TAN",
			 "Y-axis type", &status) )
	printerror( status );
     
    if ( fits_update_key(fptr, TFLOAT, "crval1", &crval1,
			 "Reference pixel value", &status) )
	printerror( status );           
    
    if ( fits_update_key(fptr, TFLOAT, "crval2", &crval2,
			 "Reference pixel value", &status) )
	printerror( status );           
    
    if ( fits_update_key(fptr, TFLOAT, "crpix1", &crpix1,
			 "Reference pixel", &status) )
	printerror( status );           
    
    if ( fits_update_key(fptr, TFLOAT, "crpix2", &crpix2,
			 "Reference pixel", &status) )
	printerror( status );           
    
    if ( fits_update_key(fptr, TFLOAT, "fov", &fov, 
			 "Field of view [degrees]", &status) )
	printerror( status );           

    if ( fits_update_key(fptr, TFLOAT, "cdelt1", &cdelt1, 
			 "Degrees / pixel", &status) )
	printerror( status );           
    
    if ( fits_update_key(fptr, TFLOAT, "cdelt2", &cdelt2, 
			 "Degrees / pixel", &status) )
	printerror( status );           
    
    if ( fits_update_key(fptr, TFLOAT, "crota1", &crota1, 
			 "Rotation in degrees", &status) )
	printerror( status );           
    
    if ( fits_update_key(fptr, TFLOAT, "equinox", &equinox, 
			 "Equinox of coordinates", &status) )
	printerror( status );           

    if ( fits_update_key(fptr, TFLOAT, "fov", &fov, 
			 "Field of view [degrees]", &status) )
	printerror( status );           

    if ( fits_update_key(fptr, TSTRING, "telescop", "SZA",
			 "", &status) )
      printerror( status ); 

    if ( fits_update_key(fptr, TSTRING, "instrume", "SZA",
			 "", &status) )
      printerror( status ); 

        
    if ( fits_close_file(fptr, &status) )                /* close the file */
	printerror( status );   

    return;
}

/*--------------------------------------------------------------------------*/
void printerror( int status)
{
    /*****************************************************/
    /* Print out cfitsio error messages and exit program */
    /*****************************************************/


    if (status)
    {
       fits_report_error(stderr, status); /* print error report */

       exit( status );    /* terminate the program, returning error status */
    }
    return;
}

