/* cpgmacros.c */
/* Useful CPGPLOT programs */

#include "cpgplot.h"
#include "localdefs.h"

/* Set viewport */
void setvp()
{  /*-----------------------------------------------------------------------
   Set the viewport, allowing margins around the edge for annotation.
   (This is similar in effect to PGVSTD, but has different margins.)
   The routine determines the view-surface size and allocates margins
   as fractions of the minimum of width and height.
   ----------------------------------------------------------------------- */
  
  float d,vpx1,vpx2,vpy1,vpy2,min();
  
  /* Set viewport (normalized device coordinates) */
  cpgsvp(0.0, 1.0, 0.0, 1.0);
  /* Inquire viewport size and position */
  cpgqvp(1, &vpx1, &vpx2, &vpy1, &vpy2);
  d=min(vpx2-vpx1,vpy2-vpy1)/40.0;
  vpx1 = vpx1 + 5.0*d;
  vpx2 = vpx2 - 2.0*d;
  vpy1 = vpy1 + 8.0*d;
  vpy2 = vpy2 - 2.0*d;
  /* Set viewport (inches) */
  cpgvsiz(vpx1, vpx2, vpy1, vpy2);
}


/* Set viewport */
void setvp_plot()
{  /*-----------------------------------------------------------------------
   Set the viewport, allowing margins around the edge for annotation.
   (This is similar in effect to PGVSTD, but has different margins.)
   The routine determines the view-surface size and allocates margins
   as fractions of the minimum of width and height.
   ----------------------------------------------------------------------- */
  
  float d,vpx1,vpx2,vpy1,vpy2,min();
  
  /* Set viewport (normalized device coordinates) */
  cpgsvp(0.0, 1.0, 0.0, 1.0);
  /* Inquire viewport size and position */
  cpgqvp(1, &vpx1, &vpx2, &vpy1, &vpy2);
  d=min(vpx2-vpx1,vpy2-vpy1)/40.0;
  vpx1 = vpx1 + 10.0*d;
  vpx2 = vpx2 - 6.0*d;
  vpy1 = vpy1 + 15.0*d;
  vpy2 = vpy2 - 15.0*d;
  /* Set viewport (inches) */
  cpgvsiz(vpx1, vpx2, vpy1, vpy2);
}

/* Color palett */
void palett(type, contra, bright)
       int type;
       float contra, bright;
{ /* ----------------------------------------------------------------------
     Set a "palette" of colors in the range of color indices used by PGIMAG.
     ---------------------------------------------------------------------- */

      float GL[2] = { 0.0, 1.0 };
      float GR[2] = { 0.0, 1.0 };
      float GG[2] = { 0.0, 1.0 };
      float GB[2] = { 0.0, 1.0 };

      float RL[9] = { -0.5, 0.0, 0.17, 0.33, 0.50, 0.67, 0.83, 1.0, 1.7 };
      float RR[9] = { 0.0, 0.0,  0.0,  0.0,  0.6,  1.0,  1.0, 1.0, 1.0 };
      float RG[9] = { 0.0, 0.0,  0.0,  1.0,  1.0,  1.0,  0.6, 0.0, 1.0 };
      float RB[9] = { 0.0, 0.3,  0.8,  1.0,  0.3,  0.0,  0.0, 0.0, 1.0 };

      float HL[5] = { 0.0, 0.2, 0.4, 0.6, 1.0 };
      float HR[5] = { 0.0, 0.5, 1.0, 1.0, 1.0 };
      float HG[5] = { 0.0, 0.0, 0.5, 1.0, 1.0 };
      float HB[5] = { 0.0, 0.0, 0.0, 0.3, 1.0 };

      float WL[10] = { 0.0, 0.5, 0.5, 0.7, 0.7, 0.85, 0.85, 0.95, 0.95, 1.0 };
      float WR[10] = { 0.0, 1.0, 0.0, 0.0, 0.3,  0.8,  0.3,  1.0,  1.0, 1.0 };
      float WG[10] = { 0.0, 0.5, 0.4, 1.0, 0.0,  0.0,  0.2,  0.7,  1.0, 1.0 };
      float WB[10] = { 0.0, 0.0, 0.0, 0.0, 0.4,  1.0,  0.0,  0.0, 0.95, 1.0 };

      float AL[20] = { 0.0, 0.1, 0.1, 0.2, 0.2, 0.3, 0.3, 0.4, 0.4, 0.5,
                   0.5, 0.6, 0.6, 0.7, 0.7, 0.8, 0.8, 0.9, 0.9, 1.0 };
      float AR[20] = { 0.0, 0.0, 0.3, 0.3, 0.5, 0.5, 0.0, 0.0, 0.0, 0.0,
		   0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 };
      float AG[20] = { 0.0, 0.0, 0.3, 0.3, 0.0, 0.0, 0.0, 0.0, 0.8, 0.8,
		   0.6, 0.6, 1.0, 1.0, 1.0, 1.0, 0.8, 0.8, 0.0, 0.0 };
      float AB[20] = { 0.0, 0.0, 0.3, 0.3, 0.7, 0.7, 0.7, 0.7, 0.9, 0.9,
		   0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };

      if (type==1) 
	/* -- gray scale */
	cpgctab(GL, GR, GG, GB, 2, contra, bright);
      else if (type==2) 
        /* -- rainbow */
	cpgctab(RL, RR, RG, RB, 9, contra, bright);
      else if (type==3) 
        /* -- heat */
	cpgctab(HL, HR, HG, HB, 5, contra, bright);
      else if (type==4) 
        /*-- weird IRAF */
	cpgctab(WL, WR, WG, WB, 10, contra, bright);
      else if (type==5) 
        /* -- AIPS */
	cpgctab(AL, AR, AG, AB, 20, contra, bright);
}

/* Allow fiddle to work */
int pgplot_is_open(void)
{
  char answer[10];                 /* The PGQINF return string */
  int answer_len = sizeof(answer); /* allocated size of answer[] */
  cpgqinf("STATE", answer, &answer_len);
  return strcmp(answer, "YES") == 0;
}

/* Fiddle color and zoom */
int fiddle()
{
      int ier, cpgcurs();
      float x, y, min(), max(), val;
      float x1, y1, x2, y2, b1, b2, c1, c2;
      char ch;
      void fiddle_menu(),drawbox();

      x = 0.5;
      y = 1.0; 

      cpgqwin(&x1, &x2, &y1, &y2);
      b1 = 0.0;
      b2 = 1.0;
      c1 = 0.0;
      c2 = 10.0;
      cpgswin(b1, b2, c1, c2);

      start : ;
      
      ier = cpgcurs(&x, &y, &ch);
      /* printf("ch=%c \n",ch); */
      /* if (ch==char[0] ||  ch=='x' || ch=='X') { */
      if (ch=='x' || ch=='X') {
	cpgswin(x1, x2, y1, y2);
        printf("p=%d sign=%f contra=%f bright=%f \n",P,SIGN,CONTRA,BRIGHT); 
	return(0);
      }
      else if (ch=='V' || ch=='v') { 
	x=x*(x2-x1)/(b2-b1);
	y=y*(y2-y1)/(c2-c1);
	printf("x %8.3f y %8.3f \n",x,y); 
	goto start;
      }
      else if (ch=='F' || ch=='f') {
	BRIGHT = max(b1, min(b2,x));
	CONTRA = max(c1, min(c2,y));
      }
      else if (ch=='C' || ch=='c') {
	CONTRA = 1.0;
	BRIGHT = 0.5;
	x = 0.5;
        y = 1.0;
      }
      else if (ch=='z' || ch=='Z') {
	printf("Zoom in/out <xl> <xr> <yl> <yr> : ");
	if (scanf("%d %d %d %d",&Nxl,&Nxr,&Nyl,&Nyr)==4)
	  Flag_zoom=ZOOM;
      }
      else if (ch=='b' || ch=='B') {
	printf("Draw box <x1> <y1> <l1> <l2> <angle[deg]> : ");
	if (scanf("%f %f %f %f %f",&Xi,&Yi,&L1,&L2,&Angle)==5)
	  Flag_box=BOX;
      }
      else if (ch=='a' || ch=='A') {
	printf("Pos = ");
	if (scanf("%d",&Pos)==1) puts(" ");
      }
      else if (ch=='l' || ch=='L') {
	printf("Losbin = ");
	if (scanf("%d",&Losbin)==1) puts(" ");
      }
      else if (ch=='r' || ch=='R') {
	Flag_zoom=NOZOOM;
	Flag_box=0;
	P=2;
	CONTRA = 1.0;
	BRIGHT = 0.5;
	SIGN=+1.0;
	x = 0.5;
        y = 1.0;
      }
      else if (ch=='-') SIGN = -SIGN;
      else if (ch=='1') P = 1;
      else if (ch=='2') P = 2;
      else if (ch=='3') P = 3;
      else if (ch=='4') P = 4;
      else if (ch=='5') P = 5;
      else if (ch=='p' || ch=='P') P = 1 + P % 5;
      else if (ch=='?') { fiddle_menu(); goto start; }
      palett(P, SIGN*CONTRA, BRIGHT);
      if (ch=='z' || ch=='Z' || ch=='b' || ch=='B') return(1);
      goto start;  
      /* goto end; */
 end: ;

      return(1);
}

/* Fiddle menu */
void fiddle_menu()
{
  puts(" ");
  puts("Use cursor to adjust color table:");
  puts(" Keys 1,2,3,4,5 select different palettes : ");
  puts("   1=grey, 2=rainbow, 3=heat, 4=IRAF, 5=AIPS");
  puts(" Key V read the cursor positions");
  puts(" Key P cycles through available palettes");
  puts(" Key - reverses color palette");
  puts(" Key C resets contrast=1.0, brightness=0.5");
  puts(" Key F adjusts contrast and brightness, with "); 
  puts("   'cursor x position' setting brightness [0.0 - 1.0] and");
  puts("   'cursor y position' setting contrast [0.0 - 10.0]");
  puts(" Key B draw box");
  puts(" Key Z zoom in/out");
  puts(" Key L Modify Losbin ");
  puts(" Key A Modify Pos ");
  puts(" Key R reset zoom & color palett");
  puts(" Key ? show menu");
  puts(" Key X or right mouse button exits program");
}

/* ****** Sort image in PGPLOT format & find man, max values *******/

void sort_image(image,limage,lfmin,lfmax,nmax,iscale)
     float *image,*limage,*lfmin,*lfmax;
     int iscale,nmax;
{
  int i,j,kc,kf;
  float min(),max(),logx(),fmin,fmax;
  
  /* Initialize */
  fmax=-1.0e+30;
  fmin=1.0e+30;
  for (i=0;i<nmax;i++) {
    for (j=0;j<nmax;j++) {
      kc=i*nmax+j;  /* C convenction */
      kf=j*nmax+i;  /* Fortran convention */
      if (iscale==LINEAR) limage[kf]=image[kc];
      else if (iscale==LOG10) limage[kf]=logx(image[kc],10);
      else if (iscale==LOG2)  limage[kf]=logx(image[kc],2);
      fmax=max(image[kc],fmax);
      fmin=min(image[kc],fmin);
    }
  }

  /* Set fmin & fmax */
  if (fmin<1.0e-30) fmin=1.0e-20;
  if (iscale==LINEAR) { /* Linear sale if iscale==0 */
    *lfmin=fmin;
    *lfmax=fmax;
  }
  else if (iscale==LOG10) { /* Log10 sale if iscale==1 */
    *lfmin=logx(fmin,10);
    *lfmax=logx(fmax,10);
  }
  else if (iscale==LOG2)  { /* Log2 sale if iscale==2 */
    *lfmin=logx(fmin,2);
    *lfmax=logx(fmax,2);
  }

}

/* ********************* Show Image ***********************/

void show_image(image,fmin,fmax,nmax,device,flag) 
     float *image,fmin,fmax;
     int nmax,flag;
     char device[200];
{  
  int c1,c2,nc;
  static float tr[6] = {0.0, 1.0, 0.0, 0.0, 0.0, 1.0}; 
  void setvp(),show_wedge(),drawbox();
  
  printf("  Plot range : fmin %e fmax %e \n",fmin,fmax);

  start : ;

  /* Clear the screen. Set up window and viewport (with same aspect ratio). */
  cpgpage();
  setvp(); 
  cpgwnad(0.0, nmax, 0.0, nmax); 
  
	
  /* Draw a map with CPGIMAG. */
  if (Flag_zoom==ZOOM) {
    cpgwnad(Nxl-1.0,Nxr+1.0,Nyl-1.0,Nyr+1.0);
    cpgimag(image,nmax,nmax,Nxl,Nxr,Nyl,Nyr,fmin,fmax,tr);
  }
  else {
    cpgwnad(0.0,nmax+1.0,0.0,nmax+1.0);
    cpgimag(image,nmax,nmax,1,nmax,1,nmax,fmin,fmax,tr);
  }
  
  /* Draw a rectangular box */
  if (Flag_box==BOX) drawbox(Xi,Yi,L1,L2,Angle);

  /* Annotate the plot, set character height, draw labeled frame, text */
  cpgsch(0.6);
  cpgbox("bcntsi",0.0,0,"bcntsiv",0.0,0);
  cpgmtxt("b",3.0,1.0,1.0,"pixel number"); 
  /* Draw a wedge & caption */
  show_wedge(fmin,fmax,flag);

  /* If the device has a cursor, allow user to fiddle with color table. */
  if ( ( !strncmp(&(device[strlen(device)-2]),"xw",2) || 
	 !strncmp(&(device[strlen(device)-2]),"xs",2) || 
	 !strncmp(&(device[strlen(device)-1]),"?",1) ) &&
       (pgplot_is_open() == 0) ) {
    if (fiddle()==1) {
      cpgask(0);
      goto start;
    }
  } 

  /* Reset the zoom */
  Flag_zoom=NOZOOM;
  Flag_zoom=0;

 end: ;

}

/* Display wedge with correct captioning */
void show_wedge(fmin,fmax,flag)
     float fmin,fmax;
     int flag;
{
  if ( flag==TM ){
    /* cpgwedg("BI", 4.0, 5.0, fmin, fmax,"log\\d10 \\u(T\\dew \\u Kelvin)");
    cpgsch(1.0);
    cpgmtxt("t", 1.0, 0.0, 0.0, "Emission Weighted Temperature Map"); */
    cpgsch(1.7);
    cpgwedg("LI", 0.5, 3.0, fmin, fmax," ");
    /* cpgptxt(42.0, 130.0, 90.0, 0.0,"log\\d10 \\u[ T\\dew \\u Kelvin ] ");  */
  }    
}


/* Show image with no wedge and title */
void show_image_nowedge(image,fmin,fmax,nmax,device,flag) 
     float *image,fmin,fmax;
     int nmax,flag;
     char device[200];
{  
  int c1,c2,nc;
  static float tr[6] = {0.0, 1.0, 0.0, 0.0, 0.0, 1.0}; 
  void setvp(),show_wedge(),drawbox();
  
  printf("  Plot range : fmin %e fmax %e \n",fmin,fmax);

  start : ;

  /* Clear the screen. Set up window and viewport (with same aspect ratio). */
  cpgpage();
  setvp(); 
  cpgwnad(0.0, nmax, 0.0, nmax); 
	
  /* Draw a map with CPGIMAG. */
  if (Flag_zoom==ZOOM) {
    cpgwnad(Nxl-1.0,Nxr+1.0,Nyl-1.0,Nyr+1.0);
    cpgimag(image,nmax,nmax,Nxl,Nxr,Nyl,Nyr,fmin,fmax,tr);
  }
  else {
    cpgwnad(0.0,nmax+1.0,0.0,nmax+1.0);
    cpgimag(image,nmax,nmax,1,nmax,1,nmax,fmin,fmax,tr);
  }

  /* Draw a rectangular box */
  if (Flag_box==BOX) { cpgslw(7); drawbox(Xi,Yi,L1,L2,Angle); cpgslw(2); }
  
  /* Annotate the plot, set character height, draw labeled frame, text */
  cpgsch(0.6);
  cpgbox("bc",0.0,0,"bc",0.0,0);
  /* cpgmtxt("b",3.0,1.0,1.0,"pixel number"); */
  /* Draw a wedge & caption */
  /* show_wedge(fmin,fmax,flag); */

  /* If the device has a cursor, allow user to fiddle with color table. */
  if ( ( !strncmp(&(device[strlen(device)-2]),"xw",2) || 
	 !strncmp(&(device[strlen(device)-2]),"xs",2) || 
	 !strncmp(&(device[strlen(device)-1]),"?",1) ) &&
       (pgplot_is_open() == 0) ) {
    if (fiddle()==1) {
      cpgask(0);
      goto start;
    }
  } 

  /* Reset the zoom */
  /* Flag_zoom=NOZOOM; */

 end: ;

}

/* Routine to draw a rectangle */
void drawbox(x1,y1,l1,l2,angle)
        float x1,y1,l1,l2,angle;
{
        float x2,x3,x4,y2,y3,y4;
	void drawline();

	/* Expect angle in degree & convert to radian */
	angle*=(PI/180.0);

	/* Define rest of 3 points */
	x2=x1+l1*cos(angle);
	y2=y1+l1*sin(angle);

	x3=x1-l2*sin(angle);
	y3=y1+l2*cos(angle);

	x4=x1+l1*cos(angle)-l2*sin(angle);
	y4=y1+l1*sin(angle)+l2*cos(angle);

	/* Draw a rectangle */
	drawline(x1,y1,x2,y2);
	drawline(x1,y1,x3,y3);
	drawline(x2,y2,x4,y4);
	drawline(x3,y3,x4,y4);
}


/* Routine to draw a straight line */
void drawline(xi,yi,xf,yf)
        float xi,yi,xf,yf;
{
        int i,nbin;
        float *x,*y,l,dl,cosine,sine,*vector();
	void free_vector();

	/* Define lines */
	l=sqrt(Squ(xf-xi)+Squ(yf-yi));
        nbin=(int)l;
        dl=l/nbin;
        cosine=(xf-xi)/l;
        sine=(yf-yi)/l;

	/* Allocate memory for vectors */
	x=vector(1,nbin);
	y=vector(1,nbin);
	
	for (i=0;i<=nbin;i++) {
	  x[i]=xi+i*dl*cosine;
	  y[i]=yi+i*dl*sine;
	}
	/* Draw lines using CPGPLOT */
        cpgline(nbin+1,x,y);

	free_vector(x,1,nbin);
	free_vector(y,1,nbin);
}

