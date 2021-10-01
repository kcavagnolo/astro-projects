/* ctools.c */

#include "cpgplot.h"
#include "localdefs.h"

/* I/O routine : Read in C-binary */
/* Read in data : density [Msun/Mpc^3] & temperature [Kelvin] */
void Read_Cube ( datafile, sv, aexpn, nnx, ddx )
    int *nnx;
    float ***sv,*ddx,*aexpn;
    char  *datafile;
{
    int i,j,k,nsize,iformat,junk,*space;
    float sum;
    float ***f3tensor();
    FILE *inp;


    /* Check the file name */
    printf("Read in data :  %s \n",datafile);
    if (!strncmp(&(datafile[strlen(datafile)-4]),".dat",4)) iformat = 1;
    else if (!strncmp(&(datafile[strlen(datafile)-2]),".d",2)) iformat = 2;
    else iformat = 3;
    /* printf("iformat = %d \n",iformat); */

    /* Reading the Fortran binary */
    if ( iformat == 1 ) {
      space=(int *)calloc(2,sizeof(int));
      inp=fopen(datafile,"rb");
      fread(&junk,sizeof(int),1,inp); 
      fread(&OmegaM,sizeof(float),1,inp); 
      fread(&OmegaL,sizeof(float),1,inp); 
      fread(&OmegaB,sizeof(float),1,inp); 
      fread(&h,sizeof(float),1,inp);
      fread(space,sizeof(int),2,inp); 
      fread(aexpn,sizeof(float),1,inp);
      fread(nnx,sizeof(int),3,inp);
      fread(space,sizeof(int),2,inp);
      fread(ddx,sizeof(float),3,inp);
      fread(space,sizeof(int),2,inp);
      nsize=nnx[0];
      if (aexpn[0]>1.0) aexpn[0]=1.0;
      /* Read in Fortran-compatible array format */
      for (i=1;i<=nsize;i++) for (j=1;j<=nsize;j++) for (k=1;k<=nsize;k++) {
        fread(&sv[k][j][i],sizeof(float),1,inp);
	sum += sv[k][j][i];
      }
      /* testing the cube */
      printf("Om=%5.3f Ol=%5.3f Ob=%5.3f h=%5.3f \n",OmegaM,OmegaL,OmegaB,h);
      printf("nx[0,1,2]=%d %d %d \n",nnx[0],nnx[1],nnx[2]);
      printf("dx[0,1,2]=%f %f %f \n",ddx[0],ddx[1],ddx[2]);
      sum *= ddx[0]*ddx[0]*ddx[0];
      /* printf("sum = %10.5e \n",sum); */
      fread(&junk,sizeof(int),1,inp); 
      fclose(inp);
    }

    /* Reading the C binary */
    if ( iformat == 2 ) {
      inp=fopen(datafile,"rb");
      /* fscanf(inp,"%f %f %f %f \n",&OmegaM,&OmegaL,&OmegaB,&h); */
      fread(&OmegaM,sizeof(float),1,inp); 
      fread(&OmegaL,sizeof(float),1,inp); 
      fread(&OmegaB,sizeof(float),1,inp); 
      fread(&h,sizeof(float),1,inp);
      fread(aexpn,sizeof(float),1,inp);
      fread(nnx,sizeof(int),3,inp);
      fread(ddx,sizeof(float),3,inp);
      nsize=nnx[0];
      if (aexpn[0]>1.0) aexpn[0]=1.0;
      /* Read in C-compatible array format */
      for (i=1;i<=nsize;i++) for (j=1;j<=nsize;j++) for (k=1;k<=nsize;k++) 
        fread(&sv[i][j][k],sizeof(float),1,inp);
      fclose(inp);
    }
}


/* Read the PRO profiles */
void Read_pro ( profile, rvir )
    float *rvir;
    char  *profile;
{
    FILE *inp;
    char trash[200];
    int i;
    float a1,a2,a3,a4,a5,a6,a7,a8,a9;
    
    /* Reading profiles */
    printf("Read in PRO profiles:  %s \n",profile);
    inp=fopen(profile,"r");
    /* Read in the header junk */
    for(i=0;i<11;i++) fgets(trash,200,inp); 
    /* Read in the virial radius and masses */
    for(i=0;i<7;i++) {
      fgets(trash,200,inp);
      sscanf(trash,"%f %f %f %f %f %f %f %f %f \n",
	     &cl.delta[i],&cl.rvir[i],&cl.mvir[i],
	     &cl.digv[i],&cl.dibv[i],&cl.dgv[i],
	     &cl.dbv[i],&cl.ddmv[i],&cl.dtv[i]);
      printf("%f %f %f %f %f %f %f %f %f \n",
	     cl.delta[i],cl.rvir[i],cl.mvir[i],
	     cl.digv[i],cl.dibv[i],cl.dgv[i],
	     cl.dbv[i],cl.ddmv[i],cl.dtv[i]);
    } 
    fclose(inp);    
    /* sscanf(trash,"%s %f",tr,freq); */
}

/* Read the SZ profiles */
void Read_szpro ( profile, rvir )
    float *rvir;
    char  *profile;
{
    FILE *inp;
    char trash[200];
    int i;
    float a1,a2,a3,a4,a5,a6,a7,a8,a9;
    
    /* Reading profiles */
    printf("Read in SZ profiles:  %s \n",profile);
    inp=fopen(profile,"r");
    /* Read in the header junk */
    for(i=0;i<12;i++) fgets(trash,200,inp); 
    /* Read in the virial radius and masses */
    for(i=0;i<7;i++) {
      fgets(trash,200,inp);
      sscanf(trash,"%f %f %f %f %f %f %f %f %f %f %f \n",
	     &cl.delta[i],&cl.rvir[i],&cl.mvir[i],
	     &cl.digv[i],&cl.dibv[i],&cl.dgv[i],
	     &cl.dbv[i],&cl.pgv[i],&cl.pigv[i],
	     &cl.tmgv[i],&cl.tmigv[i]);
      /* printf("%f %f %f %f %f %f %f %f %f %f %f \n",
	     cl.delta[i],cl.rvir[i],cl.mvir[i],
	     cl.digv[i],cl.dibv[i],cl.dgv[i],
	     cl.dbv[i],cl.pgv[i],cl.pigv[i],
	     cl.tmgv[i],cl.tmigv[i]); */
    } 
    fclose(inp);    
    /* sscanf(trash,"%s %f",tr,freq); */
}

/* Average data cube to make a smaller cube */
void Average_Cube ( data, ng, iscale )
    float ***data;
    int ng, iscale;
{
    int i,j,k,ii,jj,kk,npix;
    float ***data2,***f3tensor();
    void  free_f3tensor();

    /* Make the temporary cube */
    data2=f3tensor(1,ng,1,ng,1,ng);	
    for (i=1;i<=ng;i++) for (j=1;j<=ng;j++) for (k=1;k<=ng;k++) 
      data2[i][j][k]=0.0;

    /* Average */
    npix=ng/iscale;
    for (i=1;i<=ng;i++) for (j=1;j<=ng;j++) for (k=1;k<=ng;k++) {
      ii=(int)(i-1)/iscale+1;
      jj=(int)(j-1)/iscale+1;
      kk=(int)(k-1)/iscale+1;
      data2[ii][jj][kk]+=data[i][j][k]/Cube(iscale);
    }
    
    /* Zero the original cube and copy the temporary one */
    for (i=1;i<=ng;i++) for (j=1;j<=ng;j++) for (k=1;k<=ng;k++) 
      data[i][j][k]=0.0;
    for (i=1;i<=ng;i++) for (j=1;j<=ng;j++) for (k=1;k<=ng;k++) 
      data[i][j][k]=data2[i][j][k];
    free_f3tensor(data2,1,ng,1,ng,1,ng);
}

/* Rotate cube using the CIC interpolation */  
void Rotate_Cube ( din, dout, ng, theta, phi )
    float ***din, ***dout;
    float theta, phi;
    int ng;
{
    int i,j,k,i1,j1,k1,ix,jx,kx,ix1,jx1,kx1;
    float xc,yc,zc,dx,dy,dz,tx,ty,tz,val;
    float xi,yi,zi,xp,yp,zp,rc;
    float l1,l2,l3,m1,m2,m3,n1,n2,n3;
    int icnt;
    
    /* Initialize the array & rotate cube */
    for (i=1;i<=ng;i++) for (j=1;j<=ng;j++) for (k=1;k<=ng;k++) 
      dout[i][j][k] = 0.0; 

    icnt=0;
    for (i=1;i<=ng;i++) for (j=1;j<=ng;j++) for (k=1;k<=ng;k++)
      if ( din[i][j][k] > 0.0 ) icnt+=1;
    /* printf(" # of pixel (before) = %d \n",icnt); */


    /* Rotate the cube and interpolate using the CIC interpolation scheme */
    rc = (ng+1)/2.0;    
    for (i=1;i<=ng;i++) for (j=1;j<=ng;j++) for (k=1;k<=ng;k++) {
      /* value at the original grid */
      val=din[i][j][k];
      
      /* Rotation about x-axis followed by the rotation around z-axis */
      xp=i-rc; yp=j-rc; zp=k-rc;      
      l1=cos(phi);             m1=sin(phi);             n1=0.0;
      l2=-cos(theta)*sin(phi); m2=cos(theta)*cos(phi);  n2=sin(theta);
      l3=sin(theta)*sin(phi);  m3=-sin(theta)*cos(phi); n3=cos(theta);
      xi=rc+l1*xp+m1*yp+n1*zp;
      yi=rc+l2*xp+m2*yp+n2*zp;
      zi=rc+l3*xp+m3*yp+n3*zp;
      /* printf("%d %d %d %f %f %f \n",i,j,k,xi,yi,zi); */
      ix=(int)xi; xc=(float)ix;
      jx=(int)yi; yc=(float)jx;
      kx=(int)zi; zc=(float)kx;
      if (ix<1 || jx<1 || kx<1 || ix>=ng+1 || jx>=ng+1 || kx>=ng+1) val=0.0; 

      dx=xi-xc; tx=1.0-dx;
      dy=yi-yc; ty=1.0-dy;
      dz=zi-zc; tz=1.0-dz;

      ix1=ix+1;  
      jx1=jx+1;  
      kx1=kx+1;  

      /* No periodic boundary condition */
      if (ix<1) ix=1; if (ix>ng) ix=ng; 
      if (jx<1) jx=1; if (jx>ng) jx=ng;
      if (kx<1) kx=1; if (kx>ng) kx=ng;
      
      if (ix1<1) ix1=1; if (ix1>ng) ix1=ng; 
      if (jx1<1) jx1=1; if (jx1>ng) jx1=ng; 
      if (kx1<1) kx1=1; if (kx1>ng) kx1=ng; 

      /* assign value to appropriate cells */
      dout[ix][jx][kx]+=val*tx*ty*tz;
      dout[ix1][jx][kx]+=val*dx*ty*tz;
      dout[ix][jx1][kx]+=val*tx*dy*tz;
      dout[ix1][jx1][kx]+=val*dx*dy*tz;
      dout[ix][jx][kx1]+=val*tx*ty*dz;
      dout[ix1][jx][kx1]+=val*dx*ty*dz;
      dout[ix][jx1][kx1]+=val*tx*dy*dz;
      dout[ix1][jx1][kx1]+=val*dx*dy*dz; 
    }
    
    icnt=0;
    for (i=1;i<=ng;i++) for (j=1;j<=ng;j++) for (k=1;k<=ng;k++) 
      if ( dout[i][j][k] > 0.0 ) icnt+=1;
    /* printf(" # of pixel (after)  = %d \n",icnt); */

}

/* Useful C-routines */

/* Calculate angular diameter distance [Mpc] for given z */
float dA(z)
  float z;
{
  float val;
  double dqromb(),dAintegrand();

  val=dqromb(dAintegrand,0.0,z);
  if ( 1.0-OmegaM-OmegaL > 1.0e-4  ) 
    val=sinh(sqrt(1.0-OmegaM-OmegaL)*val)/sqrt(1.0-OmegaM-OmegaL);
  if ( 1.0-OmegaM-OmegaL < -1.0e-4 ) 
    val=sin(sqrt(OmegaM+OmegaL-1.0)*val)/sqrt(OmegaM+OmegaL-1.0);
  val*=C/H0/h/(1.0+z);
  return(val);
}

/* Calculate angular diameter distance [Mpc] between zi and zf (zf > zi) */
float dA_zi_zf(zi,zf)
  float zi,zf;
{
  float val;
  double dqromb(),dAintegrand();

  val=dqromb(dAintegrand,zi,zf);
  if ( 1.0-OmegaM-OmegaL > 1.0e-4  ) 
    val=sinh(sqrt(1.0-OmegaM-OmegaL)*val)/sqrt(1.0-OmegaM-OmegaL);
  if ( 1.0-OmegaM-OmegaL < -1.0e-4 ) 
    val=sin(sqrt(OmegaM+OmegaL-1.0)*val)/sqrt(OmegaM+OmegaL-1.0);
  val*=C/H0/h/(1.0+zf);
  return(val);
}

/* integrand required to calculate angular diameter distances */
double dAintegrand(z)
        float z;
{
        double  val;

        val=1.0/(OmegaM*Cube(1.0+z)+(1.0-OmegaM-OmegaL)*Squ(1.0+z)+OmegaL);
        if (val>1.0e-20) val=sqrt(val);
        else val=0.0;
        return(val);
}

/* Choose smaller values */
float min(a,b)
     float a,b;
{
  float val;

  if (a>b) val=b;
  else if (a<=b) val=a;
  return(val);
}

/* Choose larger values */
float max(a,b)
     float a,b;
{
  float val;

  if (a<b) val=b;
  else if (a>=b) val=a;
  return(val);
}


/* Take log of a specified base */
float logx(xx,base)
     float xx;
     int base;
{
  float val;
  val = log(xx)/log(base);
  return(val);
}

/* ************************************************** */
/* ******* Useful routines for the SZ effect ******** */
/* ************************************************** */

/* Frequency dependence of the thermal SZ effect */
/* (delta T/T) = f_tsz(xfreq) * y */
/* xfreq = hP * FREQ / kB / T_CMB */
float f_tsz(x)
     float x;
{
  float ex,val;

  ex = exp(x);
  val = x*(ex+1.)/(ex-1.)-4.;
  return(val);
}

/* ************************************************** */
/* ****** Useful routines for the X-ray analyses **** */
/* ************************************************** */

double lambda_rs(t)
        float t;
{ 
        double  val;
	float tmin=1.0e+06;
        int     n;

	/* Interpolate in log temperature space */
        n=(int)Tlength*log10(t/Tlam[0])/log10(Tlam[Tlength-1]/Tlam[0]);
        if (n>Tlength-1) {
	  printf("Temperature out of range : n=%d t=%7.4f\n",n,t); 
	  exit(1); 
        }
	else if (n<0 || t<tmin)  
	  val=DBL_MIN;  /* Cold gas does not emit X-ray much : DBL_MIN=2.225074e-308 */
        else if (n==0) 
	  val=Lambda[0]+(t-Tlam[0])*(Lambda[1]-Lambda[0])/(Tlam[1]-Tlam[0]);
        else if (n==Tlength-1) 
	  val=Lambda[Tlength-1];
        else { /* interpolate in log space */
          val=Lambda[n]+(t-Tlam[n])*(Lambda[n+1]-Lambda[n])/(Tlam[n+1]-Tlam[n]);
	  /* printf("Check %e %e \n",t,val); */
        }
        return(val);
}

