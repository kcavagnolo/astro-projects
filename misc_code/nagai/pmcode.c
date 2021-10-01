/* pmcode.c                     */
/* PM code                      */
/* Daisuke Nagai                */
/* Last modified: March 17,2002 */

#include "PMparameters.h"

main(argc,argv)
    int argc;
    char *argv[];
{
        int i,j,k,istep,namax;
	float *x,*y,*z,*px,*py,*pz,***phi,*vector(),**matrix(),***f3tensor();
	void force_test(),zeldovich1D(),zeldovichCosmo(),density(),poisson(),
	  poisson2(),update(),accuracy(),output_1D(),output_cosmo(),
	  create_white_noise(),power();
	void free_vector(),free_matrix(),free_f3tensor(),printerror();

	/* Total number of particles and cells in 3D */
	N3par=Cube(Npar);
	N3grid=Cube(Ngrid);
	Mpar=N3grid/N3par;

	/* Define vectors and matrix */
	x=vector(1,N3par);
	y=vector(1,N3par);
	z=vector(1,N3par);
	px=vector(1,N3par);
	py=vector(1,N3par);
	pz=vector(1,N3par);
	phi=f3tensor(1,Ngrid,1,Ngrid,1,Ngrid);

	/* Define epoch of the caustic formation */
	Across=1.0;
	/* Find the total number of time steps */
	Nstep=(Across-Aexp0)/Astep;

	/* Print parameters for the PM code */
	puts(" Running the PM code");
	printf(" Npar %d Ngrid %d \n",Npar,Ngrid);
	printf(" Om %3.1f Ok %3.1f Ol %3.1f \n",Om,Ok,Ol);
	printf(" Aexp0 %5.3f Across %5.3f Astep %5.3f Nstep %d \n",Aexp0,Across,Astep,Nstep);
	puts(" ");

	/* (00) Testing the Poisson solver & force accuracy using a point particle */
	if (Flag==Force) {
	  Aexpn=1.0;
	  force_test(phi);
	  exit(0); 
	}

	/* (0) Initialize positions and velocities using */
	/*     the Zel'dovich approximation              */
	Infile_power="powspec_Om1_h0.7_n1.dat";
	if (Flag==ZAtest) zeldovich1D(x,y,z,px,py,pz); 
	if (Flag==Cosmo)  zeldovichCosmo(x,y,z,px,py,pz);
	if (Flag==Power) {
	  zeldovichCosmo(x,y,z,px,py,pz);
	  /* Calculate the power spectrum of the initial density field */
	  Outfile_power="power.dat";
	  power(x,y,z);
	  /* Create white noise */
	  Outfile_power="whitenoise.dat";
	  create_white_noise(x,y,z);
	  power(x,y,z);
	  exit(0); 
	}

	Aexpn=Aexp0;
	for (istep=0;istep<=Nstep;istep++) {
	  printf("Aexpn %f \n",Aexpn);
	  /* (1) Density assignment: assign density to the grid pts. */
	  /* puts("1: Density assignment"); */
	  density(phi,x,y,z);
	  /* (2) Poisson solver: feed in rho & solve for phi @ grid pt. */
	  /* puts("2: Poisson solver"); */
	  poisson(phi);
	  /* poisson2(phi); */
	  /* (3) Update position and velocities & step in time */
	  /* puts("3: Update positions and velocities & step in time"); */
	  update(phi,x,y,z,px,py,pz);
	}

	/* Output the final positions and velocities of particles */
	if (Flag==ZAtest) output_1D(x,y,z,px,py,pz); 
	if (Flag==Cosmo) output_cosmo(x,y,z,px,py,pz);
	
	free_vector(x,1,N3par);
	free_vector(y,1,N3par);
	free_vector(z,1,N3par);
	free_vector(px,1,N3par);
	free_vector(py,1,N3par);
	free_vector(pz,1,N3par);
	free_f3tensor(phi,1,Ngrid,1,Ngrid,1,Ngrid);
}


/** (00) Testing the Poisson solver & force accuracy using one particle **/
void force_test(phi)
        float ***phi;
{
        int i,j,k,ic,ip,np;
	float *xx,*yy,*zz,*vector(),r,gexact,error,gp(),ran1();
	float xc,yc,zc,rp,tp,pp,gx,gy,gz,ggx,ggy,ggz,gcode;
	void poisson(),poisson2(),free_vector();
	time_t seed; 
        long iseed;
        FILE *out;

	/* Total number of particles to throw down */
	np=50000;
	
	/* Define vectors and matrix */
	xx=vector(1,np);
	yy=vector(1,np);
	zz=vector(1,np);

	/* Center of the 3D cubic box */
	ic=Ngrid/2;
	xc=yc=zc=(float)ic;

	/* Initalize the 3D grid array for density contrast */
	for (i=1;i<=Ngrid;i++) for(j=1;j<=Ngrid;j++) for(k=1;k<=Ngrid;k++)
	  phi[i][j][k]=-1.0;
	/* Place a particle with m=1.0 in the center of the grid */
	/* Mp=N3grid/Np=N3grid (Np=1) in this case */
	phi[ic][ic][ic]+=N3grid;

	/* Now solve the potential of the single particle */
	poisson(phi);

	/* Throw particles uniformly in radius using random number generator */
	seed=-1.0*(time(NULL)%100000);
        iseed=(long)seed;
	for (ip=1;ip<=np;ip++) {
	  /* Throwing particles uniformly in radius */
	  rp=(xc-1.0)*ran1(&iseed);
	  tp=PI*ran1(&iseed);
	  pp=2.0*PI*ran1(&iseed);
	  /* Find positions of particles in rectangular coordinates */
	  xx[ip]=rp*sin(tp)*cos(pp)+xc;
	  yy[ip]=rp*sin(tp)*sin(pp)+yc;
	  zz[ip]=rp*cos(tp)+zc;
	}
		
	/* Compare the accelerations to the exact solution @ particle positioins */
        out=fopen("force_test.dat","wb");
	for (ip=1;ip<=np;ip++) {	  
	  gx=xx[ip]-xc;
	  gy=yy[ip]-yc;
	  gz=zz[ip]-zc;
	  r=sqrt(Squ(gx)+Squ(gy)+Squ(gz)); 
	  gexact=3.0*Om/(8.0*PI*Aexpn)*N3grid*r/Cube(r);
	  /* Evaluating acceleration at the center of the cubic box */
	  ggx=gp(phi,xx,yy,zz,ip,'x');
	  ggy=gp(phi,xx,yy,zz,ip,'y');
	  ggz=gp(phi,xx,yy,zz,ip,'z');
	  gcode=sqrt(Squ(ggx)+Squ(ggy)+Squ(ggz));
	  error=(gcode-gexact)/gexact;
	  fprintf(out,"%f %f %f %f\n",r,gcode,gexact,error);
	}
	fclose(out);

	free_vector(xx,1,np);
	free_vector(yy,1,np);
	free_vector(zz,1,np);
}


/** (0-1) Initial conditions for the 1D Zel'dovich pancake collapse **/
void zeldovich1D(x,y,z,px,py,pz)
        float *x,*y,*z,*px,*py,*pz;
{
        int i,j,k,ip;
        float kw,*qx,*qy,*qz,kxq,kyq,kzq,ampx,ampy,ampz,Dg(),dDg(),rgp;
	float *vector();
	void free_vector();
  
	/* Define vector for Lagrangian coordinates */
	qx=vector(1,N3par);
	qy=vector(1,N3par);
	qz=vector(1,N3par);

	/* Wavenumber to simulate */
	kw=2.0*PI/Ngrid;
	rgp=Ngrid/Npar;

	/* Assign initial positions and velocities of all particles */
	for (i=1;i<=Npar;i++) for (j=1;j<=Npar;j++) for (k=1;k<=Npar;k++) {
	  /* Distribute Np particles uniformly in the simulations box */
	  ip=Squ(Npar)*(i-1)+Npar*(j-1)+k;
	  /* Lagrangian coordinate of the nth particle */
	  qx[ip]=i*rgp;
	  qy[ip]=j*rgp;
	  qz[ip]=k*rgp;  
	  kxq=kw*qx[ip];
	  /* Amplitude of the initial perturbation in x-direction */
	  ampx=1.0/(Dg(Across)*kw);
	  /* Assign initial positions and velocities @ x,y,z */
	  x[ip]=qx[ip]+Dg(Aexp0)*ampx*sin(kxq);
	  y[ip]=qy[ip];
	  z[ip]=qz[ip];
	  px[ip]=Squ(Aexp0-0.5*Astep)*dDg(Aexp0-0.5*Astep)*ampx*sin(kxq);
	  py[ip]=0.0;
	  pz[ip]=0.0;
	}
	free_vector(qx,1,N3par);
	free_vector(qy,1,N3par);
	free_vector(qz,1,N3par);	  
}

/** (0-2) Initial conditions for the realistic cosmological simulations **/
void zeldovichCosmo(x,y,z,px,py,pz)
        float *x,*y,*z,*px,*py,*pz;
{
        int i,j,k,l,m,n,lm,mm,nm,le,me,ne,ip,length;
        float *qx,*qy,*qz,kxq,kyq,kzq,Dg(),dDg(),xn,yn,kw,rgp,fbox;
	float deltax,deltay,deltaz,fxmin,fymin,fzmin,fxmax,fymax,fzmax;
	float *kk,*pk,*pk2nd,ktot,ptot,dum,*vector(),ak,bk,amp,gasdev();
	float ftnorm,***ft_x,***ft_y,***ft_z,**ftspec_x,**ftspec_y,**ftspec_z,
	  ***f3tensor(),**matrix();
	char file[200],dmp[100],tr[200];
	time_t seed; 
        long iseed;
	void spline(),splint(),rlft3(),free_vector(),free_f3tensor(),free_matrix();
	FILE *inp;
	
	/* Define useful constants */
	kw=2.0*PI/Ngrid;
	rgp=Ngrid/Npar;
        fbox=Lbox/Ngrid;

	/* Read in the Power Spectrum and Interpolate */
	sprintf(file,Infile_power);
	inp=fopen(Infile_power,"r");
	if (inp==NULL) {printf("File %s not found\n",file);exit(0);}
	/* Read through file to find out how many data lines there are */
	length=0;
        while(fgets(tr,200,inp)!=NULL) if (strncmp(tr,"#",1)) length++; 
	fclose(inp);
	/* now allocate appropriate amount of space */
        kk=vector(1,length);
        pk=vector(1,length);
	pk2nd=vector(1,length);
	printf("Reading %d line from %s\n",length,file);
	
	/* Read in data from the file and load arrays */
	inp=fopen(Infile_power,"r");
	for (i=1;i<=length;i++) {
	  fscanf(inp,"%f %f %f \n",&kk[i],&dum,&pk[i]);
	  /* Convert k [h^-1 Mpc] to k in code units */
	  kk[i]*=fbox; 
	  /* Convert Delta^2(k) to P(k) at z=0.0 in code units */
	  pk[i]*=(2.0*Squ(PI)/Cube(kk[i]));
	  /* Scale P(k) at z=0.0 back to the initial epoch, zi */
	  pk[i]*=Squ(Dg(Aexp0));
	}
	fclose(inp);

	/* Spline interpolate the power spectrum */
	spline(kk,pk,length,0.0,0.0,pk2nd);

	/* Seeds for the random number generator */
	seed=-1.0*(time(NULL)%100000);
        iseed=(long)seed;
	
	/* Define tensors and matrices for the real FFT (rlft3) */
	ft_x=f3tensor(1,Npar,1,Npar,1,Npar);
	ft_y=f3tensor(1,Npar,1,Npar,1,Npar);
	ft_z=f3tensor(1,Npar,1,Npar,1,Npar);
       	ftspec_x=matrix(1,Npar,1,2*Npar);
	ftspec_y=matrix(1,Npar,1,2*Npar);
	ftspec_z=matrix(1,Npar,1,2*Npar);

	/* Sample the real and imaginary components of the Fourier */
	/* coefficients that are assumed to be independent gaussian */
	/* random numbers and fills the complex array for rlft3 FFT */
       	for (l=1;l<=Npar;l++) for(m=1;m<=Npar;m++) for(n=1;n<=Npar/2+1;n++) {
	  /* CAUTION : no minus sign needed for kxq, kyq */
	  /* Minus sign in bk cancels with minus sign in kxq, kyq, kzq */
	  /* The same symmetry in bk and kxq, kyq, kzq */
	  le=l-1;  if (le>Npar/2) le=le-Npar;  kxq=kw*le;
	  me=m-1;  if (me>Npar/2) me=me-Npar;  kyq=kw*me;
	  ne=n-1;  kzq=kw*ne;
	  ktot=sqrt(Squ(kxq)+Squ(kyq)+Squ(kzq));
	  splint(kk,pk,pk2nd-1,length,ktot,&ptot);
	  amp=0.5*sqrt(ptot)/Squ(ktot);
	  if (l==m==n==1) amp=0.0;
	  if (n<=(int)Npar/2) {
	    /* Sample the real and imaginary components for kx */
	    ak=amp*gasdev(&iseed);
	    bk=amp*gasdev(&iseed);	    
	    ft_x[l][m][2*n-1]=kxq*bk;
	    ft_x[l][m][2*n]=kxq*ak;
	    /* Sample the real and imaginary components for ky */
	    ak=amp*gasdev(&iseed);
	    bk=amp*gasdev(&iseed);
	    ft_y[l][m][2*n-1]=kyq*bk;
	    ft_y[l][m][2*n]=kyq*ak;
	    /* Sample the real and imaginary components for kz */
	    ak=amp*gasdev(&iseed);
	    bk=amp*gasdev(&iseed);
	    ft_z[l][m][2*n-1]=kzq*bk;
	    ft_z[l][m][2*n]=kzq*ak;
	  }
	  /* Take care of z-axis Nyquist critical frequency components */
	  else if (n==(int)Npar/2+1) {
	    /* Sample the real and imaginary components for kx */
	    ak=amp*gasdev(&iseed);
	    bk=amp*gasdev(&iseed);
	    ftspec_x[l][2*m-1]=kxq*bk;
	    ftspec_x[l][2*m]=kxq*ak;
	    /* Sample the real and imaginary components for ky */
	    ak=amp*gasdev(&iseed);
	    bk=amp*gasdev(&iseed);
	    ftspec_y[l][2*m-1]=kyq*bk;
	    ftspec_y[l][2*m]=kyq*ak;
	    /* Sample the real and imaginary components for kz */
	    ak=amp*gasdev(&iseed);
	    bk=amp*gasdev(&iseed);
	    ftspec_z[l][2*m-1]=kzq*bk;
	    ftspec_z[l][2*m]=kzq*ak;
	  }
	}

	/* Imposing the symmetry to use the rlft3 routine (Brute force way!) */
	for (l=1;l<=Npar;l++) for(m=1;m<=Npar;m++) for(n=1;n<=Npar/2+1;n++) {
	  if (l==1) lm=1;  else lm=Npar-l+2;
	  if (m==1) mm=1;  else mm=Npar-m+2;
	  if (l==m==n==1) amp=0.0;
	  else if (n<=(int)Npar/2) {
	    ft_x[lm][mm][2*n-1]=ft_x[l][m][2*n-1];
	    ft_x[lm][mm][2*n]=-1.0*ft_x[l][m][2*n];
	    ft_y[lm][mm][2*n-1]=ft_y[l][m][2*n-1];
	    ft_y[lm][mm][2*n]=-1.0*ft_y[l][m][2*n];
	    ft_z[lm][mm][2*n-1]=ft_z[l][m][2*n-1];
	    ft_z[lm][mm][2*n]=-1.0*ft_z[l][m][2*n];
	  }
	  /* Take care of z-axis Nyquist critical frequency components */
	  else if (n==(int)Npar/2+1) {
	    ftspec_x[lm][2*mm-1]=ftspec_x[l][2*m-1];
	    ftspec_x[lm][2*mm]=-1.0*ftspec_x[l][2*m];
	    ftspec_y[lm][2*mm-1]=ftspec_y[l][2*m-1];
	    ftspec_y[lm][2*mm]=-1.0*ftspec_y[l][2*m];
	    ftspec_z[lm][2*mm-1]=ftspec_z[l][2*m-1];
	    ftspec_z[lm][2*mm]=-1.0*ftspec_z[l][2*m];
	  }
	}

	/* Take the inverse-Fourier transform of the three grids */
	/* to find how much we should displace each particle from */
	/* the uniform distribution in each direction */
	rlft3(ft_x,ftspec_x,Npar,Npar,Npar,-1);
	rlft3(ft_y,ftspec_y,Npar,Npar,Npar,-1);
	rlft3(ft_z,ftspec_z,Npar,Npar,Npar,-1);
	/* Normalization factor for the inverse FFT */
	ftnorm=2.0/N3par;
	/* Volume factor and 2PI's in definition of delta(k) */
	ftnorm=ftnorm*N3grid/Cube(2.0*PI); 
	ftnorm*=PI; 
	
	/* Define vector for Lagrangian coordinates */
	qx=vector(1,N3par);
	qy=vector(1,N3par);
	qz=vector(1,N3par);

	/* Assign initial positions and velocities of all particles */
	/* using the displacement vectors for all three directions */
	deltax=deltay=deltaz=0.0;
	fxmin=fymin=fzmin=1.0e+30;
	fxmax=fymax=fzmax=-1.0e+30;
	xn=(float)Ngrid+1.0+1.0e-20;
	yn=(float)Ngrid;
	for (i=1;i<=Npar;i++) for (j=1;j<=Npar;j++) for (k=1;k<=Npar;k++) {
	  /* Distribute Np particles uniformly in the simulations box */
	  ip=Squ(Npar)*(i-1)+Npar*(j-1)+k;
	  /* Lagrangian coordinate of the nth particle */
	  qx[ip]=i*rgp;
	  qy[ip]=j*rgp;
	  qz[ip]=k*rgp;  
	  /* Assign initial positions and velocities @ x,y,z */
	  x[ip]=qx[ip]+Dg(Aexp0)*ftnorm*ft_x[i][j][k];
	  y[ip]=qy[ip]+Dg(Aexp0)*ftnorm*ft_y[i][j][k];
	  z[ip]=qz[ip]+Dg(Aexp0)*ftnorm*ft_z[i][j][k];
	  px[ip]=Squ(Aexp0-0.5*Astep)*dDg(Aexp0-0.5*Astep)*ftnorm*ft_x[i][j][k];
	  py[ip]=Squ(Aexp0-0.5*Astep)*dDg(Aexp0-0.5*Astep)*ftnorm*ft_y[i][j][k];
	  pz[ip]=Squ(Aexp0-0.5*Astep)*dDg(Aexp0-0.5*Astep)*ftnorm*ft_z[i][j][k];
	  /* Periodical conditions */
	  if(x[ip]<1.0) x[ip]+=yn;
	  if(x[ip]>=xn) x[ip]-=yn;
	  if(y[ip]<1.0) y[ip]+=yn;
	  if(y[ip]>=xn) y[ip]-=yn;
	  if(z[ip]<1.0) z[ip]+=yn;
	  if(z[ip]>=xn) z[ip]-=yn;
	  /* Calculare the rms fluctuations */
	  if (fxmin>(x[ip]-qx[ip])) fxmin=x[ip]-qx[ip];
	  if (fxmax<(x[ip]-qx[ip])) fxmax=x[ip]-qx[ip];
	  if (fymin>(y[ip]-qy[ip])) fymin=y[ip]-qy[ip];
	  if (fymax<(y[ip]-qy[ip])) fymax=y[ip]-qy[ip];
	  if (fzmin>(z[ip]-qz[ip])) fzmin=z[ip]-qz[ip];
	  if (fzmax<(z[ip]-qz[ip])) fzmax=z[ip]-qz[ip]; 
	  deltax+=Squ(x[ip]-qx[ip])/N3par;
	  deltay+=Squ(y[ip]-qy[ip])/N3par;
	  deltaz+=Squ(z[ip]-qz[ip])/N3par;
	  /* printf("%f %f %f \n",ft_x[i][j][k],ft_y[i][j][k],ft_z[i][j][k]); */
	  /* printf("%f %f %f %f %f %f \n",x[ip],y[ip],z[ip],px[ip],py[ip],pz[ip]); */ 
	}
	
	/* RMS fluctuations */
	deltax=sqrt(deltax);
	deltay=sqrt(deltay);
	deltaz=sqrt(deltaz);
	printf("RMS : x %le y %le z %le \n",deltax,deltay,deltaz);
	/* printf("fxmin %le fxmax %le\n",fxmin,fxmax);
	printf("fymin %le fymax %le\n",fymin,fymax);
	printf("fzmin %le fzmax %le\n",fzmin,fzmax); */

	free_f3tensor(ft_x,1,Npar,1,Npar,1,Npar);
	free_f3tensor(ft_y,1,Npar,1,Npar,1,Npar);
	free_f3tensor(ft_z,1,Npar,1,Npar,1,Npar);
       	free_matrix(ftspec_x,1,Npar,1,Npar);
	free_matrix(ftspec_y,1,Npar,1,Npar);
	free_matrix(ftspec_z,1,Npar,1,Npar);
	free_vector(qx,1,N3par);
	free_vector(qy,1,N3par);
	free_vector(qz,1,N3par);	  
}

/* Growth factor appropriate for given cosmology */
float Dg(a)
        float a;
{
        float val;
	val=a;
	return(val);
}

/* Conformal time derivative of the growth factor */
float dDg(a)
        float a;
{
        float hubble(),val;
	val=a*hubble(a);
	return(val);
}
 
/* hubble parameter @ some epoch */
float hubble(a)
        float a;
{
        float am,val;
	/* In code units, H0 scales out */
        am=1.0/a;
        val=sqrt(Om*Cube(am)+Ok*Squ(am)+Ol);
	return(val);
}

/** (1) Assign density @ grid pts. **/
void density(phi,x,y,z)
        float ***phi,*x,*y,*z;
{
	int ip,i,j,k,i1,j1,k1;
	float xc,yc,zc,dx,dy,dz,tx,ty,tz;

	/* Initalize the 3D grid array for density contrast */
	for (i=1;i<=Ngrid;i++) for(j=1;j<=Ngrid;j++) for(k=1;k<=Ngrid;k++) 
          phi[i][j][k]=-1.0;

	/* Assign density @ grid pts using the CIC scheme */
	for (ip=1;ip<=N3par;ip++) {  
	  /* loop over particles */
	  i=(int)x[ip]; xc=(float)i;
	  j=(int)y[ip]; yc=(float)j;
	  k=(int)z[ip]; zc=(float)k;
	  if (i<1 || j<1 || k<1 || i>=Ngrid+1 || j>=Ngrid+1 || k>=Ngrid+1) 
	    printf("%d %d %d \n",i,j,k);

	  dx=x[ip]-xc; tx=1.0-dx;
	  dy=y[ip]-yc; ty=1.0-dy;
	  dz=z[ip]-zc; tz=1.0-dz;

	  i1=i+1;  if (i1>Ngrid) i1=1;
	  j1=j+1;  if (j1>Ngrid) j1=1;
	  k1=k+1;  if (k1>Ngrid) k1=1;

	  /* assign density to appropriate cells */
	  /* printf("%d %d %d %d\n",i,j,k,ip); */
	  phi[i][j][k]+=Mpar*tx*ty*tz;
	  phi[i1][j][k]+=Mpar*dx*ty*tz;
	  phi[i][j1][k]+=Mpar*tx*dy*tz;
	  phi[i1][j1][k]+=Mpar*dx*dy*tz;
	  phi[i][j][k1]+=Mpar*tx*ty*dz;
	  phi[i1][j][k1]+=Mpar*dx*ty*dz;
	  phi[i][j1][k1]+=Mpar*tx*dy*dz;
	  phi[i1][j1][k1]+=Mpar*dx*dy*dz; 
	}
}

/** (2-1) Poisson solver using "rlft3" FFT routine **/
void poisson(phi)
	float ***phi;
{
        int i,j,k,l,m,n,xdim,ydim,zdim;
	float ftnorm,Green(),***ft_data,**ftspec_data,***f3tensor(),**matrix();
	void  rlft3(),free_f3tensor(),free_matrix();

	/* set up arrays for Fouriers transforms of 3D density field */
	xdim=ydim=zdim=Ngrid;
	ft_data=f3tensor(1,xdim,1,ydim,1,zdim);        
        ftspec_data=matrix(1,xdim,1,2*ydim);

	/* load density field into the 3D complex arrays for fourn() */
	for(i=1;i<=xdim;i++) for(j=1;j<=ydim;j++) for (k=1;k<=zdim;k++) 
	  ft_data[i][j][k]=phi[i][j][k];
 
	/* Calculate the Fourier Transform of 3D density field */
	/* puts(" Calculating FFT of 3D density field."); */
	rlft3(ft_data,ftspec_data,xdim,ydim,zdim,1);

	/* Multiply every element in the field delta(k_lmn) by G(k_lmn) */
	/* in FT space */
	for (l=1;l<=xdim;l++) for(m=1;m<=ydim;m++) for(n=1;n<=(int)zdim/2;n++) {
	  ft_data[l][m][2*n-1]*=Green(l,m,n);
	  ft_data[l][m][2*n]*=Green(l,m,n); 
	}
	/* Take care of z-axis Nyquist critical frequency components */
	n=(int)zdim/2+1;
	for (l=1;l<=xdim;l++) for(m=1;m<=ydim;m++) {
	  ftspec_data[l][2*m-1]*=Green(l,m,n);
	  ftspec_data[l][2*m]*=Green(l,m,n); 
	}

	/* Inverse FT to obtain the grav field @ grid positions */
	/* NOTE: must divide by (zdim*ydim*xdim/2) from proper normalization */
	/* puts(" Calculating inverse FFT."); */
	rlft3(ft_data,ftspec_data,xdim,ydim,zdim,-1);

	/* un-wrap the FT array */
	/* printf(" Unwrapping the convolved image.\n"); */
	ftnorm=2.0/(float)(xdim*ydim*zdim);
	for(i=1;i<=xdim;i++) for(j=1;j<=ydim;j++) for (k=1;k<=zdim;k++)
	  phi[i][j][k]=ft_data[i][j][k]*ftnorm;

	free_f3tensor(ft_data,1,Ngrid,1,Ngrid,1,Ngrid);        
        free_matrix(ftspec_data,1,Ngrid,1,2*Ngrid);
}

/* (2-2) 2nd Poisson solver using "fourn" FFT routine */
void poisson2(phi)
     float ***phi;
{
     int i,j,k,l,m,n,loc,xdim,ydim,zdim,*nx,ndim,*ivector();
     float ftnorm,Green(),*data,*vector();
     void fourn(),free_ivector(),free_vector();

     /* set up arrays for Fouriers transforms of 3D density field */
     data=vector(1,2*Cube(Ngrid));
     nx=ivector(1,3);
     nx[1]=nx[2]=nx[3]=Ngrid;
     ndim=3;
     xdim=ydim=zdim=Ngrid;
     
     /* load density field into the 3D complex arrays for fourn() */
     loc=1;
     for(i=1;i<=xdim;i++) for(j=1;j<=ydim;j++) for (k=1;k<=zdim;k++) {
       data[loc++]=phi[i][j][k];
       data[loc++]=0.0;
     }

     /* Calculate the Fourier Transform of 3D density field */
     /* puts(" Calculating FFT of 3D density field."); */
     fourn(data,nx,ndim,1);

     /* Multiply every element in the field delta(k_lmn) by G(k_lmn) */
     /* in FT space */
     loc=1;
     for (l=1;l<=xdim;l++) for(m=1;m<=ydim;m++) for(n=1;n<=zdim;n++) {
        data[loc++]*=Green(l,m,n);
	data[loc++]*=Green(l,m,n);
     }
     
     /* Inverse FFT to obtain the grav potential @ grid positions */
     /* NOTE: must divide by (zdim*ydim*xdim/2) from proper normalization */
     /* puts(" Calculating inverse FFT of product."); */
     fourn(data,nx,ndim,-1);

     ftnorm=1.0/(float)(zdim*ydim*xdim);
     loc=1;
     for (i=1;i<=xdim;i++) for(j=1;j<=ydim;j++) for (k=1;k<=zdim;k++) { 
       phi[i][j][k]=data[loc]*ftnorm;
       loc+=2;
     }

     free_vector(data,1,2*Cube(Ngrid));     
     free_ivector(nx,1,3);
}

/* Green's function */
float Green(l,m,n)
	int l,m,n;
{
        float kw,kx,ky,kz,val,sin2();
	
	l=l-1;  if (l>=Ngrid/2) l=-1*(Ngrid-l);
	m=m-1;  if (m>=Ngrid/2) m=-1*(Ngrid-m);
	n=n-1;  if (n>=Ngrid/2) n=-1*(Ngrid-n);  

	/* NOTE: Ok to use this too becase of the symmetry */
	/*  l = l-1;
	    m = m-1;
	    n = n-1; */

	kw=2.0*PI/Ngrid;
	kx=kw*l;
	ky=kw*m;
	kz=kw*n;
	if (l==0 && m==0 && n==0) val=0.0;
	else val=-(3.0*Om/8.0/Aexpn)/(sin2(kx/2.0)+sin2(ky/2.0)+sin2(kz/2.0));
	return(val);
}

float sin2(x)
	float x;
{
        float val;
	val=Squ(sin(x));
	return(val);
}

/**  (3) updating particle velocities and positions **/
void update(phi,x,y,z,px,py,pz)
	float ***phi,*x,*y,*z,*px,*py,*pz;
{
	int ip;
	float xn,yn,ahalf,f(),gp(),faexpn,fahalf;
	float svel,sphi,fphi();
	
	ahalf=Aexpn+0.5*Astep;
	faexpn=f(Aexpn)*Astep;
	fahalf=f(ahalf)*Astep/Squ(ahalf);
	xn=(float)Ngrid+1.0+1.0e-20;
	yn=(float)Ngrid;
	svel=0.0;  /* counter for Sum(v_i**2) */          
	sphi=0.0;  /* counter for Sum(phi_i)  */
	/* Update particle velocities and then positions */
	for (ip=1;ip<=N3par;ip++) {
	  px[ip]+=faexpn*gp(phi,x,y,z,ip,'x');
	  py[ip]+=faexpn*gp(phi,x,y,z,ip,'y');
	  pz[ip]+=faexpn*gp(phi,x,y,z,ip,'z');
	  x[ip]+=fahalf*px[ip];
	  y[ip]+=fahalf*py[ip];
	  z[ip]+=fahalf*pz[ip];
	  /* Periodical conditions */
          if(x[ip]<1.0) x[ip]+=yn;
	  if(x[ip]>=xn) x[ip]-=yn;
	  if(y[ip]<1.0) y[ip]+=yn;
	  if(y[ip]>=xn) y[ip]-=yn;
	  if(z[ip]<1.0) z[ip]+=yn;
	  if(z[ip]>=xn) z[ip]-=yn;
	}
	/* Increment the time step */
	Aexpn+=Astep;
}

float f(a)
	float a;
{
	float val;
	val=sqrt(a/(Om+Ok*a+Ol*Cube(a)));
	return(val);		
}

/* Acceleration @ particle position */
float gp(phi,x,y,z,ip,ch)
	int ip;
	float ***phi,*x,*y,*z;
	char ch;
{
	int i,j,k,i1,j1,k1;
	float xc,yc,zc,dx,dy,dz,tx,ty,tz,val,gg();

	/* loop over particles */
	i=(int)x[ip]; xc=(float)i;
	j=(int)y[ip]; yc=(float)j;
	k=(int)z[ip]; zc=(float)k;
			
	dx=x[ip]-xc; tx=1.0-dx;
	dy=y[ip]-yc; ty=1.0-dy;
	dz=z[ip]-zc; tz=1.0-dz;

	i1=i+1;  if(i1>Ngrid) i1=1;
	j1=j+1;  if(j1>Ngrid) j1=1;
	k1=k+1;  if(k1>Ngrid) k1=1;

	/* Interpolating the acceleration @ particle positions */
	val=gg(phi,i,j,k,ch)*tx*ty*tz+gg(phi,i1,j,k,ch)*dx*ty*tz+
	    gg(phi,i,j1,k,ch)*tx*dy*tz+gg(phi,i1,j1,k,ch)*dx*dy*tz+
	    gg(phi,i,j,k1,ch)*tx*ty*dz+gg(phi,i1,j,k1,ch)*dx*ty*dz+
	    gg(phi,i,j1,k1,ch)*tx*dy*dz+gg(phi,i1,j1,k1,ch)*dx*dy*dz;
	return(val);
}

/* Acceleration @ grid position */
float gg(phi,i,j,k,ch)
	float ***phi;
	int i,j,k;
	char ch;
{
        int i1,j1,k1,im1,jm1,km1;
	float val;

	i1=i+1;  im1=i-1;  if(i1>Ngrid) i1=1;  if(im1<1) im1=Ngrid-im1;
	j1=j+1;  jm1=j-1;  if(j1>Ngrid) j1=1;  if(jm1<1) jm1=Ngrid-jm1;
	k1=k+1;  km1=k-1;  if(k1>Ngrid) k1=1;  if(km1<1) km1=Ngrid-km1;

	if (ch=='x') val=-(phi[i1][j][k]-phi[im1][j][k])/2.0;
	if (ch=='y') val=-(phi[i][j1][k]-phi[i][jm1][k])/2.0;
	if (ch=='z') val=-(phi[i][j][k1]-phi[i][j][km1])/2.0;
	return(val);
}

/* Potential @ particle position */
float fphi(phi,x,y,z,ip)
	int ip;
	float ***phi,*x,*y,*z;
{
	int i,j,k,i1,j1,k1;
	float xc,yc,zc,dx,dy,dz,tx,ty,tz,val,gg();

	/* loop over particles */
	i=(int)x[ip]; xc=(float)i;
	j=(int)y[ip]; yc=(float)j;
	k=(int)z[ip]; zc=(float)k;
			
	dx=x[ip]-xc; tx=1.0-dx;
	dy=y[ip]-yc; ty=1.0-dy;
	dz=z[ip]-zc; tz=1.0-dz;

	i1=i+1;  if (i1>Ngrid) i1=1;
	j1=j+1;  if (j1>Ngrid) j1=1;
	k1=k+1;  if (k1>Ngrid) k1=1;

	/* Interpolating the potential @ particle positions */
	val=phi[i][j][k]*tx*ty*tz+phi[i1][j][k]*dx*ty*tz+
	    phi[i][j1][k]*tx*dy*tz+phi[i1][j1][k]*dx*dy*tz+
	    phi[i][j][k1]*tx*ty*dz+phi[i1][j][k1]*dx*ty*dz+
	    phi[i][j1][k1]*tx*dy*dz+phi[i1][j1][k1]*dx*dy*dz;
	return(val);
}


/**  Compare the results of the PM code to the 1D exact solution **/
void output_1D(x,y,z,px,py,pz)
     float *x,*y,*z,*px,*py,*pz;
{       
       int i,j,k,ip;
       float ampx,Dg(),dDg(),kw,qx,kxq;
       float xexact,pexact;
       FILE *out;

       /* Wavenumber to simulate */
       kw=2.0*PI/Ngrid;
       /* Amplitude of the initial perturbation in x-direction */
       ampx=1.0/(Dg(Across)*kw);

       /* Writing outputs to file */
       out=fopen("results.dat","wb");
       /* for (i=1;i<=Npar;i++) for (j=1;j<=Npar;j++) for (k=1;k<=Npar;k++) {
	 ip=Squ(Npar)*(i-1)+Npar*(j-1)+k;
	 qx=i*Ngrid/Npar;
	 kxq=kw*qx;
	 xexact=qx+Dg(Across)*ampx*sin(kxq);
	 pexact=Squ(Across-0.5*Astep)*dDg(Across-0.5*Astep)*ampx*sin(kxq);
	 fprintf(out,"%f %f %f %f %e\n",qx,x[ip],px[ip],xexact,pexact);
       }
       fclose(out); */

       for (i=1;i<=Npar;i++) {
	 ip=Squ(Npar)*(i-1)+1;
	 qx=i*Ngrid/Npar;
	 kxq=kw*qx;
	 xexact=qx+Dg(Across)*ampx*sin(kxq);
	 pexact=Squ(Across-0.5*Astep)*dDg(Across-0.5*Astep)*ampx*sin(kxq);
	 fprintf(out,"%f %f %f %f %e\n",qx,x[ip],px[ip],xexact,pexact);
       }
       fclose(out);

}

/**  Output the results of the cosmological simulations **/
void output_cosmo(x,y,z,px,py,pz)
     float *x,*y,*z,*px,*py,*pz;
{       
       int i,j,k,ip;
       float ***rho,***f3tensor(),**matrix(),**rhoz;
       void density(),free_matrix(),free_f3tensor();
       FILE *out;

       /* Define a tensor for density */
       rho=f3tensor(1,Ngrid,1,Ngrid,1,Ngrid);
       rhoz=matrix(1,Ngrid,1,Ngrid);
       /* Assign density @ grid points using the CIC interpolation scheme */
       density(rho,x,y,z);
       /* Writing outputs to file */
       out=fopen("cosmo.dat","wb");
       fprintf(out,"%d %6.4f %6.4f \n",Ngrid,Aexpn,Astep);
       fprintf(out,"%5.3f %5.3f %5.3f\n",Om,Ok,Ol);
       for (i=1;i<=Ngrid;i++) for (j=1;j<=Ngrid;j++) {
	 for(k=1;k<=Ngrid;k++) rhoz[i][j]+=rho[i][j][k];
	 fprintf(out,"%f ",rhoz[i][j]);
       }
       fclose(out);
       free_matrix(rhoz,1,Ngrid,1,Ngrid);
       free_f3tensor(rho,1,Ngrid,1,Ngrid,1,Ngrid);
}

/** Testing the power spectrum routine by simulating the white noise **/
/** power spectrum by randomly placing Np particles in the simulation box **/ 
void create_white_noise(x,y,z)
        float *x,*y,*z;
{
        int i,j,k,ip;
	float xn,yn,ran1();
	time_t seed; 
        long iseed;
  
	/* Seeds for the random number generator */
	seed=-1.0*(time(NULL)%100000);
        iseed=(long)seed;

	/* For the periodic boundary conditions */
	xn=(float)Ngrid+1.0+1.0e-20;
	yn=(float)Ngrid;
	/* Distribute Np particles randomly in the simulations box */
	for (i=1;i<=Npar;i++) for (j=1;j<=Npar;j++) for (k=1;k<=Npar;k++) {  
	  ip=Squ(Npar)*(i-1)+Npar*(j-1)+k;
	  /* Coordinates of the nth particle */
	  x[ip]=Ngrid*ran1(&iseed);
	  y[ip]=Ngrid*ran1(&iseed);
	  z[ip]=Ngrid*ran1(&iseed);
	  /* Periodical conditions */
	  if(x[ip]<1.0) x[ip]+=yn;
	  if(x[ip]>=xn) x[ip]-=yn;
	  if(y[ip]<1.0) y[ip]+=yn;
	  if(y[ip]>=xn) y[ip]-=yn;
	  if(z[ip]<1.0) z[ip]+=yn;
	  if(z[ip]>=xn) z[ip]-=yn;
	}
}

/* Calculating power spectrum of the density field */
void power(x,y,z)
        float *x,*y,*z;
{
        int l,m,n,le,me,ne,nbin,npow,*nmode,*ivector();
	float delta,fmin,fmax;
	float kw,fbox,Dg(),kxq,kyq,kzq,ktot,dk,ak,bk,factor,*kmode,*pow,*vector();
	float ***rho,***f3tensor(),**rho_spec,**matrix();
	void rlft3(),free_ivector(),free_vector(),free_matrix(),free_f3tensor();
	FILE *out;
     
	/* Define arrays for density field */
	rho=f3tensor(1,Ngrid,1,Ngrid,1,Ngrid);
	rho_spec=matrix(1,Ngrid,1,2*Ngrid);
     
	/* Assign density to the grid points */
	density(rho,x,y,z);    
	delta=0.0;
	fmin=1.0e+30;
	fmax=-1.0e+30;
	for (l=1;l<=Ngrid;l++) for(m=1;m<=Ngrid;m++) for(n=1;n<=Ngrid;n++) {
	  delta+=Squ(rho[l][m][n])/N3grid;
	  if (fmin>rho[l][m][n]) fmin=rho[l][m][n];
	  if (fmax<rho[l][m][n]) fmax=rho[l][m][n];   
	}
	delta=sqrt(delta);
	printf("RMS : delta %e fmin %e fmax %e \n",delta,fmin,fmax);
	
	/* Take FFT of the density field */
	rlft3(rho,rho_spec,Ngrid,Ngrid,Ngrid,1);
     
	/* Calculating the power */
	npow=50;
	pow=vector(1,npow);
	kmode=vector(1,npow);
	nmode=ivector(1,npow);
	/* Initialize the arrays */
	for (l=1;l<=npow;l++) { kmode[l]=0.0; pow[l]=0.0; nmode[l]=0; }
	/* bin width in k-space */
	dk=sqrt(3)*PI/npow;
	/* Define useful constants */
	kw=2.0*PI/Ngrid;
	fbox=Lbox/Ngrid;
	/* Volume factor in definition of delta(k) or P(k)=|delta(k)|^2  */
	factor=Squ(1.0/N3grid); 
     
	/* Computer the Power Spectrum : P(k) [h^-3 Mpc^3] */
	/* CAUTION : symmetry in output of rlft3() routine (working now!) */
	for (l=1;l<=Ngrid;l++) for(m=1;m<=Ngrid;m++) for(n=1;n<=(int)Ngrid/2+1;n++) {
	  le=l-1;  if (le>Ngrid/2) le=le-Ngrid;  kxq=kw*le;
	  me=m-1;  if (me>Ngrid/2) me=me-Ngrid;  kyq=kw*me;
	  ne=n-1;  if (ne>Ngrid/2) ne=ne-Ngrid;  kzq=kw*ne;
	  ktot=sqrt(Squ(kxq)+Squ(kyq)+Squ(kzq));
	  nbin=ktot/dk;
	  /* Take care of 000 component */
	  if (n==1) {
	    ak=rho[l][m][2*n-1];    
	    bk=rho[l][m][2*n];
	    kmode[nbin]+=ktot;
	    pow[nbin]+=(Squ(ak)+Squ(bk));
	    nmode[nbin]+=1;
	  }
	  else if (n<=(int)Ngrid/2) {
	    ak=rho[l][m][2*n-1];
	    bk=rho[l][m][2*n];
	    kmode[nbin]+=2.0*ktot;
	    pow[nbin]+=2.0*(Squ(ak)+Squ(bk));
	    nmode[nbin]+=2;
	  }
	  /* Take care of z-axis Nyquist critical frequency components */ 
	  else if (n==(int)Ngrid/2+1) {
	    ak=rho_spec[l][2*m-1];    
	    bk=rho_spec[l][2*m];
	    kmode[nbin]+=ktot;
	    pow[nbin]+=(Squ(ak)+Squ(bk));
	    nmode[nbin]+=1;
	  }
	}

	/* Output the power spectrum to a file */
	out=fopen(Outfile_power,"wb");
	for (l=1;l<=npow;l++) {
	  kmode[l]=kmode[l]/nmode[l]/fbox;
	  /* P(k) at z=zi */
	  pow[l]=pow[l]/nmode[l]*Cube(Lbox)*factor;
	  /* Scale P(k) at z=zi to z=0.0 */
	  /* pow[l]/=Squ(Dg(Aexp0)); */ 
	  /*printf("%e %e %d\n",kmode[l],pow[l],nmode[l]);*/
	  fprintf(out,"%e %e %d\n",kmode[l],pow[l],nmode[l]);
	}
	fclose(out);
	
	free_vector(pow,1,npow);
	free_vector(kmode,1,npow);
	free_ivector(nmode,1,npow);
	free_matrix(rho_spec,1,Ngrid,1,Ngrid);
	free_f3tensor(rho,1,Ngrid,1,Ngrid,1,Ngrid);
}
