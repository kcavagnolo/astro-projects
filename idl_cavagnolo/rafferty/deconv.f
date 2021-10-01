cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c	deconv.f (12/27/94)
c
c	This routine deconvolves the X-ray surface brightness of an
c		extended source to give the emissivity and density
c		as a function of radius.  The emissivity and density
c		are assumed to be comstant within sphereical shells
c
c	This version also derived the erros in the emissivity and
c		density by propagation of errors.
c
c	Input:
c		file xsurf.dat, giving the radii and flux of annuli
c		dmpc = angular diamter distance (Mpc)
c		z = redshift
c		xlamb = x-ray emissivity coeffient, such that
c			the xray emissivity emiss (ergs/cm^3/sec)
c			in the band (0.1 -- 2.4 keV)*(1+z)
c			is emiss = xlamb*xne**2
c			where xne is the electron density
c	
c	format of xsurf.dat
c		each line has
c			rm = mean radius (arcsec)
c			r1 = inner radius (arcsec)
c			r2 = outer radius (arcsec)
c			flux = flux (ergs/cm^2/sec, 0.1 - 2.4) in annulus,
c				as observed at earth, corrected for
c				absorption
c			xsurf = surface brightness
c				(ergs/cm^2/sec/arcmin^2, 0.1 - 2.4)
c			xerror = error in xsurf
c			rml = log10 of rm
c			erp = positive error bar in rml
c			erm = negative error bar in rml
c			xsurfl = log10 of xsurf
c			exp = positve error bar in xsurfl
c			exm = minus error bar in xsurfl
c
c	Output:
c		file emiss.dat, eachline has
c		rm = mean radius (arcsec)
c		r1 = inner radius (arcsec)
c		r2 = outer radius (arcsec)
c		emiss = emissivity (ergs/cm^3/sec),
c                       in the band (0.1 -- 2.4 keV)*(1+z)
c		xne = electron density (cm^-3)
c		rml = log10 of rm
c		erp = positive error bar in rml
c		erm = negative error bar in rml
c		xnel = log10 of xne
c		enp = positive error bar in xnel
c		enm = minus error bar in xnel
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	implicit real*8 (a-h,o-z)
c
c	i,j,k,iplux,iminus = bin counting indices
c	nmax = maximum number of data bins
	integer nmax,nmaxp,i,j,k,iplus
	parameter(nmax=100,nmaxp=nmax+1)
c
c	input data from xsurf.dat
c
c	mean and inner radius of each bin (arcsec)
c	rml = log10 of rm
c	erp = positive error bar in rml
c	erm = negative error bar in rml
	real*8 rm(nmax),r(nmaxp),rml(nmax),erp(nmax),erm(nmax)
c
c	flux in annulus (ergs/cm^2/sec) as observed at earth,
c		corrected for absorption
c	efp = positve error bar in log10 flux
c	efm = minus error bar in log10 flux
	real*8 flux(nmax),efp(nmax),efm(nmax)
c
c	other input parameters, not used in code
c
c	r2 = outer radius (arcsec)
c	xsurf = surface brightness
c	xerror = error in xsurf
c	xsurfl = log10 of xsurf
	real*8 r2,xsurf,xerror,xsurfl
c
c	output properties
c
c	emiss(i) = emissivity in bin i (ergs/cm**3/sec)
c	xne(i) = electron density (cm**-3) in bin i
c	xnel(i) = log10 xne
c	enp = positive error bar in xnel
c	enm = minus error bar in xnel
	real*8 emiss(nmax),emissp(nmax),emissm(nmax),xne(nmax),xnel(nmax),enp(nmax),enm(nmax)
c
c	projection matrix, inverted
	real*8 a(nmax,nmax)
c
c	geometric or physical factors
c
c	fact = 3*(rad/arcsec)**3/mpc (in cm)
c		converts flux/dmpc (in ergs/cm^2/sec/mpc) into ergs/cm^3/sec
c		assuming the units of angle are arcsec
c
c	factor includes the redshift in flux to emissivity conversion
c	rad1,rd1,rd2,rd3,rd4,y = factors from volume of shell
c	
	real*8 fact,factor,rad1,rd1,rd2,rd3,rd4,y
	parameter(fact=8.532121d-9)
c
c	cluster properties
c	dmpc = angular diameter distance (mpc)
c	z = redshift
c	xlamb = x-ray emissivity coefficient (ergs*cm^3/sec)
c		s.t. xlamb*ne**2 is emissivity in same emitted
c		band as flux
	real*8 dmpc,z,xlamb
c
c	give the distance, in Mpc
c
	write(6,*) 'Angular Diameter Distance (Mpc)?'
	read (5,*,end=2000) dmpc
c
c	read the redshift
c
	write(6,*) 'Redshift?'
	read (5,*,end=2000) z
c
c	read the X-ray emissivity factor
c
	write(6,*) 'Xlamb (ergs*cm^3/sec)?'
	read (5,*,end=2000) xlamb
c
c	this factor includes the redshift in flux to emissivity conversion
c
	factor=fact*(1.0+z)**4/dmpc
c
c	open input and output
c
	open(10,file='xsurf.dat',status='old')
	open(11,file='emiss.dat',status='new')
	i=1
c
c	read in radii in arcsec, flux in ergs/cm^2/sec
c
100	read(10,*,end=200) rm(i),r(i),r2,flux(i),xsurf,xerror,rml(i),
     1    erp(i),erm(i),xsurfl,efp(i),efm(i)
c
c	stop when flux goes negative
c
	if (flux(i).le.0.0) go to 200
	flux(i)=flux(i)*factor
	i=i+1
	r(i)=r2
	go to 100
200	ntot=i-1
c
c	initialize inverse matrix to zero (for safety)
	do 250 i=1,ntot
	do 250 j=1,ntot
250	a(i,j)=0.0d0
c
c	loop renormalizes flux and calculates inverse matrix
c
	do 500 i=ntot,1,-1
c
c	renormalize flux
c
	rad1=1.0d0/dsqrt(r(i+1)**2-r(i)**2)**3
	flux(i)=flux(i)*rad1
c
c	convert log errors to linear errors in renormalized flux
c
	eelp=efp(i)
	eelm=efm(i)
	efp(i)=flux(i)*(10.0d0**efp(i)-1.0d0)
	efm(i)=flux(i)*(1.0d0-10.0d0**(-efm(i)))
c
c	calculate inversion matrix for deconvolution
c
	a(i,i)=1.0d0
350	if (i.eq.ntot) go to 500
	iplus=i+1
	do 450 j=iplus,ntot
c
c	calculate y factor of projection matrix
c
	rd1=dsqrt(r(j+1)**2-r(i)**2)**3
	rd2=dsqrt(r(j+1)**2-r(i+1)**2)**3
	rd3=dsqrt(r(j)**2-r(i)**2)**3
	rd4=dsqrt(r(j)**2-r(i+1)**2)**3
	y=(rd1-rd2-rd3+rd4)*rad1
c
c	calculate inverse matrix -- this order saves storing the y's
c
	do 400 k=j,ntot
400	a(i,k)=a(i,k)-y*a(j,k)
450	continue
500	continue
c
c	now, do the deprojection and accummulate errors
c
	do 1000 i=ntot,1,-1
c	initialize to zero
	emiss(i)=0.0d0
	enp(i)=0.0d0
	enm(i)=0.0d0
c
c	do deprojection
c
	do 600 j=i,ntot
c
c	emissivity deprojected
c
	emiss(i)=emiss(i)+a(i,j)*flux(j)
c
c	accummulate errors quadratically since fluxes are independent
c
	enp(i)=enp(i)+a(i,j)**2*efp(j)**2
600	enm(i)=enm(i)+a(i,j)**2*efm(j)**2
c
c	skip points with negative emissivities
c
	if (emiss(i).lt.0.0d0) go to 1000
c
c	calculate electron density
c
	xne(i)=dsqrt(emiss(i)/xlamb)
c
c	calculate log10 electron density
c
	xnel(i)=dlog10(xne(i))
c
c	calculate plus and minus errors in emissivity
c
	enp(i)=dsqrt(enp(i))
	enm(i)=dsqrt(enm(i))
	emissp(i)=enp(i)
	emissm(i)=enm(i)
c
c	convert to log10 errors in electron density
c
	enp(i)=0.5d0*dlog10(1.0d0+enp(i)/emiss(i))
c
c	if minus error is large => electron density < 0
c		=> set to large value for plotting
c
	if (enm(i).lt.emiss(i)) then
	   enm(i)=-0.5d0*dlog10(1.0d0-enm(i)/emiss(i))
	else
	   enm(i)=50.0
	endif
1000	continue
c
c	write output for plotting
c
1200	do 1500 i=1,ntot
1500	write(11,1600) rm(i),r(i),r(i+1),emiss(i),emissp(i),emissm(i),xne(i),rml(i),erp(i),
     1     erm(i),xnel(i),enp(i),enm(i)
1600	format(13(1pe12.4))
2000	stop
	end
