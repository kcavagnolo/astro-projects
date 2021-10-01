c     wls_regress.f
c
c     Calculate linear regression assuming model where independent
c     variable is subject to insignificant error, and there is intrinsic 
c     scatter (homoscedastic) and errors in the dependent variable.
c     Both analytic and bootstrap uncertainties are given.
c
c     WLSS = wighted least squares with scatter
c
c     This program uses subroutines sixlin.f (for OLS) and bootsp.f (for
c     selecting bootstrap samples written by Isobe, Babu, and Feigelson.
c
c     Authors:
c     Michael Akritas (Penn State) statistical analysis
c     Matthew A. Bershady (Penn State) coded
c     
c     Modification History:
c     Apr 2  1995   MAB     Initial Release
c     Jan 25 1996   MAB     Swap implicit and parameter lines for successful
c                           compilation with Decstation 5000 (Ultrix 4.3);
c                           error reported by John Blakeslee (MIT).
c     Apr 28 1998   MAB     Add information on yerr.
c     
      implicit double precision (x,y,r)
      parameter (nmax=1000)
      dimension x1(nmax),y2(nmax),y2err(nmax),
     &     x1sim(nmax),y2sim(nmax),y2errsim(nmax),res(nmax)
      integer nmax,npts,nsim,iseed
      double precision a,b,avar,bvar,evar
      double precision asim,bsim,avarsim,bvarsim,evarsim
      double precision asum,assum,bsum,bssum,esum,essum
      double precision sda,sdb,sde,sdtest,rnsim
      double precision aavg,bavg,eavg
      character infile*50,outfile*50
c
c     inputs
c
      write(6,*) 'Name of input data file [x,y,yerr]: '
      write(6,*) ' NB: yerr is the standard deviation of'
      write(6,*) '     the measurement error in y'
      read(5,*) infile
      write(6,*) 'Name of output file:'
      read(5,*) outfile
      write(6,*) 'Number of bootstrap simulations: '
      read(5,*) nsim
      write(6,*) 'Integer seed for random number generator: '
      read(5,*) iseed
      open(unit=7,file=infile,status='old')
      rewind(7)
      open(unit=8,file=outfile,status='new')
      rewind(8)
c     
c     read in data and confidence intervals (assumed 1-sigma errors,
c     i.e. standard deviation)
c     
      npts = 0
      do 10 i=1,1000
         read(7,*,end=10) x1(i),y2(i),y2err(i)
         npts = npts +1
 10   continue
      write(6,*) 'Finished reading file.  NPTS =',npts
c
c     Get nominal values for a,b, and their variances via wls
c
      call wlss ( nmax,npts,x1,y2,y2err, a,avar,b,bvar,evar,res )
c
c     Make Bootstrap simulated datasets, and compute averages
C     and standard deviations of regression coefficients
C
      rnsim = dfloat(nsim)
      asum = 0.0
      assum = 0.0
      bsum = 0.0
      bssum = 0.0
      esum = 0.0
      essum = 0.0
      sda = 0.0
      sdb = 0.0
      sde = 0.0
      do 30  i=1,nsim
         if(i.eq.1) iseed = -1*iabs(iseed)
         call bootspe(nmax,npts,x1,y2,y2err,
     &        x1sim,y2sim,y2errsim,iseed)
         call wlss (nmax,npts,x1sim,y2sim,y2errsim,
     &        asim,avarsim,bsim,bvarsim,evarsim,res )
         asum = asum + asim
         assum = assum + asim**2
         bsum = bsum + bsim
         bssum = bssum + bsim**2
         if ( evarsim.gt.0.0 ) esum = esum + sqrt(evarsim)
         essum = essum + evarsim
 30   Continue
      aavg = asum/rnsim
      sdtest = assum - rnsim*aavg**2
      if(sdtest.gt.0.0) sda = dsqrt(sdtest/(rnsim-1.0))
      bavg = bsum/rnsim
      sdtest = bssum - rnsim*bavg**2
      if(sdtest.gt.0.0) sdb = dsqrt(sdtest/(rnsim-1.0))
      eavg = esum/rnsim
      sdtest = essum - rnsim*eavg**2
      if(sdtest.gt.0.0) sde = dsqrt(sdtest/(rnsim-1.0))
c      
c     write out the data
c      
      write(8,99) 'fit','B','err(B)','A','err(A)','s','err(s)'
      write(8,100) 'WLSS(Y|X)',b,sqrt(bvar),a,sqrt(avar),sqrt(evar)
      write(8,100) 'bootstrap:',bavg,sdb,aavg,sda,eavg,sde
 99   format (7a12)
 100  format (a12,6e12.3)
c     
      stop
      end
c -----------------------------------------------------------------------------
      subroutine wlss ( nmax,npts,x1,y2,y2err, a,avar,b,bvar,evar,res )
c -----------------------------------------------------------------------------
c     Core subroutine: Calculate weighted least-squares for model
c     with intrinsic scatter.
c
      integer nmax,npts
      implicit double precision (x,y,r)
      dimension x1(nmax),y2(nmax),y2err(nmax),res(nmax)
      double precision a,avar,b,bvar
      double precision avec(6),bvec(6),sigavec(6),sigbvec(6)
      double precision resav,resvar,sig22var,evar,estarvar
      double precision wsum,wsumx1,wsumx1y2,wsumy2,wsumx1x1,denom
c      
c     Step 1: call sixlin to get linear regression coefficients from OLS.
c     (NB: Only need j=1 case: OLS(Y|X))
c
      call sixlin(nmax,npts,x1,y2,avec,sigavec,bvec,sigbvec)
c
c     Step 2: Calculate residuals and average residual
c     
      resav = 0.
      do 20 i=1,npts
         res(i) = y2(i) - avec(1) - bvec(1)*x1(i)
         resav = resav + res(i)
 20   continue
      resav = resav/dfloat(npts)
c
c     Step 3: Obtain estimate of variance of the intrinsic scatter
c
      resvar = 0.
      sig22var = 0.
      do 30 i=1,npts
         resvar = resvar + (res(i)-resav)**2
         sig22var = sig22var + y2err(i)**2
 30   continue
      sig22var = sig22var/dfloat(npts)
      resvar = resvar/dfloat(npts)
      evar = resvar - sig22var
c     
c     Prepare for b and a calculation
c     
      do 40 i=1,npts
         estarvar = evar + y2err(i)**2
         wsum = wsum + 1./estarvar
         wsumx1 = wsumx1 + x1(i)/estarvar
         wsumx1y2 = wsumx1y2 + x1(i)*y2(i)/estarvar
         wsumy2 = wsumy2 + y2(i)/estarvar
         wsumx1x1 = wsumx1x1 + x1(i)*x1(i)/estarvar
 40   continue
c
c     Calcalculate b and a estimates and estimated variances
c
      denom =  wsum * wsumx1x1 - wsumx1**2
      b = ( wsum * wsumx1y2 - wsumx1 * wsumy2 ) / denom
      a = ( wsumx1x1 * wsumy2 - wsumx1 * wsumx1y2 ) / denom
      bvar = wsum / denom
      avar = wsumx1x1 / denom
c
      return
      end
