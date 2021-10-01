c     bces_regress.f   -  Bi-variate Correlated Errors and Scatter
c
c     Calculate linear regression model when both variables
c     are subject to known errors, and there is intrinsic scatter.
c     Errors may be correlated or UNcorrelated, and may be
c     homoscedastic or heteroscedastic, i.e. the same or different
c     for each datum.
c
c     BCES = bivariate, correlated errors and scatter
c
c     Data input: 5 column, unformatted
c         y1 ey1 y2 ey2 ey1y2
c
c     y1 =  measurement of the first observable
c     ey1 = measurement error of first observable (standard deviation)
c     y2 = measurement of second observable
c     ey2 = measurement of second observable (standard deviation)
c     ey1y2 = covariance of measurement errors of first and second observables
c
c     NB: 
c         (1) variance = (standard deviation)^2
c         (2) for uncorrelated errors, set ey1y2 = 0
c
c     Authors:
c     Authors:
c     Michael Akritas (Penn State)     - statistical analysis
c     Tina Bird (Kansas)               - BCES(X1|X2) algorithm coded
c     Matthew A. Bershady (Penn State) - code modified/expanded/generalized;
c                                        bootstrap added
c     
c     Modification History:
c     Feb 2  1995   TB      Initial Release    
c     Mar 26 1995   MAB     reformat; add orthogonal regression; add variances
c                           for a3 and a4; generalize for off-diagonal
c                           (covariant) errors (but no inputs yet);
c                           all real-> double precision; add bootstrap errors;
c                           add input of covariant errors - required.
c     Apr 28 1998   MAB     add comment on input definitions; correct two
c                           errors: (1) cerr treatment as covariance and not
c                           'standard deviation,' (2) initialization typo
c                           xdisp = 0. -> xstddev = 0 in subroutine class.
c     
      parameter (nmax=1000,nmod=4)
      implicit double precision (x,y,z,c)
      dimension y1(nmax),y1err(nmax),y2(nmax),y2err(nmax),cerr(nmax)
      dimension zeta(nmax,nmod),xi(nmax,nmod)
      dimension y1sim(nmax),y1errsim(nmax),y2sim(nmax)
      dimension y2errsim(nmax),cerrsim(nmax)
      integer npts,nmax,nsim,iseed
      double precision a(4),b(4),avar(4),bvar(4),bvar_ifab(4)
      double precision asim(4),bsim(4),avarsim(4),bvarsim(4)
      double precision asum(4),bsum(4),aavg(4),bavg(4)
      double precision assum(4),bssum(4),sda(4),sdb(4)
      double precision bvar_ifabsim(4),sdtest,rnsim
      character infile*50,outfile*50,model(4)*15
c
      data model / '      BCES(Y|X)', '      BCES(X|Y)',
     &      '  BCES Bisector', 'BCES Orthogonal' /
c
c     inputs
c
      write(6,*) 'Name of input file [y1 ey1 ey2 ey2 ey1y2]:'
      write(6,*) ' NB: (a) ey1 and ey2 are standard deviations of'
      write(6,*) '     the measurement errors in y1 and y2, respectively'
      write(6,*) '     (b) ey1y2 is the covariance of the measurement'
      write(6,*) '     errors in y1 and y2'
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
c     read in data and confidence intervals
c     
      npts = 0
      do 10 i=1,1000
         read(7,*,end=10) y1(i),y1err(i),y2(i),y2err(i),cerr(i)
         npts = npts +1
 10   continue
      write(6,*) 'Finished reading file.  NPTS =',npts
c
c     Calculate nominal fits
c
      call bess ( nmax,nmod,npts,y1,y1err,y2,y2err,cerr,
     &     a,b,avar,bvar,bvar_ifab,xi,zeta )
c
c     Make Bootstrap simulated datasets, and compute averages
C     and standard deviations of regression coefficients
C
      rnsim = dfloat(nsim)
      do 20 i=1,4
         asum(i) = 0.0
         assum(i) = 0.0
         bsum(i) = 0.0
         bssum(i) = 0.0
         sda(i) = 0.0
         sdb(i) = 0.0
 20   continue
      do 30  i=1,nsim
         if(i.eq.1) iseed = -1*iabs(iseed)
         call bootspbec(nmax,npts,iseed,y1,y1err,y2,y2err,cerr,
     &        y1sim,y1errsim,y2sim,y2errsim,cerrsim)
         call bess ( nmax,nmod,npts,y1sim,y1errsim,y2sim,
     &        y2errsim,cerrsim,asim,bsim,avarsim,bvarsim,
     &        bvar_ifabsim,xi,zeta )
         do 35 j=1,4
            asum(j) = asum(j) + asim(j)
            assum(j) = assum(j) + asim(j)**2
            bsum(j) = bsum(j) + bsim(j)
            bssum(j) = bssum(j) + bsim(j)**2
 35      continue
 30   continue
      do 40 i=1,4
         aavg(i) = asum(i)/rnsim
         sdtest = assum(i) - rnsim*aavg(i)**2
         if(sdtest.gt.0.0) sda(i) = dsqrt(sdtest/(rnsim-1.0))
         bavg(i) = bsum(i)/rnsim
         sdtest = bssum(i) - rnsim*bavg(i)**2
         if(sdtest.gt.0.0) sdb(i) = dsqrt(sdtest/(rnsim-1.0))
 40   continue
c      
c     write out the data
c      
      write(*,99) 'fit','B','err(B)','A','err(A)'
      write(8,99) 'fit','B','err(B)','A','err(A)'
      do 50 i=1,4
         write(*,100) model(i),b(i),sqrt(bvar(i)),a(i),sqrt(avar(i))
         write(*,100) 'bootstrap',bavg(i),sdb(i),aavg(i),sda(i)
         write(*,*) ' '
         write(8,100) model(i),b(i),sqrt(bvar(i)),a(i),sqrt(avar(i))
         write(8,100) 'bootstrap',bavg(i),sdb(i),aavg(i),sda(i)
         write(8,*) ' '
 50   continue
      write(8,*) ' '
      write(8,*) 'Comparison of variances for B:'
      write(8,*) ' '
      write(8,99) 'fit','err(B)','err(B) IFAB'
      write(8,100) 'OLS BISECTOR',sqrt(bvar(3)),
     &     sqrt(bvar_ifab(3))
      write(8,100) 'Orthogonal',sqrt(bvar(4)),
     &     sqrt(bvar_ifab(4))
 99   format (5a12)
 100  format (a15,4e12.3)
      close (7)
      close (8)
c     
      stop
      end
c     
c -----------------------------------------------------------------------
      subroutine bess ( nmax,nmod,npts,y1,y1err,y2,y2err,cerr,
     &     a,b,avar,bvar,bvar_ifab,xi,zeta )
c -----------------------------------------------------------------------
c     Do the entire regression calculation for 4 slopes:
c     OLS(Y|X), OLS(X|Y), bisector, orthogonal
c
      implicit double precision (c,x,y,z)
      dimension y1(nmax),y1err(nmax),y2(nmax),y2err(nmax)
      dimension cerr(nmax),xi(nmax,nmod),zeta(nmax,nmod)
      integer i,npts,nmax,nmod
      double precision a(4),b(4),avar(4),bvar(4),bvar_ifab(4)
      double precision y1av,y2av,y1var,y2var,y1stddev,y2stddev
      double precision sig11var,sig22var,sig12var,sign
      double precision zetaav(4),zetadisp(4),xidisp(4),xiav(4)
      double precision covar_y1y2,covb1b2
c     
c     calculate sigma's for datapoints using length of conf. intervals
c
      sig11var = 0.
      sig22var = 0.
      sig12var = 0.
      do 2 i=1,npts
         sig11var = sig11var + y1err(i)**2
         sig22var = sig22var + y2err(i)**2
c
c     old version; mistake in treating cerr like a standard deviation
c     instead of a covariance
c        sig12var = sig12var + cerr(i)**2 
c                
c     new verions: Apr 28 1998
c
         sig12var = sig12var + cerr(i)
 2    continue
      sig11var = sig11var/real(npts)
      sig22var = sig22var/real(npts)
      sig12var = sig12var/real(npts)
c     
c     calculate means and variances
c     
      call class(y1,npts,y1av,y1stddev)
      call class(y2,npts,y2av,y2stddev)
      y1var = y1stddev**2
      y2var = y2stddev**2
      covar_y1y2 = 0.
      do 5 i=1,npts
         covar_y1y2 = (y1(i)-y1av)*(y2(i)-y2av) + covar_y1y2
 5    continue
      covar_y1y2 = covar_y1y2/real(npts)
c      
c     compute the regression slopes for OLS(Y2|Y1), OLS(Y1|Y2),
c     bisector, and orthogonal.
c      
      b(1) = (covar_y1y2 - sig12var)/(y1var - sig11var)
      b(2) = (y2var - sig22var)/(covar_y1y2 - sig12var)
      b(3) = ( b(1)*b(2) - 1.0 
     &     + sqrt((1.0 + b(1)**2)*(1.0 + b(2)**2)) ) /
     &     (b(1)+b(2))
      if ( covar_y1y2.lt.0. ) then
         sign = -1.
      else
         sign = 1.
      endif
      b(4) = 0.5*((b(2)-(1./b(1))) 
     &     + sign * sqrt(4.+(b(2)-(1./b(1)))**2))
c      
c     compute intercepts for above 4 cases:
c      
      do 10 i=1,4
         a(i) = y2av - b(i)*y1av
 10   continue
c      
c     set up variables to calculate standard deviations of slope
c     and intercept (MAB renamed: chi -> xi, xi -> zeta to be consistent 
c     with text.)
c
      do 15 i=1,npts
         xi(i,1) = ( (y1(i)-y1av) * (y2(i)-b(1)*y1(i)-a(1)) + 
     &        b(1)*y1err(i)**2 ) / (y1var-sig11var)
         zeta(i,1) = y2(i) - b(1)*y1(i) - y1av*xi(i,1)
         xi(i,2) = ( (y2(i)-y2av) * (y2(i)-b(2)*y1(i)-a(2)) - 
     &        y2err(i)**2 ) / covar_y1y2
         zeta(i,2) = y2(i) - b(2)*y1(i) - y1av*xi(i,2)
         xi(i,3) = xi(i,1) * 
     &        (1.+b(2)**2)*b(3) / 
     &        ((b(1)+b(2))*sqrt((1.+b(1)**2)*(1.+b(2)**2))) + 
     &        xi(i,2) * 
     &        (1.+b(1)**2)*b(3) /
     &        ((b(1)+b(2))*sqrt((1.+b(1)**2)*(1.+b(2)**2)))
         zeta(i,3) = y2(i) - b(3)*y1(i) - y1av*xi(i,3)
         xi(i,4) = xi(i,1) * 
     &        b(4)/(b(1)**2*sqrt(4.+(b(2)-1./b(1))**2)) +
     &        xi(i,2)*b(4)/sqrt(4.+(b(2)-1./b(1))**2)
         zeta(i,4) = y2(i) - b(4)*y1(i) - y1av*xi(i,4)
 15   continue
c      
c     calculate variance for all a and b
c
      do 20 i=1,4
         call nclass(nmax,nmod,npts,i,xi,xiav,xidisp)
         call nclass(nmax,nmod,npts,i,zeta,zetaav,zetadisp)
         bvar(i) = xidisp(i)**2/real(npts)
         avar(i) = zetadisp(i)**2/real(npts)
 20   continue
c      
c     alternate slope variances for b3 and b4 via IFAB formulae;
c     requires calculating first covariance for b1,b2
c
      covb1b2 = xidisp(1)*xidisp(2)/real(npts)
      bvar_ifab(3) = ( b(3)**2 / 
     &     (((b(1)+b(2))**2)*(1.+b(1)**2)*(1.+b(2)**2)) ) *
     &     ( (1.+b(2)**2)**2*bvar(1) + (1.+b(1)**2)**2*bvar(2) + 
     &     2.*(1.+b(1)**2)*(1.+b(2)**2)*covb1b2 )
      bvar_ifab(4) = ( b(4)**2 / 
     &     (4.*b(1)**2 + (b(1)*b(2)-1.)**2 ) ) *
     &     ( bvar(1)/b(1)**2 + 2.*covb1b2 + b(1)**2*bvar(2) )
c
      return
      end
c -----------------------------------------------------------------------
      subroutine class (x,n,xav,xstddev)
c -----------------------------------------------------------------------
c     Calculate mean and standard deviation of the array X of N numbers.
c     
      implicit double precision (x)
      dimension x(n)
      integer i
      double precision xav,xstddev
c
      xav = 0.0
      xstddev = 0.0
      
      do 10 i=1,n
         xav = xav + x(i)
 10   continue
      xav = xav/dfloat(n)
c      
      do 20 i=1,n
         xstddev = (x(i) - xav)**2 + xstddev
 20   continue
      xstddev = sqrt(xstddev/dfloat(n))
c      
      return
      end
c -----------------------------------------------------------------------
      subroutine nclass (nmax,nmod,npts,i,x,xav,xstddev)
c -----------------------------------------------------------------------
c     Calculate mean and standard deviation of the array X of N numbers.
c     
      implicit double precision (x)
      dimension x(nmax,nmod),xav(nmod),xstddev(nmod)
      integer i,j,nmax,nmod,npts
c
      xav(i) = 0.0
      xstddev(i) = 0.0
c
      do 10 j=1,npts
         xav(i) = xav(i) + x(j,i)
 10   continue
      xav(i) = xav(i)/dfloat(npts)
c      
      do 20 j=1,npts
         xstddev(i) = (x(j,i) - xav(i))**2 + xstddev(i)
 20   continue
      xstddev(i) = sqrt(xstddev(i)/dfloat(npts))
c
      return
      end
