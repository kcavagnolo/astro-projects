From bonifaci@oat.ts.astro.it Tue May  7 06:22:16 1996
Return-Path: <bonifaci@oat.ts.astro.it>
Received: from nexus.astro.psu.edu by juakali.astro.psu.edu (4.1/Client-1.3)
	id AA19935; Tue, 7 May 96 06:22:15 EDT
Received: from freya.oat.ts.astro.it by nexus.astro.psu.edu (4.1/Nexus-1.3)
	id AA28204; Tue, 7 May 96 06:17:12 EDT
Message-Id: <9605071017.AA28204@nexus.astro.psu.edu>
Received: by freya.oat.ts.astro.it
	(1.38.193.4/16.2) id AA08781; Tue, 7 May 1996 11:01:16 +0200
From: bonifaci@oat.ts.astro.it (Piercarlo Bonifacio)
Subject: I found a bug in the way I wrote the data to the table
To: mab@astro.astro.psu.edu (Matthew A. Bershady)
Date: Tue, 7 May 96 11:01:15 METDST
In-Reply-To: <9605031202.AA15665@juakali.astro.psu.edu>; from "Matthew A. Bershady" at May 3, 96 8:02 am
Mailer: Elm [revision: 70.85]
Status: RO

c     bces_regress.f   -  Bi-variate Correlated Errors and Scatter
c
c     Calculate linear regression model when both variables
c     are subject to known errors, and there is intrinsic scatter.
c     Errors may be correlated or UNcorrelated.
c
c     BCES = bivariate, correlate errors and scatter
c
c     Authors:
c     Michael Akritas (Penn State) statistical analysis
c     Tina Bird (Kansas) code written 
c     Matthew A. Bershady (Penn State) code expanded/modified
c     
c     Modification History:
c     Feb 2  1995   TB      Initial Release    
c     Mar 26 1995   MAB     reformat; add orthogonal regression; add variances
c                           for a3 and a4; generalize for off-diagonal
c                           (covariant) errors (but no inputs yet);
c                           all real-> double precision; add bootstrap errors;
c                           add input of covariant errors - required.
c     
c
c     May   1996  Modified to accept input from a MIDAS table
c                 and write output to descriptors of the table
c                 by Piercarlo Bonifacio(Osservatorio Astronomico di Trieste)
c
      program  bcestbl
c
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
      double precision dum(4),f,xx
      real p
      character infile*50,model(4)*15,boot(4)*15
      integer ibuf(10),kx,ky,kex,key,kexy
      integer status,tid,im,io(4)
      integer iav,kunit,knull,acts,i
      character*15 descr
      character*15 xcol,ycol,excol,eycol,exycol,lab
      character*16 formr4,unit,label
      logical loop,sel,l1,l2,l3,l4
      common /vmr/madrid

        INCLUDE 'MID_INCLUDE:ST_DEF.INC'
        INCLUDE 'MID_INCLUDE:ST_DAT.INC'

      data formr4/'f6.2'/
      data unit /'dex'/
      data label /' '/
      data model / 'BCESYX', 'BCESXY',
     &      'BCES_Bisector', 'BCES_Orthogonal' /
      data boot /'bootyx','bootxy','bootbis','bootorth'/
        
        CALL STSPRO('bcestbl')

c
c INPUT section ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c

c get table name and column names from keywords
c
        call stkrdc('intable',1,1,50,iav,infile,kunit,knull,status)
        call stkrdc('xcol',1,1,15,iav,xcol,kunit,knull,status)
        call stkrdc('ycol',1,1,15,iav,ycol,kunit,knull,status)
        call stkrdc('excol',1,1,15,iav,excol,kunit,knull,status)
        call stkrdc('eycol',1,1,15,iav,eycol,kunit,knull,status)
        call stkrdc('exycol',1,1,15,iav,exycol,kunit,knull,status)
c
c    get nsim and iseed from keywords
c
        call stkrdi('nsim',1,1,im,nsim,kunit,knull,status)
        call stkrdi('iseed',1,1,im,iseed,kunit,knull,status)

        call tbtopn(infile,F_IO_MODE ,tid,status)
c
c read the standard descriptor tblcontr to get the number of rows, columns etc.
c
        descr='tblcontr'
        call stdrdi(tid,descr,1,10,acts,ibuf,kunit,knull,status) 
c
c find the numbers of the necessary columns
c
        l1=.false.
        l2=.false.
        l3=.false.
        l4=.false.
        do i=1,ibuf(3)
         call tblget(tid,i,lab,status)
         if(lab.eq.xcol)kx=i
         if(lab.eq.ycol)ky=i
         if(lab.eq.excol)kex=i
         if(lab.eq.eycol)key=i
         if(lab.eq.exycol)kexy=i
         if(lab.eq.model(1))l1=.true.
         if(lab.eq.model(1))io(1)=i
         if(lab.eq.model(2))l2=.true.
         if(lab.eq.model(2))io(2)=i
         if(lab.eq.model(3))l3=.true.
         if(lab.eq.model(3))io(3)=i
         if(lab.eq.model(4))l4=.true.
         if(lab.eq.model(4))io(4)=i
        enddo
        sel=.true.
        npts=0
c    read the  vectors in input
c
        do i=1,ibuf(4)
c
c read in only rows which are selected
c
           call tbsget(tid,i,sel,status)
           if(sel)then
            npts=npts+1
            call tberdr(tid,i,kx,p,loop,status) 
            y1(npts)=p
            call tberdr(tid,i,ky,p,loop,status) 
            y2(npts)=p
            call tberdr(tid,i,kex,p,loop,status) 
            y1err(npts)=p
            call tberdr(tid,i,key,p,loop,status) 
            y2err(npts)=p
            call tberdr(tid,i,kexy,p,loop,status) 
            cerr(npts)=p
           endif
        enddo
c
c end of INPUT section ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c

      write(6,*) 'Finished reading table.  NPTS =',npts

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
c
c initialize columns for fit if they do not already exist
c
        if(.not.l1)then
        io(1)=ibuf(3)+1
        call tbcini(tid,D_R8_FORMAT,1,formr4,unit,model(1),io(1),status)
        endif
        descr='tblcontr'
        call stdrdi(tid,descr,1,10,acts,ibuf,kunit,knull,status) 
        if(.not.l2)then
        io(2)=ibuf(3)+1
        call tbcini(tid,D_R8_FORMAT,1,formr4,unit,model(2),io(2),status)
        endif
        call stdrdi(tid,descr,1,10,acts,ibuf,kunit,knull,status) 
        if(.not.l3)then
        io(3)=ibuf(3)+1
        call tbcini(tid,D_R8_FORMAT,1,formr4,unit,model(3),io(3),status)
        endif
        call stdrdi(tid,descr,1,10,acts,ibuf,kunit,knull,status) 
        if(.not.l4)then
        io(4)=ibuf(3)+1
        call tbcini(tid,D_R8_FORMAT,1,formr4,unit,model(4),io(4),status)
        endif
        call stdrdi(tid,descr,1,10,acts,ibuf,kunit,knull,status) 
c
c
c
      do 50 i=1,4
         write(*,100) model(i),b(i),sqrt(bvar(i)),a(i),sqrt(avar(i))
         write(*,100) 'bootstrap',bavg(i),sdb(i),aavg(i),sda(i)
          dum(1)=b(i)
          dum(2)=sqrt(bvar(i))
          dum(3)=a(i)
          dum(4)=sqrt(avar(i))
c
c
c  write coefficients in a double precision descriptor
c
          call stdwrd(tid,model(i),dum,1,4,kunit,status)
c
          dum(1)=bavg(i)
          dum(2)=sdb(i)
          dum(3)=aavg(i)
          dum(4)=sda(i)
          call stdwrd(tid,boot(i),dum,1,4,kunit,status)
c
c write fitted line in a column
c

          do j=1,ibuf(4)
           call tberdr(tid,j,kx,p,loop,status)
           xx=p
           f=b(i)*xx+a(i)
           call tbewrd(tid,j,io(i),f,status)
          enddo
         write(*,*) ' '
 50   continue
      write(6,*) ' '
      write(6,*) 'Comparison of variances for B:'
      write(6,*) ' '
      write(6,99) 'fit','err(B)','err(B) IFAB'
      write(6,100) 'OLS BISECTOR',sqrt(bvar(3)),
     &     sqrt(bvar_ifab(3))
      write(6,100) 'Orthogonal',sqrt(bvar(4)),
     &     sqrt(bvar_ifab(4))
 99   format (5a12)
 100  format (a15,4e12.3)
c     
      call tbtclo(tid,status)
      call STSEPI
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
         sig12var = sig12var + cerr(i)**2
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
      xdisp = 0.0
      
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
c ------------------------------------------------------------------------------
      subroutine bootspbec (nmax,n,iseed,x,xerr,y,yerr,cerr,
     &     xboot,xerrboot,yboot,yerrboot,cerrboot)
c ------------------------------------------------------------------------------
c     Constructs Monte Carlo simulated data set using the
c     Bootstrap algorithm.                               
c                                                        
c     Random number generator is real function ran3.f, from
c     Numerical Recipes (an adaptation of Knuth's algorithm).
c
      implicit double precision (x,y,c)
      dimension x(nmax),xerr(nmax),y(nmax),yerr(nmax),cerr(nmax),
     &     xboot(nmax),xerrboot(nmax),yboot(nmax),yerrboot(nmax),
     &     cerrboot(nmax)
      integer i,j,n,nmax
      real*4 ran3
      external ran3
c     
      do 10 i = 1,n
         j = int(ran3(iseed)*float(n) + 1.0)
         xboot(i) = x(j)
         xerrboot(i) = xerr(j)
         yboot(i) = y(j)
         yerrboot(i) = yerr(j)
         cerrboot(i) = cerr(j)
 10   continue
c     
      return
      end
      
      FUNCTION RAN3(IDUM)
C         IMPLICIT REAL*4(M)
C         PARAMETER (MBIG=4000000.,MSEED=1618033.,MZ=0.,FAC=2.5E-7)
      PARAMETER (MBIG=1000000000,MSEED=161803398,MZ=0,FAC=1.E-9)
      DIMENSION MA(55)
c
      save
c
      DATA IFF /0/
      IF(IDUM.LT.0.OR.IFF.EQ.0)THEN
        IFF=1
        MJ=MSEED-IABS(IDUM)
        MJ=MOD(MJ,MBIG)
        MA(55)=MJ
        MK=1
        DO 11 I=1,54
          II=MOD(21*I,55)
          MA(II)=MK
          MK=MJ-MK
          IF(MK.LT.MZ)MK=MK+MBIG
          MJ=MA(II)
11      CONTINUE
        DO 13 K=1,4
          DO 12 I=1,55
            MA(I)=MA(I)-MA(1+MOD(I+30,55))
            IF(MA(I).LT.MZ)MA(I)=MA(I)+MBIG
12        CONTINUE
13      CONTINUE
        INEXT=0
        INEXTP=31
        IDUM=1
      ENDIF
      INEXT=INEXT+1
      IF(INEXT.EQ.56)INEXT=1
      INEXTP=INEXTP+1
      IF(INEXTP.EQ.56)INEXTP=1
      MJ=MA(INEXT)-MA(INEXTP)
      IF(MJ.LT.MZ)MJ=MJ+MBIG
      MA(INEXT)=MJ
      RAN3=MJ*FAC
      RETURN
      END
 

