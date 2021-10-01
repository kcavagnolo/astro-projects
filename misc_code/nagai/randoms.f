cxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
      integer function ipoidev8(xm)
c
c    Double precision version of ipoidev
c
      implicit none
      double precision xm,xmw
      integer ipoidev128
      integer ipoiss128,em
      
      integer idum
      data idum /-100/

      if (xm.lt.12.0d0)then
        em=ipoidev128(xm)
      else
        xmw=xm
        em=0
 3      continue
        if(xmw.gt.12.0d0)then
          em=em+ipoiss128(idum)
          xmw=xmw-12.0d0
          goto 3
        endif
        em=em+ipoidev128(xmw)
      endif
      ipoidev8=em
      return
      end
cxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
      integer function ipoiss128(idum)
c
c Double precision version of ipoiss12
c
      implicit none
      integer idum
      logical flag
      integer nmax
      parameter (nmax=150)
      double precision r(nmax),m
      parameter (m=12.0d0)
      integer nr,ifail,ranref8
      data flag /.true./
      
      save nr,r,flag

      if (flag) then
        nr=150
        ifail=0
        call poi_ref8 (m,r,nr,ifail)
        if (ifail.ne.0) then
          print*,'ifail=',ifail, '  in poi_ref'
          pause
        endif
        flag=.false.
      endif
      ipoiss128=ranref8(r,nr)
      return
      end
cxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
      subroutine poi_ref8(t,r,nr,ifail)
c
c     Sets up the reference vector for a poisson distribution.
c     add and times can be changed if a truncation probability of
c     1.0e-12 is regarded as unsatisfactory.
      double precision t
      integer ifail,nr
      double precision r(nr)
      double precision add,const1,const2,const3,one,times,twopi,w,x,y,z,zero
      integer i,ibase,ibot,idiff,ierr,itop,j,k
      data add /8.5d0/, times /7.15d0/, zero/0.0d0/, one /1.0d0/,
     ~    twopi /6.283185307179586d0/, const1/8.333333333333333d-2/,
     ~    const2 /2.777777777777778d-3/,const3 /7.936507936507937d-4/

      ierr = 1
      if (t.lt.zero) goto 140
      x=dsqrt(t)*times
      itop=idint(t+x+add)
      ibot=max(0,idint(t-x))
      ierr=2
      if ((itop-ibot+4).gt.nr) goto 140
      idiff=itop-nr
      ibase=ibot-idiff
      if (ibot.gt.0) goto 40

c     use the direct method if t .lt. 50.
      x=zero
      y=dexp(-t)
      z=zero
      do i=ibase,nr
         z=z+y
         r(i)=z
         x=x+one
         y=y*t/x
      enddo
      goto 120
c     use stirlings formula if t .gt. 50.
   40 i=idint(t)
      x=dfloat(i)
      z=one/(x*x)
      y=((t/x)**i)*dexp((x-t)-(const1-(const2-const3*z)*z)/x)/
     ~     dsqrt(twopi*x)
      j=i-idiff
      w=x
      z=y
      do k=ibase,j
         i=j+ibase-k
         r(i)=y
         y=y*x/t
         x=x-one
      enddo
      y=zero
      do i=ibase,j
         y=y+r(i)
         r(i)=y
      enddo
      j=j+1
      do i=j,nr
         w=w+one
         z=z*t/w
         y=y+z
         r(i)=y
      enddo
c     finish off in either case.
  120 call refindx8(idiff,ibase,r,nr)
      ifail=0
      return
  140 ifail=ierr
      return
      end
cxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
      integer function ipoidev128(xm)
c
c  Double precision version of ipoidev12
c
      implicit none
      integer iem
      double precision xm,pi
      parameter (pi=3.14159265358979d0)
      double precision g,oldm,t,ran28
      save g,oldm
      data oldm /-1.0d0/
      integer idum
      data idum /-100/

      if (xm.ne.oldm) then
        oldm=xm
        g=dexp(-xm)
      endif
      iem=-1
      t=1.0d0
 2    iem=iem+1
      t=t*ran28(idum)
      if (t.gt.g) go to 2
      ipoidev128=iem
      return
      end

cxxxxxxxxxxxxxxxxxxxxxxxxxxx      
      subroutine setran2(idum)
      implicit none
      integer idum,idum0
      double precision ran28,w
      idum0=idum
      if(idum0.gt.0)idum0=-idum0
      
      w=ran28(idum0)
      
      return
      end

cxxxxxxxxxxxxxxxxxxxxxxxxxxxx
      subroutine ran_setran2
      implicit none
      integer time,n
      character arg*80
      logical defined
      
      call get_command_line_par ('ranseed',1,arg)
      if (defined(arg)) then
        read (arg,*) n
      else
        n=time()
        n=n**10+n**9+n**8+n**7+n**6+n**5+n**4+n**3+n**2+n+n/2+n/3
        n=iabs(n)
      endif

      call setran2(n)
      return
      end

*xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

      function ran28(idum0)
c
c  first call should with negative idum0 !!!
c
c
c
c
      implicit none
      integer idum0,idum,IM1,IM2,IMM1,IA1,IA2,IQ1,IQ2,IR1,IR2,
     ~    NTAB,NDIV
      real*8 ran28,AM,EPS,RNMX,eps1,www
      real ran2,eps4,rnmx4,eps14,AM4
      parameter (IM1=2147483563, IM2=2147483399, AM=1.0d0/IM1,
     ~    IMM1=IM1-1,IA1=40014,IA2=40692, IQ1=53668, IQ2=52774,
     ~    IR1=12211, IR2=3791, NTAB=32, NDIV=1+IMM1/NTAB, AM4=AM)
      integer idum2,j,k,iv(NTAB),iy
      logical flag
      save iv,iy,idum2,flag,idum,EPS,RNMX,eps4,rnmx4
      
      data idum2 /123456789/, iy /0/, flag /.true./

      if(flag) then
        call ini_ran2 (iv,NTAB,iy,idum0,idum,idum2,rnmx4,RNMX,eps4,eps,
     ~      IM1,IR1,IQ1,IA1)
        flag=.false.
      endif
      
      k=idum/IQ1
      idum=IA1*(idum-k*IQ1)-k*IR1
      if(idum.lt.0) idum=idum+IM1
      k=idum2/IQ2
      idum2=IA2*(idum2-k*IQ2)-k*IR2
      if(idum2.lt.0) idum2=idum2+IM2
      j=1+iy/NDIV
      iy=iv(j)-idum2
      iv(j)=idum
      if(iy.lt.1)iy=iy+IMM1
      ran28=min(AM*iy,RNMX)
      ran28=max(ran28,EPS)
      return
      

cxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
      entry ran2(idum0)
      
      if(flag) then
        call ini_ran2 (iv,NTAB,iy,idum0,idum,idum2,rnmx4,RNMX,eps4,eps,
     ~      IM1,IR1,IQ1,IA1)
        flag=.false.
      endif
      k=idum/IQ1
      idum=IA1*(idum-k*IQ1)-k*IR1
      if(idum.lt.0) idum=idum+IM1
      k=idum2/IQ2
      idum2=IA2*(idum2-k*IQ2)-k*IR2
      if(idum2.lt.0) idum2=idum2+IM2
      j=1+iy/NDIV
      iy=iv(j)-idum2
      iv(j)=idum
      if(iy.lt.1)iy=iy+IMM1
      ran2=min(AM4*iy,rnmx4)
      ran2=max(ran2,eps4)
      return
      
      end
      
      
cxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
      subroutine ini_ran2 (iv,NTAB,iy,idum0,idum,idum2,rnmx4,RNMX,eps4,eps,
     ~    IM1,IR1,IQ1,IA1)
      implicit none
      integer NTAB,IM1,IR1,IQ1,IA1,iy,idum0,idum,idum2
      integer iv(NTAB)
      real rnmx4,eps4
      double precision RNMX,EPS
      integer j,k
      double precision EPS1
      real eps14

      do j=1,NTAB
        iv(j)=0
      enddo
      EPS=1.0d0
 10   EPS=0.5d0*EPS
      eps1=1.0d0+EPS
      if(eps1.gt.1.0d0) goto 10
      EPS=2.0d0*EPS
      RNMX=1.0d0-EPS
      eps4=1.0
 20   eps4=0.5*eps
      eps14=1.0+eps4
      if(eps14.gt.1.0) goto 20
      eps4=2.0*eps4
      rnmx4=1.0-eps4
      idum=max(-idum0,1)
      if(idum0.eq.0)idum=100
      idum2=idum
      do j=NTAB+8,1,-1
        k=idum/IQ1
        idum=IA1*(idum-k*IQ1)-k*IR1
        if(idum.lt.0) idum=idum+IM1
        if(j.le.NTAB) iv(j)=idum
      enddo
      iy=iv(1)
      
      return
      end


cxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
      function ranref8 (r,nr)
c
c
c  Generates an integer from a refference vector
c
c
      integer ranref8
      integer nr
      double precision r(nr)
      double precision x
      integer n
      double precision ran28
      integer idum

      data idum /0/

      x=ran28(idum)
      n=idint(x*r(1))
      n=idint(r(n+3))
      if (x.le.r(n)) goto 40
   20 n=n+1
      if (x.gt.r(n)) goto 20
 40   ranref8=n+idint(r(2))
      return
      end


cxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
      subroutine refvec8 (p, np, ip, lp, r, nr, ifail)
c-------------------------------------------------------------------------
c   a. purpose
c   ==========
c
c    refvec sets up the reference vector r for a discrete distribution
c    with   pdf    (probability  density   function)  or  cdf  (cumulative
c    distribution function) p  for further use by ranref
c
c   b. specification
c   ================
c
c          subroutine refvec (p,np,ip,lp,r,nr,ifail)
c          integer np,ip,nr,ifail
c          logical lp
c          //real// p(np),r(nr)
c
c   c. parameters
c   =============
c   p - //real// array of dimension (np).
c     before entry, p must contain the pdf or cdf of the distribution.
c
c   np - integer.
c     on entry, np must specify the dimension of p. np must be positive.
c
c   ip - integer.
c     on entry, ip must specify the value of the variate,  assumed to be a
c     whole number, to which the probability in p(1) corresponds.
c
c   lp - logical.
c     on entry, lp indicates the type of information contained in p. if lp
c     is .true., p contains a cumulative  distribution  function (cdf); if
c     lp is .false., p contains a probability density function (pdf).
c
c   r - //real// array of dimension (nr).
c     on exit, r contains  the  reference  vector r (see  section 3 of the
c     library manual routine document).
c
c   nr - integer.
c     on  entry, nr must  specify  the  dimension  of r. the  recommended
c     value  is  roughly  5 + 1.4*np. nr must be larger than np + 2.
c
c     unchanged on exit.
c
c   ifail - integer.
c     before entry, ifail must be assigned a value. for users not familiar
c     with this parameter the recommended value is 0.
c
c     unless  the  routine  detects  an error  (see next  section),  ifail
c     contains 0 on exit.
c
c
c   d. error indicators and warnings
c   ================================
c   ifail = 1
c     on entry, np.lt.1.
c
c   ifail = 2
c     on entry, nr.lt.np + 3.
c
c   ifail = 3
c     if lp  is .true.  on  entry,  then  the  values  in p are not all in
c     non-descending  order, as required by a cdf. if lp is .false.,  then
c     at  least  one of the  probabilities  in p is  negative,  or all the
c     probabilities are zero.
c
c   ifail = 4
c     the total  probability is not 1. in this case, r is set up correctly
c     since the error may be due to larger rounding  errors than expected.
c
c  e example of subroutine use
c  =========================
c        integer i, ifail, ix, nout
c        double precision p(10), r(19)
c        integer ranref
c        data nout /6/
c
c        data p(1), p(2), p(3), p(4), p(5), p(6), p(7), p(8), p(9),p(10) /
c       *0.0d0,0.1d0,0.2d0,0.4d0,0.5d0,0.6d0,0.8d0,0.9d0,1.0d0,1.0d0/
c        ifail = 0
c* Set a ref. vector
c        call refvec8(p, 10, 0, .true., r, 19, ifail)
c
c* Generate 5 integers from given distribution
c        do i=1,5
c           ix = ranref(r,19)
c           write (nout,*) ix
c        enddo
c        stop
c        end
c------------------------------------------------------------------

      implicit none
      integer ifail, ip, np, nr
      logical lp
      double precision p(np), r(nr)
      double precision eps, one, x, zero
      integer i, ierr, j, k

      double precision epsmach8

      data one /1.0d0/, zero /0.0d0/
      ierr=1
      if (np.lt.1) goto 180
      ierr=2
      if (nr.lt.(np+3)) goto 180
      eps=epsmach8(eps)
      ierr=3
      if (lp) goto 60
      j=1
      do i=1,np
         if (p(i).lt.zero) goto 180
         if (p(i).gt.eps) j=i
      enddo
      x=zero
      do i=1,j
         k=nr-j+i
         x=x+p(i)
         r(k)=x
      enddo
      goto 120
   60 x=zero
      j=0
      do i=1,np
         if (p(i).lt.x) goto 180
         x=p(i)
         if (p(i).lt.one) j=i
      enddo
      if (j.lt.np) j=j+1
      do i=1,j
         k=nr-j+i
         r(k)=p(i)
      enddo
  120 if (r(nr).eq.zero) goto 180
      k=ip-nr+j-1
      j=nr-j+1
      do i=j,nr
         if (r(i).gt.eps) goto 160
      enddo
      i=nr

  160 call refindx8(k,i,r,nr)
      ifail=0

      return
  180 ifail=ierr
      return
      end

cxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
      subroutine refindx8 (m,n,r,nr)
c
c     Sets up the index part of the reference vector.
c
      implicit none
      integer m, n, nr
      double precision r(nr)
      double precision half, one, x, y, zero
      integer i, j, k

      data half /0.5d0/, zero /0.0d0/, one /1.0d0/

      r(1)=dfloat(n-3)
      r(2)=dfloat(m) + half
c     this is in case truncation is towards -infinity.
      if (idint(r(2)).gt.m) r(2)=r(2)-one
      x=r(nr)
      do i=n,nr
         r(i)=r(i)/x
      enddo
      x=one/r(1)
      y=zero
      j=n-1
      k=n
      do i=3,j
   20    if (r(k).gt.y) goto 40
         k=k+1
         goto 20
   40    r(i)=dfloat(k) + half
         y=y + x
      enddo
      return
      end

cxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
      function epsmach8(x)
c
c
c    1.0 + eps = 1
c
c
      double precision eps,eps1,epsmach8,x
      eps=1.0d0
10    eps=0.5d0*eps
      eps1=1.0d0+eps
      if(eps1.gt.1.0d0) goto 10
      epsmach8=2.0d0*eps

      return
      end
