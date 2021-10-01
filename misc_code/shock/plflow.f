CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Plot flow quantities from a file
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C***********************************************************
C
C Plot density
C
      subroutine plden(t,n,m,r,x,y)
      implicit none
      integer n
      real*8 t,m(n),r(n+1)
      real x(n),y(n)
      integer i
      real*8 rho,pi
      real ymin,ymax
      character top*24
      parameter (pi=3.1415926535897932384d0)
      do i=1,n
         rho=m(i)/((4.d0*pi/3.d0)*(r(i+1)**3-r(i)**3))
         y(i)=log10(rho)
         if (i.eq.1) then
            ymin=y(i)
            ymax=ymin
            x(1)=log10(r(2))
         else
            ymin=min(ymin,y(i))
            if (i.ne.2) then
               ymax=max(ymax,y(i))
            endif
            x(i)=log10(0.5d0*(r(i)+r(i+1)))
         endif
      end do
      ymin=1.03*ymin-.03*ymax-1.e-3
      ymax=1.03*ymax-.03*ymin+1.e-3
C      print *,'rho(1-10):',(10.**y(i),i=1,10)
      print *,'rho(1-10):',(y(i),i=1,10)
C     Log linear
      call pgenv(x(1)-0.05,x(n),ymin,ymax,0,30)
      call pgline(n,x,y)
      write (top,fmt='(a,f8.4)')'Density    t = ',t
      call pglab('r','\\gr',top)
      return
      end
C
C
C***********************************************************
C
C Plot temperature
C
      subroutine pltemp(t,n,r,temp,x,y)
      implicit none
      integer n
      real*8 t,r(n+1),temp(n)
      real x(n),y(n)
      integer i
      real ymin,ymax
      character top*30
      do i=1,n
         y(i)=temp(i)
         if (i.eq.1) then
            x(1)=log10(r(2))
         else
            if (i.eq.2) then
               ymax=y(2)
               ymin=y(2)
            endif
            ymin=min(ymin,y(i))
            ymax=max(ymax,y(i))
            x(i)=log10(0.5*(r(i)+r(i+1)))
         endif
      end do
      print *,'T(1-10), Tmax:',(temp(i),i=1,10),10.**ymax
      print *,'T(1), Tmax:',temp(1),ymax
C      ymin=1.03*ymin-.03*ymax-1.e-5
      ymin=0.0
      ymax=1.03*ymax-.03*ymin+1.e-5
      call pgenv(x(1),x(n),ymin,ymax,0,10)
      call pgline(n,x,y)
      write (top,fmt='(a,f8.4)')'Temperature    t = ',t
      call pglab('r','T',top)
      return
      end
C
C
C***********************************************************
C
C Plot entropy
C
      subroutine plent(t,n,m,r,temp,x,y)
      implicit none
      integer n
      real*8 t,m(n),r(n+1),temp(n)
      real x(n),y(n)
      integer i
      real*8 rho,pi
      real ymin,ymax
      parameter (pi=3.1415926535897932384d0)
      character top*30
      do i=2,n
         rho=m(i)/((4.d0*pi/3.d0)*(r(i+1)**3-r(i)**3))
         y(i-1)=log10(temp(i))-(2.d0/3.d0)*log10(rho)
         if (i.eq.2) then
            ymin=y(1)
            ymax=y(1)
         else
            ymin=min(ymin,y(i-1))
            ymax=max(ymax,y(i-1))
         endif
         x(i-1)=log10(0.5d0*(r(i)+r(i+1)))
      end do
      print *,'ymin, ymax:',ymin,ymax
      print *,'Entropies:',(y(i),i=1,10)
      ymin=1.03*ymin-.03*ymax-1.e-5
      ymax=1.03*ymax-.03*ymin+1.e-5
      call pgenv(x(1),x(n),ymin,ymax,0,10)
      call pgline(n-1,x,y)
      write (top,fmt='(a,f8.4)')'Entropy    t = ',t
      call pglab('r','log\\d10\\u T/\\gr\\u2/3\\d',top)
      return
      end
C
C
C***********************************************************
C
C Plot velocity
C
      subroutine plvel(t,n,r,vr,x,y)
      implicit none
      integer n
      real*8 t,r(n+1),vr(n)
      real x(n),y(n)
      integer i
      real ymin,ymax
      character top*26
      do i=2,n
         y(i)=vr(i)
         if (i.eq.2) then
            ymax=y(2)
            ymin=ymax
         else
            ymin=min(ymin,y(i))
            ymax=max(ymax,y(i))
         endif
         x(i)=log10(r(i))
      end do
      ymin=1.03*ymin-.03*ymax-1.e-3
      ymax=1.03*ymax-.03*ymin+1.e-3
      call pgenv(x(1),x(n),ymin,ymax,0,10)
      call pgline(n-1,x(2),y(2))
      write (top,fmt='(a,f8.4)')'Velocity    t = ',t
      call pglab('r','v\\dr\\u',top)
      return
      end
C
C
C***********************************************************
C
      implicit none
      integer nmax,n
      parameter (nmax=1001)
      real*8 t,m(nmax),r(nmax+1),vr(nmax+1),temp(nmax)
      real x(nmax),y(nmax)
      real*8 eta,kick,avis,cfac
      character fname*256
      if (iargc().eq.1) then
         call getarg(1,fname)
      endif
      call rdin(eta,kick,avis,cfac,t,nmax,n,m,r,vr,temp,fname)
      call pgbegin(0,'?',1,1)
      call pgslw(4)
      call pgsch(1.5)
      print *,'Time: ',t
      print *,'eta, kick, avis, cfac:',eta,kick,avis,cfac
      call plden(t,n,m,r,x,y)
      call plvel(t,n,r,vr,x,y)
      call pltemp(t,n,r,temp,x,y)
      call plent(t,n,m,r,temp,x,y)
      call pgend
      end
