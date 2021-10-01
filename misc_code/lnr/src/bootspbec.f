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
      
