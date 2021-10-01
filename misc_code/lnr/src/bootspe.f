c ------------------------------------------------------------------------------
      subroutine bootspe (nmax,n,x,y,yerr,xboot,yboot,yerrboot,iseed)
c ------------------------------------------------------------------------------
c     Constructs Monte Carlo simulated data set using the
c     Bootstrap algorithm.                               
c                                                        
c     Random number generator is real function ran3.f, from
c     Numerical Recipes (an adaptation of Knuth's algorithm).
c
      implicit double precision (x,y)
      dimension x(nmax),y(nmax),yerr(nmax),
     &     xboot(nmax),yboot(nmax),yerrboot(nmax)
      integer i,j,n,nmax
      real*4 ran3
      external ran3
c     
      do 10 i = 1,n
         j = int(ran3(iseed)*float(n) + 1.0)
         xboot(i) = x(j)
         yboot(i) = y(j)
         yerrboot(i) = yerr(j)
 10   continue
c     
      return
      end
      
