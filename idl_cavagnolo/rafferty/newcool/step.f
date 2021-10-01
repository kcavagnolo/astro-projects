CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Cubic spline fitter (smoother)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C***********************************************************
C
C For spline fitting (smoothing).  Steps solution for mu and nu
C from k to k+1, and finds d(k).
C Notation as in my notes:
C Assumes that a(1),...a(k), b(1),...b(k), c(1),...c(k), d(1),...d(k-1),
C mu(1),...mu(k) and nu(1),...nu(k) are known on entry.  Determines
C mu(k+1), nu(k+1) and d(k).
C No sanity checks.
C
C Arguments:
C k = as described above
C n = number of data points
C x(n) = ordered array of abscissa values
C y(n) = data being fitted
C rs2(n) = reciprocal of sigma**2
C lambda = smoothing parameter
C a(n) = spline zeroth order coefficients
C b(n) = spline first order coefficients
C c(n) = spline second order coefficients
C d(n) = spline third order coefficients
C mu(n+1) = lagrange multipliers for continuity constraints
C nu(n+1) = lagrange multipliers for smoothness constraints
C
      subroutine dsolve(k,n,x,y,rs2,lambda,a,b,c,d,mu,nu)
      implicit none
      integer k,n
      real*8 x(n),y(n),rs2(n),lambda
      real*8 a(n),b(n),c(n),d(n),mu(n+1),nu(n+1)
      real*8 delp,del,rh
C
C For k = 1 delp does not matter anyway
C
      if (k.gt.1) then
         delp=x(k)-x(k-1)
      else
         delp=0.d0
      endif
C
C This fudge does affect the value mu(n+1), etc.  However, when
C those values vanish it does not affect the solution.
C
      if (k.lt.n) then
         del=x(k+1)-x(k)
      else
         del=delp
      endif
C
C First get mu(k+1)
C
      rh=mu(k)*delp**2+3.d0*nu(k)*(delp+del)
      if (k.eq.1) then
         rh=rh-12.d0*lambda*c(1)*del
      else if (k.eq.n) then
         rh=rh+12.d0*lambda*c(n)*del
      endif
      mu(k+1)=rh/del**2
C
C then nu(k+1)
C
      rh=-(2.d0/3.d0)*rh-(1.d0/3.d0)*mu(k)*delp**2-nu(k)*delp
      nu(k+1)=rh/del
C
C next d(k)
C
      rh=(2.d0*rs2(k)*(y(k)-a(k))+mu(k+1)-mu(k))/(12.d0*lambda)
      if (k.gt.1) then
         d(k)=d(k-1)+rh
      else
         d(k)=rh
      endif
      return
      end
C
C
C***********************************************************
C
C Uses continuity conditions to step solutions for a(k), b(k)
C and c(k) from k to k+1.
C
C Arguments:
C k = as above
C n = number of data points
C x(n) = x data
C a(n) = zeroth order spline coefficients
C b(n) = first order spline coefficients
C c(n) = second order spline coefficients
C d(n) = third order spline coefficients
C
      subroutine cont(k,n,x,a,b,c,d)
      implicit none
      integer k,n
      real*8 x(n),a(n),b(n),c(n),d(n)
      real*8 del
      del=x(k+1)-x(k)
C Continuity of second derivative
      c(k+1)=c(k)+3.d0*d(k)*del
C Continuity of first derivative
      b(k+1)=b(k)+(2.d0*c(k)+3.d0*d(k)*del)*del
C Continuity
      a(k+1)=a(k)+(b(k)+(c(k)+d(k)*del)*del)*del
      return
      end
C
C
C***********************************************************
C
C Does one trial solution.
C a(1), b(1), c(1) must be specified on entry.  This fills in
C the rest of the a, b, c, d, mu and nu arrays.
C
C Arguments:
C n = number of data points
C x(n) = x data
C y(n) = data being fitted
C rs2(n) = 1/sigma**2 for y data
C lambda = smoothing parameter
C a(n) = zeroth order spline coefficients
C b(n) = first order spline coefficients
C c(n) = second order spline coefficients
C d(n) = third order spline coefficients
C mu(n+1) = lagrange multipliers for the continuity constraints
C nu(n+1) = lagrange multipliers for the smoothness constraints
C
      subroutine stepper(n,x,y,rs2,lambda,a,b,c,d,mu,nu)
      implicit none
      integer n,k
      real*8 x(n),y(n),rs2(n),lambda,a(n),b(n),c(n),d(n)
      real*8 mu(n+1),nu(n+1)
C
C These reflect the lack of constraints for k = 1
C
      mu(1)=0.d0
      nu(1)=0.d0
C
C Get d(1), mu(2) and nu(2)
C
      call dsolve(1,n,x,y,rs2,lambda,a,b,c,d,mu,nu)
C
C rest of the solution
C
      do k=2,n
C
C Get a(k), b(k), c(k)
C
         call cont(k-1,n,x,a,b,c,d)
C
C Get d(k), mu(k+1), nu(k+1)
C
         call dsolve(k,n,x,y,rs2,lambda,a,b,c,d,mu,nu)
      end do
      return
      end
C
C
C***********************************************************
C
C Drives the solver.  This version uses the requirement
C (see my notes) that c(1) = 0 to speed the solution.
C The x data are assumed to be ordered.
C
C Arguments:
C n = number of data points
C x(n) = x data
C y(n) = data being fitted
C rs2(n) = 1/sigma**2 for y data
C lambda = smoothing parameter
C a(n) = zeroth order spline coefficients
C b(n) = first order spline coefficients
C c(n) = second order spline coefficients
C d(n) = third order spline coefficients
C mu(n+1) = lagrange multipliers for the continuity constraints
C nu(n+1) = lagrange multipliers for the smoothness constraints
C
      subroutine fdriver(n,x,y,rs2,lambda,a,b,c,d,mu,nu)
      implicit none
      integer n
      real*8 x(n),y(n),rs2(n),lambda
      real*8 a(n),b(n),c(n),d(n),mu(n+1),nu(n+1)
      real*8 ymax,ymin,epa,epr,epsd
      parameter (epr=1.d-14,epa=1.d-30,epsd=1.d-10)
      integer i
      real*8 abase,dela,delb,odn,omu,det,bbase
      real*8 delda,delma,deldb,delmb
C
C Find y data range
C
      ymax=y(1)
      ymin=ymax
      do i=2,n
         if (y(i).gt.ymax) then
            ymax=y(i)
         else if (y(i).lt.ymin) then
            ymin=y(i)
         endif
      end do
      if (ymax-ymin.lt.epa+epr*abs(ymax)) then
         print *,'Cannnot cope with silly data'
         stop 1
      endif
C
C Initially work around a(1) = abase and b(1) = bbase
C
      abase=y(1)
      bbase=0.d0
C
C Trial variation for a(1)
C
      dela=ymax-ymin
C
C Trial variation for b(1)
C
      delb=(ymax-ymin)/(x(n)-x(1))
      do i=1,2
C
C Record the trial solutions
C
C Base solution
         a(1)=abase
         b(1)=bbase
         c(1)=0.d0
         call stepper(n,x,y,rs2,lambda,a,b,c,d,mu,nu)
         odn=d(n)
         omu=mu(n+1)
C Offsets - note sign reversal in delta's
         a(1)=abase+dela
         call stepper(n,x,y,rs2,lambda,a,b,c,d,mu,nu)
         delda=odn-d(n)
         delma=omu-mu(n+1)
         a(1)=abase
         b(1)=bbase+delb
         call stepper(n,x,y,rs2,lambda,a,b,c,d,mu,nu)
         deldb=odn-d(n)
         delmb=omu-mu(n+1)
C
C Find a(1), b(1) which make d(n), mu(n+1) zero
C
         det=delda*delmb-deldb*delma
         a(1)=abase+dela*(odn*delmb-omu*deldb)/det
         b(1)=bbase+delb*(omu*delda-odn*delma)/det
C
C and the rest of it
C
         call stepper(n,x,y,rs2,lambda,a,b,c,d,mu,nu)
         if (abs(d(n))*(x(n)-x(n-1))**4.lt.epsd*(ymax-ymin)) then
            return
         endif
         abase=a(1)
         bbase=b(1)
         dela=.001d0*dela
         delb=.001d0*delb
      end do
      return
      end
C
C
C***********************************************************
C
C The cost function
C
      function cost(n,x,y,rs2,lambda,a,b,c,d)
      implicit none
      integer n
      real*8 cost
      real*8 x(n),y(n),rs2(n),lambda,a(n),b(n),c(n),d(n)
      real*8 par,del
      integer i
      cost=rs2(1)*(y(1)-a(1))**2
      do i=2,n
         cost=cost+rs2(i)*(y(i)-a(i))**2
         del=x(i)-x(i-1)
         par=d(i-1)*del
         cost=cost+4.d0*lambda*del*(c(i-1)**2+3.d0*par*(c(i-1)+par))
      end do
      return
      end
