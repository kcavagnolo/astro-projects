CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Locate a shock and determine the jump conditions,
C ie shock strength
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C***********************************************************
C
C Shock velocity jump to shock Mach number.
C Arguments:
C mps = velocity difference in units of preshock sound speed
C gamma = ratio of specific heats
C
      function mofv(mps,gamma)
      implicit none
      real*8 mofv,mps,gamma
      real*8 t
      t=abs(0.25d0*(gamma+1.d0)*mps)
      mofv=t+sqrt(1.d0+t**2)
      return
      end
C
C
C***********************************************************
C
C Shock temperature jump to Mach number
C Arguments:
C theta = T2/T1
C gamma = ratio of specific heats
C
      function moft(theta,gamma)
      implicit none
      real*8 moft,theta,gamma
      real*8 w,m2
      if (theta.le.1.d0) then
         print *,'moft: no shock for theta =',theta
         stop 1
      endif
      w=0.5d0*(gamma+1.d0)*(theta-1.d0)/(gamma-1.d0)
      m2=((gamma-1.d0)+(gamma+1.d0)*(w+sqrt(theta+w**2)))/(2.d0*gamma)
      moft=sqrt(m2)
      return
      end
C
C
C***********************************************************
C
C Shock density jump to Mach number
C Arguments:
C r = rho2/rho1
C gamma = ratio of specific heats
C
      function mofr(r,gamma)
      implicit none
      real*8 mofr,r,gamma
      real*8 x
      if (r.le.1.d0) then
         print *,'mofr: density must increas:',r
         stop 1
      endif
      x=gamma+1.d0-(gamma-1.d0)*r
      if (x.le.0.d0) then
         print *,'mofr: density ratio too large:',r,x
         stop 1
      endif
      mofr=sqrt(2.d0*r/x)
      return
      end
C
C
C***********************************************************
C
      implicit none
      integer ncmax,ncell
      parameter (ncmax=1001)
      real*8 m(ncmax),r(ncmax+1),vr(ncmax+1),temp(ncmax)
      real*8 eta,kick,avis,cfac,t
      integer slen,i,ish,imin,irm,ivm
      parameter (slen=256)
      character fname*(slen)
      logical ambient
      real*8 rho,rhounsh,mofv,moft,mofr,mv,mt,mr,gamma,pi,rmax,vmax,tsh
      parameter (pi=3.1415926535897932384d0)
      if (iargc().eq.1) then
         call getarg(1,fname)
      endif
      call rdin(eta,kick,avis,cfac,t,ncmax,ncell,m,r,vr,temp,fname)
C
C Find the outer edge of the shocked region
C
      i=ncell
      ambient=.true.
      do while (i.gt.1.and.ambient)
         rho=m(i)/((4.d0*pi/3.d0)*
     &        (r(i+1)-r(i))*(r(i)**2+r(i)*r(i+1)+r(i+1)**2))
         rhounsh=(2.d0/(r(i)+r(i+1)))**eta
         if (rho.gt.1.001d0*rhounsh) then
            ambient=.false.
         else
            i=i-1
         endif
      end do
      if (i.le.1) then
         print *,'No shocked region'
         stop 1
      endif
      print *,'i, rho, shock radius:',i,rho,r(i+1)
      ish=i
C
C Compare to unshocked density at same radius
C
      imin=max(i-10,1)
      rmax=0.d0
      vmax=0.d0
      print *,'i, radius, velocity, density, temperature:'
      do i=imin,ish
         rho=m(i)/((4.d0*pi/3.d0)*
     &        (r(i+1)-r(i))*(r(i)**2+r(i)*r(i+1)+r(i+1)**2))
         rhounsh=(2.d0/(r(i)+r(i+1)))**eta
         if (rho.gt.rmax*rhounsh) then
            rmax=rho/rhounsh
            irm=i
            tsh=temp(i)
         endif
         if (vr(i).gt.vmax) then
            vmax=vr(i)
            ivm=i
         endif
         print *,i,r(i),vr(i),rho,temp(i)
      end do
      print *,'Density and temperature at:',irm
      print *,'Velocity at:',ivm
      gamma=5.d0/3.d0
C Assume preshock velocity is zero
      mv=mofv(vmax/sqrt(gamma*temp(ish+1)),gamma)
      mr=mofr(rmax,gamma)
C Assume preshock temperature is 1
      mt=moft(tsh,gamma)
      print *,'Mach number from velocity, density, temperature:'
      print *,mv,mr,mt
      end
