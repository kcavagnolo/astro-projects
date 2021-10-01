CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Explosive spherical shock model.
C This shock is driven by large initial thermal
C energy in the central cell - which is treated as
C ordinary matter.
C Simple isothermal gravity allows the initial atmosphere
C to be hydrostatic (rho = r^-eta, T = 1).
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C***********************************************************
C
C Difference scheme.
C Arguments:
C dt = time step
C n = number of cells
C m(n) = cell masses
C r(n) = radii of cell boundaries (working values)
C vr(n) = velocities of cell boundaries (working values)
C temp(n) = cell temperatures (working values)
C ri(n), vri(n), tempi(n) = initial values as above
C rf(n), vrf(n), tempf(n) = final values as above
C eta = initial density exponent (scaling for gravity)
C avis = Richmyer & Morton artifical viscosity parameter
C
      subroutine diffsc(dt,n,m,r,vr,temp,ri,vri,tempi,rf,vrf,tempf,
     &     eta,avis)
      implicit none
      integer n
      real*8 dt,m(n),r(n+1),vr(n+1),temp(n),ri(n+1),vri(n+1),tempi(n)
      real*8 rf(n+1),vrf(n+1),tempf(n+1),eta,avis
      integer i
      real*8 dr,onv,div,theta,peff,pepr,heat
C
C Update positions
C
      do i=1,n+1
         rf(i)=ri(i)+dt*vr(i)
      end do
C
C Velocities and energies
C
      do i=1,n
         dr=r(i+1)-r(i)
C
C        pi / V(shell)
C
         onv=0.75d0/(dr*(r(i+1)**2+(r(i+1)+r(i))*r(i)))
C
C        Div v
C
         div=4.d0*(r(i+1)**2*vr(i+1)-r(i)**2*vr(i))*onv
C
C        Artificial viscous temperature
C
         if (div.lt.0.d0) then
            theta=avis*(div*dr)**2
         else
            theta=0.d0
         endif
C
C        Effective pressure (times pi)
C
         peff=m(i)*onv*(temp(i)+theta)
C
C        Update velocity
C
         if (i.eq.1) then
C           Inner BC
            vrf(1)=0.d0
         else if (i.eq.2) then
C           Allow for large size and high sound speed of cell 1
C           - pressure applies to cell edge.
            vrf(2)=vri(2)+dt*
     &           ((8.d0/3.d0)*(r(3)**2+(r(3)+r(2))*r(2))*
     &           (pepr-peff)/m(2)-eta/r(2))
         else
            vrf(i)=vri(i)+dt*
     &           ((8.d0/3.d0)*(r(i+1)**2+(r(i+1)+r(i-1))*r(i-1))*
     &           (pepr-peff)/(m(i)+m(i-1))-eta/r(i))
         endif
         pepr=peff
C
C        Adiabatic plus viscous heating rate per unit mass
C
         heat=-div*(temp(i)+theta)
C
C        Update temperature
C
         tempf(i)=tempi(i)+(2.d0/3.d0)*dt*heat
      end do
C
C     No compression in outermost cell
C
      vrf(n+1)=vrf(n)*(rf(n)/rf(n+1))**2
      return
      end
C
C
C***********************************************************
C
C One time step
C
      subroutine tstep(t,dt,n,m,r,vr,temp,rw,vrw,tempw,eta,avis)
      implicit none
      integer n
      real*8 t,dt,m(n),r(n+1),vr(n+1),temp(n),rw(n+1),vrw(n+1),tempw(n)
      real*8 eta,avis
      integer i
C
C The explicit step
C
      call diffsc(dt,n,m,r,vr,temp,r,vr,temp,rw,vrw,tempw,eta,avis)
C
C Implicit step
C
      call diffsc(dt,n,m,rw,vrw,tempw,r,vr,temp,r,vr,temp,eta,avis)
C
C Final is average of two steps
C
      do i=1,n
         r(i)=0.5d0*(r(i)+rw(i))
         vr(i)=0.5d0*(vr(i)+vrw(i))
         temp(i)=0.5d0*(temp(i)+tempw(i))
      end do
      r(n+1)=0.5d0*(r(n+1)+rw(n+1))
      vr(n+1)=0.5d0*(vr(n+1)+vrw(n+1))
      t=t+dt
      return
      end
C
C
C***********************************************************
C
C Get timestep
C
      function courant(cfac,n,r,vr,temp)
      implicit none
      integer n
      real*8 courant,cfac,r(n+1),vr(n+1),temp(n)
      integer i
      real*8 tc
C
C Courant condition
C
      if (temp(1).lt.0.0) then
         print *,'Negative temperature in cell 1:',temp(1)
         courant=-1.d0
         return
      endif
C
C This fudge to remove some start up nasties
C
      tc=min(r(2)-r(1),r(3)-r(2))/(sqrt(temp(1))+abs(vr(2)-vr(1)))
      do i=2,n
         if (temp(i).lt.0.d0) then
            print *,'Negative temperature in cell:',i,temp(i)
            courant=-1.d0
            return
         endif
         tc=min(tc,(r(i+1)-r(i))/(sqrt(temp(i))+abs(vr(i+1)-vr(i))))
      end do
      courant=cfac*tc
      return
      end
C
C
C***********************************************************
C
C Initial conditions - lot's of energy in the innermost cell
C
      subroutine init(n,r,m,vr,temp,eta,kick,avis,cfac)
      implicit none
      integer n
      real*8 r(n+1),m(n),vr(n+1),temp(n),eta,kick,avis,cfac
      integer i
      real*8 rmin,rmax,pi
      parameter (pi=3.1415926535897932384d0)
      print *,'rmin, rmax?'
      read *,rmin,rmax
      print *,'eta (density = r**-eta)?'
      read *,eta
      r(1)=0.d0
      vr(1)=0.d0
      do i=1,n
         r(i+1)=rmin*(rmax/rmin)**((i-1)/dble(n-1))
         temp(i)=1.0
         m(i)=(4.d0*pi/(3.d0-eta))*(r(i+1)**(3.d0-eta)-r(i)**(3.d0-eta))
         vr(i+1)=0.d0
      end do
      print *,'kick, avis, cfac?'
      read *,kick,avis,cfac
      temp(1)=kick
      return
      end
C
C
C***********************************************************
C
C Run the flow for a number of steps
C
      function runit(nstep,tend,t,n,m,r,vr,temp,rw,vrw,tempw,
     &                 eta,avis,cfac)
      implicit none
      integer runit,nstep,n
      real*8 tend,t,r(n+1),m(n),vr(n+1),temp(n),rw(n+1),vrw(n+1)
      real*8 tempw(n),eta,avis,cfac
      integer istep
      real*8 dt,courant
      istep=1
      do while (istep.lt.nstep.and.t.lt.tend)
         dt=courant(cfac,n,r,vr,temp)
         if (dt.le.0.d0) then
            print *,'Trouble in courant'
            runit=1
            return
         endif
         call tstep(t,dt,n,m,r,vr,temp,rw,vrw,tempw,eta,avis)
         istep=istep+1
      end do
      runit=0
      return
      end
C
C
C***********************************************************
C
      implicit none
      integer n,nstep,runit,iret,nfile,jbase
      parameter (n=1001)
      real*8 t,m(n),r(n+1),vr(n+1),temp(n),rw(n+1),vrw(n+1),tempw(n)
      real*8 kick,avis,cfac,tout,tend,tnext,eta
      character outfbase*256,outfile*256
      call init(n,r,m,vr,temp,eta,kick,avis,cfac)
      t=0.0
      tnext=0.0
      print *,'Steps per output, time per output, final time?'
      read *,nstep,tout,tend
      print *,'Output file base name?'
      read *,outfbase
      jbase=lnblnk(outfbase)
      nfile=0
      iret=0
      do while (t.lt.tend.and.iret.eq.0)
         tnext=tnext+tout
         iret=runit(nstep,tnext,t,n,m,r,vr,temp,rw,vrw,tempw,
     &              eta,avis,cfac)
         if (iret.eq.0) then
            print *,'Time: ',t
            if (nfile.gt.999) then
               print *,'Too many output files'
               stop 1
            endif
            write (outfile,'(a,i3.3)')outfbase(1:jbase),nfile
            nfile=nfile+1
            call wrout(eta,kick,avis,cfac,t,n,m,r,vr,temp,
     &                 outfile(1:jbase+3))
         endif
      end do
      end
