CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Profile of the emission measure weighted temperature across a
C numerically calculated shock front.
C NB: Assumes unperturbed rho = r^{-eta} (including norm)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C***********************************************************
C
C Natural log of the gamma function.  Straight from Numerical
C Recipes.  This is Lanczos formula.  Assumes a positive
C argument.
C
      function gammln(xx)
      implicit none
      real*4 gammln,xx
      real*8 x,tmp,ser,cof(6)
      integer j
      data cof /76.18009173,-86.50532033,24.01409822,
     &          -1.231739516,0.120858003d-2,-0.536382d-5/
      if (xx.gt.1.) then
         x=xx-1.0
      else
C For x < 1, compute gamma(x+1)
         x=xx
      endif
      tmp=x+5.5d0
      tmp=(x+.5d0)*log(tmp)-tmp
      ser=1.d0
      do j=1,6
         x=x+1.d0
         ser=ser+cof(j)/x
      end do
      if (xx.gt.1.) then
         gammln=tmp+log(2.50662827465*ser)
      else
C Computed gamma(x+1), use gamma(x)=gamma(x+1)/x
         gammln=tmp+log(2.50662827465*ser/xx)
      endif
      return
      end
C
C
C***********************************************************
C
C Integrals of emission measure and emission measure weighted
C temperature at one position
C Arguments:
C pom = cylindrical radius of line of sight
C nshck = flow data points in shocked region
C r(nshck+1) = radii for flow data
C temp(nshck) = temperatures for flow data
C emps(nshck) = emission measure per unit volume of shocked gas
C empu(nshck) = emission measure per unit volume of unshocked gas
C eta = initial density is r**-eta
C sbnorm = scaling factor for unperturbed SB profile
C emint = emission measure integrated along line of sight
C emwtint = integratral of emission measure weighted temperature
C
      subroutine demwt(pom,nshck,r,temp,emps,empu,eta,sbnorm,
     &     emint,emwtint)
      implicit none
      integer nshck
      real*8 r(nshck+1),temp(nshck)
      real pom,emps(nshck),empu(nshck),eta
      real sbnorm,emint,emwtint
      real ell,em
      integer i
C Unperturbed emission measure
      emint=sbnorm*pom**(1.0-2.0*eta)
      emwtint=emint
C
C Net contribution of shocked region
C
      i=nshck
      do while (pom.lt.r(i+1).and.i.ge.2)
         ell=sqrt(r(i+1)**2-pom**2)
         if (pom.lt.r(i)) then
            ell=ell-sqrt(r(i)**2-pom**2)
         endif
C Remove missing unshocked gas
         emint=emint-empu(i)*ell
         emwtint=emwtint-empu(i)*ell
C Add shocked gas
         emint=emint+emps(i)*ell
         emwtint=emwtint+emps(i)*ell*temp(i)
         i=i-1
      end do
      return
      end
C
C
C***********************************************************
C
C Emission measure weighted temperature profile
C Arguments:
C n = size of flow data arrays
C m(n) = cell masses
C r(n+1) = shell radii
C vr(n+1) = shell velocities
C temp(n) = cell temperatures
C emps = emission measure per unit volume of cells
C empu = emission measure per unit volume of unshocked gas at same radius
C rmin, rmax = range of radii over which to get EM weighted temperature
C              (in observed units)
C nsb = number of radii at which to compute EM weighted temperature
C rsb(nsb) = points at which DEM is computed
C emwt(nsb) = emission measure weighted temperatures
C rshck = shock radius (observed units)
C eta = power law for unperturbed density profile
C 
      subroutine emwtprof(n,m,r,vr,temp,emps,empu,rmin,rmax,nsb,rsb,
     &     emint,emwtint,emwt,rshck,eta)
      implicit none
      integer n,nsb
      real*8 m(n),r(n+1),vr(n+1),temp(n)
      real emps(n),empu(n),rmin,rmax
      real rsb(nsb),emint(nsb),emwtint(nsb),emwt(nsb),rshck,eta
      integer i,ishck,j
      real rho,pi,rhounsh,sbnorm,rresc,gammln
      parameter (pi=3.1415926535897932384)
      logical ambient
C
C Find the shocked region
C
      i=n
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
C Conversion from model radius to observed radius
      rresc=rshck/r(i+1)
      ishck=i
C Normalization for SB in the initial model atmosphere
      sbnorm=0.5*sqrt(pi)*exp(gammln(eta-0.5)-gammln(eta))
      print *,'radius, density, velocity, temperature:'
      do i=1,ishck
         rhounsh=(2.d0/(r(i)+r(i+1)))**eta
         empu(i)=rhounsh**2
         rho=m(i)/((4.d0*pi/3.d0)*
     &        (r(i+1)-r(i))*(r(i)**2+r(i)*r(i+1)+r(i+1)**2))
         emps(i)=rho**2
         if (i.gt.ishck-10) then
            print *,i,r(i),rho,vr(i),temp(i)
         endif
      end do
C
C Compute EM weighted temperature
C
      do i=1,nsb
         rsb(i)=(rmin/rresc)*(rmax/rmin)**((i-1)/(nsb-1.0))
         call demwt(rsb(i),ishck,r,temp,emps,empu,eta,sbnorm,
     &        emint(i),emwtint(i))
         emwt(i)=emwtint(i)/emint(i)
         rsb(i)=rresc*rsb(i)
      end do
      return
      end
C
C
C***********************************************************
C
      implicit none
      integer ncmax,ncell,i,nsb,nsm,nrs,nb
      parameter (nsb=301,ncmax=1001,nsm=100)
      real rmin,rmax,rshck
      real*8 m(ncmax),r(ncmax+1),vr(ncmax+1),temp(ncmax)
      real emps(ncmax),empu(ncmax)
      real rsb(nsb),emint(nsb),emwtint(nsb),emwt(nsb)
      real etau
      real*8 eta,kick,avis,cfac,t
      integer slen,nplot
      parameter (slen=256)
      character fname*(slen),ans*(16),lbuf*256
      real temin,temax
C Range for plot
      print *,'Plot range: rmin, rmax (arcsec)?'
      read *,rmin,rmax
      print *, 'Using rmin, rmax:', rmin, rmax
      print *,'eta?'
      read *,etau
      print *, 'Using eta:', etau
      print *,'Domain of temperature (about 1.0)?'
      read *,temin,temax
      print *, 'Using temperature range:', temin, temax
      print *,'Shock radius (arcsec)?'
      read *,rshck
      print *, 'Using shock radius:', rshck
      call pgbegin(0,'?',1,1)
      call pgslw(2)
      call pgsch(1.5)
      call pgenv(rmin,rmax,temin,temax,0,0)
      nplot=0
      print *,'More (y/n)?'
      read *,ans
      do while (ans(1:1).eq.'y'.or.ans(1:1).eq.'Y')
C
C Read flow data
C
         nplot=nplot+1
         fname=''
         call rdin(eta,kick,avis,cfac,t,ncmax,ncell,m,r,vr,temp,fname)
         if (abs(eta-etau).gt.1.d-5*eta) then
            print *,'Mismatching etas:',etau,eta
            stop 1
         endif
C
C Make EM weighted temperature profile
C
         call emwtprof(ncell,m,r,vr,temp,emps,empu,rmin,rmax,nsb,
     &        rsb,emint,emwtint,emwt,rshck,etau)
         call pgline(nsb,rsb,emwt)
         print *,'More (y/n)?'
         read '(a)',ans
      end do
      if (nplot.eq.1) then
         write (lbuf,*)'Abell 2199 model ',fname(1:len_trim(fname))
         call pglab('r (arcsec)','EM weighted temperature',
     &        lbuf(1:len_trim(lbuf)))
      else
         call pglab('r (arcsec)','EM weighted temperature','')
      endif
      call pgend
      end
