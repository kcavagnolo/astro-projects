CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C X-ray SB profile for a shock model in a r^{-\eta} gas
C density distribution.  Version for publication
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C***********************************************************
C
C SB for one point
C Arguments:
C pom = cylindrical radius of line of sight
C nshck = flow data points in shocked region
C r(nshck+1) = radii for flow data
C demp(nshck) = difference in emission measure per unit volume
C               between shocked and unshocked gas
C eta = initial density is r**-eta
C sbnorm = scaling factor for unperturbed SB profile
C
      function sb(pom,nshck,r,demp,eta,sbnorm)
      implicit none
      integer nshck
      real*8 r(nshck+1)
      real sb,pom,demp(nshck),eta,sbnorm
      real ell
      integer i
C
C Unperturbed SB
C
      sb=sbnorm*pom**(1.0-2.0*eta)
C
C Net contribution of shocked region
C
      i=nshck
      do while (pom.lt.r(i+1).and.i.ge.2)
         ell=sqrt(r(i+1)**2-pom**2)
         if (pom.lt.r(i)) then
            ell=ell-sqrt(r(i)**2-pom**2)
         endif
         sb=sb+ell*demp(i)
         i=i-1
      end do
      return
      end
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
C Make SB profile for model
C Arguments:
C n = number of model cells
C m(n) = cell masses
C r(n+1) = radii of cell boundaries
C vr(n+1) = velocities of cell boundaries
C temp(n) = cell temperatures (scaled)
C tunsh = temperature of unshocked gas
C demp(n) = array for difference between emission per unit volume
C           of shocked gas and unperturbed gas
C rmin, rmax = plot range
C nsb = number of SB points to make
C rsb(nsb) = radii for SB plot
C sbp(nsb) = SB results for plotting
C eta = unperturbed rho ~ r^{-\eta}
C sbnfac = normalising constant for model SB profile
C
      subroutine sbprof(n,m,r,vr,temp,tunsh,demp,rmin,rmax,nsb,rsb,sbp,
     &     rshck,eta,sbnfac)
      implicit none
      integer n,nsb
      real*8 m(n),r(n+1),vr(n+1),temp(n)
      real tunsh,demp(n),rmin,rmax
      real rsb(nsb),sbp(nsb)
      real rshck,eta,sbnfac
      integer i,ishck
C      real rho,pi,sb,rhounsh,oem,emfac,sbnorm,rresc,sresc,gammln
      real rho,pi,sb,rhounsh,oem,tresp,sbnorm,rresc,sresc,gammln,elo,ehi
      real tt
      parameter (pi=3.1415926535897932384,elo=0.5,ehi=7)
      logical ambient
C
C Find the shocked region
C
      i=n
      ambient=.true.
      do while (i.gt.1.and.ambient)
         rho=m(i)/((4.*pi/3.)*
     &        (r(i+1)-r(i))*(r(i)**2+r(i)*r(i+1)+r(i+1)**2))
         rhounsh=(2.0/(r(i)+r(i+1)))**eta
         if (rho.gt.1.001*rhounsh) then
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
C
C Tabulate emission measure per unit volume
C
      oem=tresp(elo,ehi,tunsh)
C Normalization for SB in the initial model atmosphere
      sbnorm=oem*0.5*sqrt(pi)*exp(gammln(eta-0.5)-gammln(eta))
C Conversion from model SB to observed SB
      sresc=sbnfac/(rresc**(2.0*eta-1.0)*sbnorm)
      print *,'radius, density, velocity, temperature:'
      do i=1,ishck
         rhounsh=(2.0/(r(i)+r(i+1)))**eta
         rho=m(i)/((4.*pi/3.)*
     &        (r(i+1)-r(i))*(r(i)**2+r(i)*r(i+1)+r(i+1)**2))
         tt=temp(i)
         demp(i)=rho**2*tresp(elo,ehi,tt)-rhounsh**2*oem
         if (i.gt.ishck-10) then
            print *,i,r(i),rho,vr(i),temp(i)
         endif
      end do
C
C Tabulate SB
C
      do i=1,nsb
         rsb(i)=(rmin/rresc)*(rmax/rmin)**((i-1)/(nsb-1.0))
         sbp(i)=sresc*sb(rsb(i),ishck,r,demp,eta,sbnorm)
         rsb(i)=rresc*rsb(i)
      end do
      return
      end
C
C
C***********************************************************
C
C Read SB data from a file
C Arguments:
C nsm = number of entries there is room for
C nrs = number of entries read (returned)
C rin(nsm), rout(nsm) = bin boundaries for SB data
C rsb(nsm) = SB data
C rsbe(nsm) = SB errors
C eta = initial density is r**-eta
C sbnfac = normalization used to scale model to data
C
      subroutine rdsb(nsm,nrs,rin,rout,rsb,rsbe,eta,sbnfac)
      implicit none
      integer nsm,nrs
      real rin(nsm),rout(nsm),rsb(nsm),rsbe(nsm),eta,sbnfac
      character buff*256,sbfile*64
      integer i,j
      real num,denom,xi,rnmin,rnmax,r,fac
      print *,'Surface brightness profile?'
      read '(a)',sbfile
      open (unit=2,file=sbfile,status='OLD',err=1)
C Skip heading line and get first line of data
      read (2,'(a)',end=2,err=3)buff
      read (2,'(a)',end=2,err=3)buff
C Get data entries and normalising constant
      nrs=1
      print *,'Range for normalising model, rnmin, rnmax (pixels)?'
      read *,rnmin,rnmax
      j=0
      num=0.0
      denom=0.0
      xi=2.0*eta-1.0
      do while (nrs.le.nsm)
         read (buff,*)rin(nrs),rout(nrs),rsb(nrs),rsbe(nrs)
         if (rin(nrs).gt.rnmin.and.rout(nrs).lt.rnmax) then
            r=0.5*(rin(nrs)+rout(nrs))
            fac=r**(-xi)/rsbe(nrs)
            num=num+fac*rsb(nrs)/rsbe(nrs)
            denom=denom+fac**2
            j=j+1
         endif
         read (2,'(a)',end=4,err=3)buff
         nrs=nrs+1
      end do
      print *,'rdsb: out of room'
      stop 1
 4    continue
      close (2)
      if (j.eq.0) then
         print *,'rdsb: no data in normalising range'
         stop 1
      endif
      sbnfac=num/denom
      return
 2    continue
      print *,'rdsb: unexpected end of SB data file'
      stop 1
 3    continue
      print *,'rdsb: error reading SB profile'
      stop 1
 1    continue
      print *,'rdsb: failed to open SB data file'
      stop 1
      end
C
C
C***********************************************************
C
      implicit none
      integer ncmax,ncell,i,nsb,nsm,nrs
      parameter (nsb=501,ncmax=1001,nsm=200)
      real rmin,rmax,rin(nsm),rout(nsm),resb(nsm),rsbe(nsm),sbnfac,rshck
      real rc(nsm),er(nsm)
      real*8 m(ncmax),r(ncmax+1),vr(ncmax+1),temp(ncmax)
      real demp(ncmax)
      real rsb(nsb),sbp(nsb)
      real etau
      real*8 eta,kick,avis,cfac,t
      real sbmin,sbmax,tunsh
      real secperpix
      integer slen
      parameter (slen=256)
      character fname*(slen),ans*(16),lbuf*256
      logical checke
C NB: Pixel size assumed fixed
      data secperpix /0.492/
C Get SB data to compare with models
      print *,'eta?'
      read *,etau
      call rdsb(nsm,nrs,rin,rout,resb,rsbe,etau,sbnfac)
C Range for plot
      print *,'Plot range rmin, rmax (pixels)?'
      read *,rmin,rmax
C Make arrays for plotting SB data
      sbmin=-100.0
      do i=1,nrs
C Bin centre and 'error' in radius
         if (rin(i).eq.0.0) then
            rc(i)=0.5*(rin(i)+rout(i))
         else
            rc(i)=sqrt(rin(i)*rout(i))
         endif
         checke=rout(i).ge.rmin.and.rin(i).le.rmax
         rc(i)=log10(rc(i)*secperpix)
         er(i)=log10(rout(i)*secperpix)-rc(i)
C Bin centre and error for SB
         if (resb(i).gt.0.0) then
            rsbe(i)=log10(1.0+rsbe(i)/resb(i))
            resb(i)=log10(resb(i))
         else
            resb(i)=-100.0
            rsbe(i)=0.0
         endif
         if (checke) then
            if (sbmin.le.-100.0) then
               sbmin=resb(i)-rsbe(i)
               sbmax=resb(i)+rsbe(i)
            else if (resb(i).gt.-100.0) then
               sbmin=min(sbmin,resb(i)-rsbe(i))
               sbmax=max(sbmax,resb(i)+rsbe(i))
            endif
         endif
      end do
C Plot SB data
      sbmin=sbmin-0.03*(sbmax-sbmin)
      sbmax=sbmax+0.03*(sbmax-sbmin)
      call pgbegin(0,'?',1,1)
      call pgslw(3)
      call pgsch(1.5)
      call pgenv(log10(rmin*secperpix),log10(rmax*secperpix),
     &     sbmin,sbmax,0,30)
      call pgerrb(5,nrs,rc,resb,er,1.0)
      call pgerrb(6,nrs,rc,resb,rsbe,1.0)
C Temperature of unshocked gas
      print *,'Unshocked temperature (keV)?'
      read *,tunsh
C Shock radius
      print *,'Shock radius (pixels)?'
      read *,rshck
C Loop, plotting models
      print *,'More (y/n)?'
      read '(a)',ans
      do while (ans(1:1).eq.'y'.or.ans(1:1).eq.'Y')
C
C Read flow data
C
         fname = ' '
         call rdin(eta,kick,avis,cfac,t,ncmax,ncell,m,r,vr,temp,fname)
         if (abs(eta-etau).gt.1.d-5*eta) then
            print *,'Mismatching etas:',etau,eta
            stop 1
         endif
C Scale cell temperatures
         do i=1,ncell
            temp(i)=tunsh*temp(i)
         end do
C
C Make and plot SB profile
C
         call sbprof(ncell,m,r,vr,temp,tunsh,demp,rmin,rmax,nsb,rsb,sbp,
     &        rshck,etau,sbnfac)
         do i=1,nsb
            rsb(i)=log10(rsb(i)*secperpix)
            sbp(i)=log10(sbp(i))
         end do
         call pgline(nsb,rsb,sbp)
         print *,'More (y/n)?'
         read '(a)',ans
      end do
      call pglab('r (arcsec)','SB (ct pixel\\u-1\\d s\\u-1\\d)','')
      call pgend
      end
