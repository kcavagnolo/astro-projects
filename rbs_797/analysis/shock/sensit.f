CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routines to read in tables of relative sensitivity for
C Chandra bands.  Old version relies on Larry David's table
C (lpdtsensit) that was made for Hydra A.  The new version
C at the end has a single table giving response for the system
C of interest and a band identified in the file.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C***********************************************************
C
C Set up the tables of relative count rate.  This reads in
C Larry's table from the original email.
C
      subroutine stemt
      implicit none
      include 'emtab.inc'
      integer i,ngarb
      parameter (ngarb=31)
      real norm
C
C Read raw sensitivities from file
C
      ntable=0
      norm=-1.0
      open (unit=2,file='lpdtsensit',err=1,status='OLD')
      goto 2
 1    continue
      print *,'Failed to open lpdsesnsit in stemt'
      stop 1
 3    continue
      print *,'Unexpected end of file in lpdsensit'
      stop 1
 2    continue
C
C Skip leading junk
C
      do i=1,ngarb
         read (2,*,end=3)
      end do
C
C Get the data
C
      do while (ntable.lt.ntmax)
         ntable=ntable+1
         read (2,*,end=4)kt(ntable),soft(ntable),hard(ntable)
         if (kt(ntable).eq.5.0) then
            norm=soft(ntable)+hard(ntable)
         endif
      end do
      print *,'Out of room for E.M. table in stemt'
      stop 1
 4    continue
C
C Normalise at 5 keV
C
      if (norm.le.0.0) then
         print *,'Failed to find normalising emission measure'
         stop 1
      endif
      ntable=ntable-1
      norm=1.0/norm
      do i=1,ntable
         soft(i)=norm*soft(i)
         hard(i)=norm*hard(i)
      end do
      close (2)
      return
      end
C
C
C***********************************************************
C
C Relative count rate vs temperature.
C Arguments:
C temp = temperature in keV
C band = 1 - soft
C        2 - hard
C        3 - soft + hard
C
      function emfac(temp,band)
      implicit none
      real emfac,temp
      integer band
      include 'emtab.inc'
      integer i,ilo,ihi
      if (temp.lt.0.75*kt(1)) then
C
C Disallow temperatures more than a little off the bottom of the table
C
         print *,'Cannot handle temperatures <',0.75*kt(1),temp
         stop 1
      else if (temp.ge.kt(ntable)) then
C
C Assume 1/sqrt(T) above the top end of the table
C
         if (band.eq.1) then
            emfac=soft(ntable)
         else if (band.eq.2) then
            emfac=hard(ntable)
         else if (band.eq.3) then
            emfac=soft(ntable)+hard(ntable)
         else
            print *,'Invalid band in emfac:',band
            stop 1
         endif
         emfac=emfac*sqrt(kt(ntable)/temp)
      else
C
C Binary search for interval
C
         ilo=1
         ihi=ntable
         do while (ihi.gt.ilo+1)
            i=(ilo+ihi)/2
            if (temp.ge.kt(i)) then
               ilo=i
            else
               ihi=i
            endif
         end do
         if (band.eq.1) then
            emfac=(soft(ilo)*(kt(ihi)-temp)+soft(ihi)*(temp-kt(ilo)))/
     &         (kt(ihi)-kt(ilo))
         else if (band.eq.2) then
            emfac=(hard(ilo)*(kt(ihi)-temp)+hard(ihi)*(temp-kt(ilo)))/
     &         (kt(ihi)-kt(ilo))
         else if (band.eq.3) then
            emfac=((soft(ilo)+hard(ilo))*(kt(ihi)-temp)+
     &           (soft(ihi)+hard(ihi))*(temp-kt(ilo)))/(kt(ihi)-kt(ilo))
         else
            print *,'Invalid band in emfac:',band
            stop 1
         endif
      endif
      return
      end
C
C
C***********************************************************
C
      subroutine sensitfake
      implicit none
      integer npt,i
      parameter (npt=301)
      real kt(npt),soft(npt),hard(npt)
      real ktmin,ktmax,cemin,cemax,emfac
C
C Initialise relative emission measure table
C
      call stemt
C
C Fill plot arrays
C
      print *,'ktmin, ktmax?'
      read *,ktmin,ktmax
      cemax=0.0
      cemin=1.0
      do i=1,npt
         kt(i)=ktmin+(i-1)*(ktmax-ktmin)/(npt-1)
         soft(i)=emfac(kt(i),1)
         hard(i)=emfac(kt(i),2)
         cemax=max(cemax,soft(i),hard(i))
         cemin=min(cemin,soft(i),hard(i))
         kt(i)=log10(kt(i))
      end do
      cemax=1.03*cemax-.03*cemin
      cemin=1.03*cemin-.03*cemax
      call pgbegin(0,'?',1,1)
      call pgenv(log10(ktmin),log10(ktmax),cemin,cemax,0,10)
      call pgline(npt,kt,soft)
      call pgsls(2)
      call pgline(npt,kt,hard)
      call pglab('kT (keV)','Relative count rate',
     &           'Soft & Hard count rate')
      call pgend
      end
C
C
C***********************************************************
C
C Set up sensitivity table
C
      subroutine stresp
      implicit none
      integer ntmax,ntable
      parameter (ntmax=40)
      real b_low,b_high,kt(ntmax),em(ntmax)
      common /nem/ ntable,b_low,b_high,kt,em
      integer i,inorm
      real low,high,ktnorm,nfac
      parameter (ktnorm=5.0)
      character fname*256
      print *,'Sensitivity file?'
      read *,fname
      open (unit=2,file=fname,status='OLD')
      inorm=0
      do i=1,ntmax
         read (2,*,end=1)low,high,kt(i),em(i)
C Save and check band
         if (i.eq.1) then
            b_low=low
            b_high=high
         else if (low.ne.b_low.or.high.ne.b_high) then
            print *,'nstemt: unexpected band:',low,high,b_low,b_high
            stop 1
         endif
C Find nearest to normalizing temperature
         if (kt(i).le.5.0) then
            inorm=i
         endif
      end do
      i=ntmax+1
      read (2,*,end=1)low
      print *,'nstemt: insufficient room'
      stop 1
 1    continue
      ntable=i-1
      nfac=1.0/em(inorm)
      do i=1,ntable
         em(i)=nfac*em(i)
      end do
      return
      end
C
C
C***********************************************************
C
C Relative sensitivity as a function of temperature.
C Gives relative count rate as a function of temperature for
C a fixed band.
C
      function tresp(low,high,temp)
      implicit none
      real tresp,low,high,temp
      integer ntmax,ntable
      parameter (ntmax=40)
      real b_low,b_high,kt(ntmax),em(ntmax)
      common /nem/ ntable,b_low,b_high,kt,em
      integer i,idn,iup
      logical inited
      save inited
      data inited /.false./
      if (.not.inited) then
         call stresp
         inited=.true.
      endif
      if (low.ne.b_low.or.high.ne.b_high) then
         print *,'tresp: wrong energy band:',low,high,b_low,b_high
         stop 1
      endif
      if (temp.lt.kt(1).or.temp.gt.kt(ntable)) then
         print *,'tresp: temperature outside table range:',temp,
     &        kt(1),kt(ntable)
         stop 1
      endif
      idn=1
      iup=ntable
      do while (iup-idn.gt.1)
         i=(idn+iup)/2
         if (temp.lt.kt(i)) then
            iup=i
         else
            idn=i
         endif
      end do
      tresp=(em(idn)*(kt(iup)-temp)+em(iup)*(temp-kt(idn)))
     &     /(kt(iup)-kt(idn))
      return
      end
C
C
      subroutine nsensfake
      implicit none
      integer npt,i
      parameter (npt=301)
      real x(npt),y(npt),xmin,xmax,ymin,ymax,emin,emax,tresp
      parameter (emin=0.5,emax=7)
      print *,'Temperature range (keV)?'
      read *,xmin,xmax
      ymax=0.0
      do i=1,npt
         x(i)=xmin*(xmax/xmin)**((i-1)/(npt-1.0))
         y(i)=tresp(emin,emax,x(i))
         x(i)=log10(x(i))
         ymax=max(ymax,y(i))
      end do
      xmin=log10(xmin)
      xmax=log10(xmax)
      ymin=0.0
      ymax=1.03*ymax
      call pgbegin(0,'?',1,1)
      call pgenv(xmin,xmax,ymin,ymax,0,10)
      call pgline(npt,x,y)
      call pglab('kT (keV)','Relative count rate',
     &     'Count rate vs temperature for a fixed emission measure')
      call pgend
      end
