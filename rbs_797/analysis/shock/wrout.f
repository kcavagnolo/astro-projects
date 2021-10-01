CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Output and input of flow data
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C***********************************************************
C
C Crude attempt to see if the name string is defined
C
      function chkname(name)
      implicit none
      logical chkname
      character name*(*)
      if (len(name).le.0.or.ichar(name).eq.0.or.name(1:1).eq.' ') then
         chkname=.false.
      else
         chkname=.true.
      endif
      return
      end
C
C
C***********************************************************
C
      subroutine wrout(eta,kick,avis,cfac,t,n,m,r,vr,temp,oname)
      implicit none
      integer n
      real*8 eta,kick,avis,cfac,t
      real*8 m(n),r(n+1),vr(n+1),temp(n)
      character oname*(*)
      logical chkname
      character fname*256
C Attempt to see if the file name is defined
      if (.not.chkname(oname)) then
         print *,'Output file name?'
         read *,fname
      else
         fname=oname
      endif
      open (unit=1,file=fname,form='UNFORMATTED',err=1,status='NEW')
      write (1,err=2)eta,kick,avis,cfac,t,n
      write (1,err=2)m,r,vr,temp
      close (1)
      return
 1    continue
      print *,'Failed to open',fname
      stop 1
 2    continue
      print *,'Error while writing flow data to file'
      stop 1
      end
C
C
C***********************************************************
C
      subroutine rdin(eta,kick,avis,cfac,t,nmax,n,m,r,vr,temp,fname)
      implicit none
      integer nmax,n
      real*8 eta,kick,avis,cfac,t
      real*8 m(nmax),r(nmax+1),vr(nmax+1),temp(nmax)
      character fname*(*)
      logical chkname
      integer i
C Attempt to see if file name is defined
      if (.not.chkname(fname)) then
         print *,'Input file name?'
         read *,fname
      endif
      open (unit=2,file=fname,form='UNFORMATTED',err=1,status='OLD')
      read (2,err=2)eta,kick,avis,cfac,t,n
      if (n.gt.nmax) then
         print *,'Insufficient room for flow arrays in rdin',nmax,n
         stop 1
      endif
      read (2,err=2)(m(i),i=1,n),(r(i),i=1,n+1),(vr(i),i=1,n+1),
     &              (temp(i),i=1,n)
      close (2)
      return
 1    continue
      print *,'Failed to open',fname
      stop 1
 2    continue
      print *,'Error reading flow data in rdin'
      stop 1
      end
