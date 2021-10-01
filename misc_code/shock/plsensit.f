CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Plot the sensitivity vs temperature
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
      implicit none
      integer nsmax,i,ns
      parameter (nsmax=128)
      real kt(nsmax),sens(nsmax),elo,ehi,xmin,xmax,ymin,ymax,elf,ehf
      character fname*256
      if (iargc().ne.1) then
         call getarg(0,fname)
         print *,'Usage: ',fname(1:lnblnk(fname)),' <sensitivities>'
         stop 1
      endif
      call getarg(1,fname)
      open (unit=2,file=fname,status='OLD')
      ymax=0.0
      do i=1,nsmax
         read (2,*,end=1)elo,ehi,kt(i),sens(i)
         if (i.eq.1) then
            elf=elo
            ehf=ehi
            print *,'Energy range: ',elf,ehf
         else if (elo.ne.elf.or.ehi.ne.ehf) then
            print *,'Energy range should be fixed:',elf,elo,ehf,ehi
            stop 1
         endif
         kt(i)=log10(kt(i))
         ymax=max(ymax,sens(i))
      end do
      print *,'Read off end of table'
      stop 1
 1    continue
      ns=i-1
      xmin=kt(1)
      xmax=kt(ns)
      ymin=0.0
      ymax=1.03*ymax
      call pgbegin(0,'?',1,1)
      call pgenv(xmin,xmax,ymin,ymax,0,10)
      call pgline(ns,kt,sens)
      call pglab('kT (keV)','Relative count rate',
     &     'Temperature dependence of the sensitivity')
      call pgend
      end
