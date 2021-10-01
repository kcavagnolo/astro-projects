CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Revised cooling function, based on data of
C B\"ohringer & Hensler.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C***********************************************************
C
      subroutine stcool
      implicit none
      integer ndmax,ndata,i
      parameter (ndmax=34)
      real*8 t(ndmax),cf0(ndmax),cf2(ndmax)
      real*8 rs2(ndmax),a0(ndmax),b0(ndmax),c0(ndmax),d0(ndmax)
      real*8 a2(ndmax),b2(ndmax),c2(ndmax),d2(ndmax)
      real*8 mu(ndmax+1),nu(ndmax+1),lambda,dx
      real*8 back0(ndmax),back2(ndmax)
      real*8 ab0(ndmax),bb0(ndmax),cb0(ndmax),db0(ndmax)
      real*8 ab2(ndmax),bb2(ndmax),cb2(ndmax),db2(ndmax)
      integer npt,k
      parameter (npt=1001)
      real x(npt),y0(npt),y2(npt)
      common /cftab/ x,y0,y2
C The data give the cooling function for Z=0 and Z=2 at equal
C logarithmic intervals from 10^4 to 10^8 K.
      open (unit=2,name='cfn.dat',err=1,status='old')
      do ndata=1,ndmax
         read (2,*,end=2)t(ndata),cf0(ndata),cf2(ndata)
      end do
 3    continue
      print *,'Should be 33 entries - have read:',ndata
      stop 1
 1    continue
      print *,'Unable to open cfn.dat'
      stop 1
 2    continue
      ndata=ndata-1
      if (ndata.ne.33) goto 3
C Spline fits for the two data sets
      do i=1,ndata
         rs2(i)=1.d0
         back0(ndata+1-i)=cf0(i)
         back2(ndata+1-i)=cf2(i)
      end do
C      print *,'Lambda:'
C      read *,lambda
      lambda=.0001d0
C Zero abundance cooling function
      call fdriver(ndata,t,cf0,rs2,lambda,a0,b0,c0,d0,mu,nu)
      call fdriver(ndata,t,back0,rs2,lambda,ab0,bb0,cb0,db0,mu,nu)
C This kludge is to compensate for the numerical failings of
C my spline fitting routine.  Take half the coefficients from the
C forward solution and the other half from the reverse solution.
C      i=(ndata+1)/2
C      print *,'ays:',a0(i),ab0(ndata+1-i)
C      print *,'dees:',d0(i),-db0(ndata-i)
      do i=(ndata+1)/2,ndata
         a0(i)=ab0(ndata+1-i)
         b0(i)=-bb0(ndata+1-i)
         c0(i)=cb0(ndata+1-i)
         if (i.lt.ndata) then
            d0(i)=-db0(ndata-i)
         else
            d0(i)=0.d0
         endif
      end do
C Z=2 cooling function
      call fdriver(ndata,t,cf2,rs2,lambda,a2,b2,c2,d2,mu,nu)
      call fdriver(ndata,t,back2,rs2,lambda,ab2,bb2,cb2,db2,mu,nu)
C      i=(ndata+1)/2
C      print *,'ays:',a2(i),ab2(ndata+1-i)
C      print *,'dees:',d2(i),-db2(ndata-i)
      do i=(ndata+1)/2,ndata
         a2(i)=ab2(ndata+1-i)
         b2(i)=-bb2(ndata+1-i)
         c2(i)=cb2(ndata+1-i)
         if (i.lt.ndata) then
            d2(i)=-db2(ndata-i)
         else
            d2(i)=0.d0
         endif
      end do
C Check plots
C      call pgbegin(0,'?',1,1)
C      call pgenv(3.9,8.1,-23.6,-20.8,0,0)
C      do i=1,ndata
C         x(i)=t(i)
C         y0(i)=cf0(i)
C         y2(i)=cf2(i)
C      end do
C      call pgpoint(ndata,x,y0,4)
C      call pgpoint(ndata,x,y2,5)
C Fill in the tables of the cooling function
      k=1
      do i=1,npt
         x(i)=t(1)+(t(ndata)-t(1))*(i-1.d0)/(npt-1.d0)
         do while (k.lt.ndata.and.t(k+1).le.x(i))
            k=k+1
         end do
         dx=x(i)-t(k)
         y0(i)=a0(k)+(b0(k)+(c0(k)+d0(k)*dx)*dx)*dx
         y2(i)=a2(k)+(b2(k)+(c2(k)+d2(k)*dx)*dx)*dx
         x(i)=10.**(x(i))
         y0(i)=10.**(y0(i))
         y2(i)=10.**(y2(i))
         y2(i)=.5*(y2(i)-y0(i))
      end do
C      call pgline(npt,x,y0)
C      call pgline(npt,x,y2)
C      call pgend
      end
C
C
C***********************************************************
C
C Cooling function (cgs).
C Arguments:
C t = temperature (K)
C a = abundance (cosmic units)
C
      function cool(t,a)
      implicit none
      real cool,t,a
      integer npt,il,ir,i
      parameter (npt=1001)
      real tt(npt),y0(npt),abd(npt)
      common /cftab/ tt,y0,abd
C Check for low temperatures
      if (t.lt.tt(1)) then
         print *,'Temperature too low in cool:',t
         stop 1
      endif
C Kludge for high temperatures - assume pure brems
      if (t.ge.tt(npt)) then
C         print *,'Temperature too high in cool:',t
         cool=y0(npt)*sqrt(t/tt(npt))
         return
      endif
C Binary search for the right interval
      il=1
      ir=npt
      do while (il+1.lt.ir)
         i=(il+ir)/2
         if (t.lt.tt(i)) then
            ir=i
         else
            il=i
         endif
      end do
      cool=((y0(il)+a*abd(il))*(tt(ir)-t)+
     &      (y0(ir)+a*abd(ir))*(t-tt(il)))/(tt(ir)-tt(il))
      return
      end
C
C
C***********************************************************
C
      implicit none
      real t,a,lam,cool,ne,fhe,k,spy,kev,tkev,tcool
      parameter (fhe=1./13.,k=1.38e-23,spy=3.156e7,kev=1.60e-16)
      external cool
      call stcool
      do while (.true.)
         print *,'Temperature (keV), n_e (cm**-3) and abundance:'
         read *,tkev,ne,a
         t=tkev*(kev/k)
         if (t.lt.1.e4) then
            stop
         endif
         lam=cool(t,a)
         print *,'Cooling function (cgs): ',lam
         tcool=1.5*(2.+fhe)/(1.-fhe)*(tkev*1e7*kev)/(ne*lam*spy)
         print *,'Cooling time (y): ',tcool
      end do
      end
