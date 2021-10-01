CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Decrement over the centre of a cavity in a double beta model
C atmosphere.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C***********************************************************
C
C SB over the centre of a cavity in a beta model, normalized to
C 1 with no cavity.
C Arguments:
C beta = for the beta model
C a = core radius for the beta model
C r = cavity radius (same units as a)
C pom = distance of cavity centre from cluster centre
C z = distance of cavity centre from plane of sky
C
      function cavsb(beta,a,r,pom,z)
      implicit none
      real*8 cavsb,beta,a,r,pom,z
      real*8 aa,bb,tt,zz,x,res,fincbeta
      aa=3.d0*beta-0.5
      bb=0.5d0
      tt=pom**2+a**2
      zz=z-r
      x=tt/(tt+zz**2)
      if (zz.lt.0.d0) then
         res=fincbeta(aa,bb,x)
      else
         res=2.d0-fincbeta(aa,bb,x)
      endif
      zz=z+r
      x=tt/(tt+zz**2)
      res=res+fincbeta(aa,bb,x)
      cavsb=0.5d0*res
      return
      end
C
C
C***********************************************************
C
C Derivative of normalized SB above with respect to z.
C
      function dsbdz(beta,a,r,pom,z)
      implicit none
      real*8 dsbdz,beta,a,r,pom,z
      real*8 aa,bb,fbeta,onl2
      real*8 bsave,norm
      save bsave,norm
      data bsave /0.d0/
      if (beta.ne.bsave) then
C         print *,'dsbdz: init'
         bsave=beta
         aa=3.d0*beta-0.5d0
         bb=0.5d0
         norm=1.d0/fbeta(aa,bb)
      endif
      onl2=1.d0/(pom**2+a**2)
      dsbdz=norm*((1.d0+(z-r)**2*onl2)**(-3.d0*beta)
     &     -(1.d0+(z+r)**2*onl2)**(-3.d0*beta))
     &     *sqrt(onl2)
      return
      end
C
C
C***********************************************************
C
C Solve for z giving specified decrement
C
      function soldec(sb1,beta1,a1,sb2,beta2,a2,r,pom,zi,targ)
      implicit none
      real*8 soldec,sb1,beta1,a1,sb2,beta2,a2,r,pom,zi,targ
      integer i,nit
      parameter (nit=10)
      real*8 z,del,cavsb,slope,dsbdz,res
      z=zi
      do i=1,nit
         del=1.d0-targ-sb1*cavsb(beta1,a1,r,pom,z)
     &        -sb2*cavsb(beta2,a2,r,pom,z)
         slope=-sb1*dsbdz(beta1,a1,r,pom,z)-sb2*dsbdz(beta2,a2,r,pom,z)
         z=z-del/slope
C         print *,i,z,slope,del
      end do
      if (abs(del).gt.1.d-10) then
         print *,'soldec: failed to converge'
         stop 1
      endif
      soldec=z
      return
      end
C
C
      implicit none
      integer npt,i
      parameter (npt=301)
      real*8 cavsb,sb1,beta1,a1,sb2,beta2,a2
      real*8 relnorm,norm1,norm2
      real*8 ra,rb,sbc,esbc,sban,esban
      real*8 r,pom,z,res,soldec,targ,zt,ztl,zth,zi,dmax
      real*4 xx(npt),yy(npt),xmin,xmax,ymin,ymax
      character fname*256,label*4
      if (iargc().ne.1) then
         call getarg(0,fname)
         print *,'Usage: ',fname(1:lnblnk(fname)),' <Decrement_file>'
         stop 1
      endif
      call getarg(1,fname)
      open (unit=2,file=fname,status='OLD')
C      print *,'First beta model parameters: A, beta and a?'
      read *,sb1,beta1,a1
      print *,'First beta model parameters: A, beta and a',sb1,beta1,a1
C      print *,'Second beta model parameters: A, beta and a?'
      read *,sb2,beta2,a2
      print *,'Second beta model parameters: A, beta and a',sb2,beta2,a2
      do while (.true.)
         print *
C         print *,'Cavity radius, distance from centre and decrement?'
         read (2,*,err=1)label,ra,rb,r,pom,sbc,esbc,sban,esban
         targ=1.d0-sbc
         print *,'Cavity radius, distance from centre and decrement:',
     &        r,pom,targ
         relnorm=sb2*(1.d0+pom**2/a2**2)**(0.5d0-3.d0*beta2)
     &        /(sb1*(1.d0+pom**2/a1**2)**(0.5d0-3.d0*beta1))
         print *,'norm_2/norm_1:',relnorm
         norm1=1.d0/(1.d0+relnorm)
         norm2=relnorm/(1.d0+relnorm)
         zi=0.d0
         do i=1,npt
            z=(i-1)*(3.d0*pom)/(npt-1)
            xx(i)=z
            res=norm1*cavsb(beta1,a1,r,pom,z)
     &           +norm2*cavsb(beta2,a2,r,pom,z)
            yy(i)=res
            if (i.eq.1) then
               dmax=1.d0-res
               print *,'Maximum decrement:',dmax
            endif
            if (1.d0-res.gt.targ) then
               zi=z
            endif
         end do
         z=pom/sqrt(2.d0)
         res=norm1*cavsb(beta1,a1,r,pom,z)+norm2*cavsb(beta2,a2,r,pom,z)
         print *,'Typical distance from plane of sky:',z,
     &        ', gives decrement:',1.d0-res
         if (dmax.le.targ) then
            zt=-1.d0
            ztl=-1.d0
         else
            zt=soldec(norm1,beta1,a1,norm2,beta2,a2,r,pom,zi,targ)
            targ=1.d0-sbc+esbc
            if (dmax.le.targ) then
               ztl=-1.d0
            else
               ztl=soldec(norm1,beta1,a1,norm2,beta2,a2,r,pom,zi,targ)
            endif
         endif
         targ=1.d0-sbc-esbc
         if (dmax.le.targ) then
            zth=-1.d0
         else
            zth=soldec(norm1,beta1,a1,norm2,beta2,a2,r,pom,zi,targ)
         endif
         print *,'Distance from plane of sky for target decrement:',
     &        zt,ztl,zth
         xmin=0.0
         xmax=3.d0*pom
         ymin=0.0
         ymax=1.0
         call pgbegin(0,'?',1,1)
         call pgenv(xmin,xmax,ymin,ymax,0,0)
         call pgline(npt,xx,yy)
         call pglab('Distance from plane of sky','Central SB',
     &      'Brightness at cavity centre vs distance distance from sky')
         call pgend
      end do
 1    continue
      end
