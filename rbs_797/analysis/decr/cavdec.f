C Calculates the decrement over the center of a cavity in a double beta
C model atmosphere normalized to 1 with no cavity.
C
C Copyright (C) Paul E. J. Nulsen, nulsen@cfa.harvard.edu
C Modified by Kenneth W. Cavagnolo, kcavagno@uwaterloo.ca
C
C This program is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published by
C the Free Software Foundation; either version 2 of the License, or (at
C your option) any later version.
C                                                                           
C This program is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
C General Public License for more details.
C                                                                           
C You should have received a copy of the GNU General Public License
C along with this program; if not, write to:
C    Free Software Foundation, Inc.
C    51 Franklin St, Fifth Floor
C    Boston, MA 02110-1301

C**********************************
C Evaluate the beta-model decrement
C**********************************

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

C****************************************************
C Derivative of normalized SB above with respect to z
C****************************************************

      function dsbdz(beta,a,r,pom,z)
      implicit none
      real*8 dsbdz,beta,a,r,pom,z
      real*8 aa,bb,fbeta,onl2
      real*8 bsave,norm
      save bsave,norm
      data bsave /0.d0/
      if (beta.ne.bsave) then
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

C***************************************
C Solve for z giving specified decrement
C***************************************

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
      end do
      if (abs(del).gt.1.d-10) then
         print *,'## ERROR: soldec failed to converge'
         stop 1
      endif
      soldec=z
      return
      end

C*************
C Main program
C*************

      implicit none
      integer npt,i
      parameter (npt=1001)
      real*8 cavsb,sb1,beta1,a1,sb2,beta2,a2
      real*8 relnorm,norm1,norm2
      real*8 ra,rb,sbc,esbc,sban,esban
      real*8 r,pom,z,res,soldec,targ,zt,ztl,zth,zi,dmax,dout
      real*4 xx(npt),yy(npt),xmin,xmax,ymin,ymax
      character infile*256,outfile1*256,outfile2*256,label*4
      if (iargc().ne.3) then
         call getarg(0,infile)
         print *,'## Usage: linux> ./cavdec <In> <Outdat> <Outtab> '
         stop 1
      endif
      call getarg(1,infile)
      call getarg(2,outfile1)
      call getarg(3,outfile2)
      open (unit=2,file=infile,status='OLD')
      open (unit=3,file=outfile1,status='NEW')
      open (unit=4,file=outfile2,status='NEW')
      write(3,*) '# POS_Distance SB_Central'
      write(3,*) '# kpc --'
      write(4,*) '#cav reff R   maxy z   zmin zmax'
      write(4,*) '#-   kpc  kpc -    kpc kpc  kpc'
      read *,sb1,beta1,a1
      print *,'1st beta-model component:'
      print *,'   S0:   ',sb1
      print *,'   rc:   ',a1
      print *,'   beta: ',beta1
      read *,sb2,beta2,a2
      print *,'2nd beta-model component:'
      print *,'   S0:   ',sb2
      print *,'   rc:   ',a2
      print *,'   beta: ',beta2
      print *,'############################'
      print *,'############################'
      do while (.true.)
         read (2,*,err=1)label,ra,rb,r,pom,sbc,esbc,sban,esban
         write(3,*) -r,-sbc
         targ=1.d0-sbc
         relnorm=(sb2*(1.d0+pom**2/a2**2)**(0.5d0-3.d0*beta2))
     &        /(sb1*(1.d0+pom**2/a1**2)**(0.5d0-3.d0*beta1))
         print *,'Cavity ',label
         print *,'   beta (eta2/eta1):',relnorm
         print *,'   r_cav:           ',r,' kpc'
         print *,'   proj. distance:  ',pom,' kpc'
         print *,'   Target decrement:',targ
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
               print *,'   Max decrement:   ',dmax
            endif
            if (1.d0-res.gt.targ) then
               zi=z
            endif
            write(3,*) z,res
         end do
         write(3,*) -999,-999
         z=pom/sqrt(2.d0)
         res=norm1*cavsb(beta1,a1,r,pom,z)+norm2*cavsb(beta2,a2,r,pom,z)
         print *,'   Typical POS dist:',z,
     &        ' gives decrement:',res
         if (dmax.le.targ) then
            zt=-1.000001d0
            ztl=-1.000001d0
         else
            zt=soldec(norm1,beta1,a1,norm2,beta2,a2,r,pom,zi,targ)
            targ=1.d0-sbc+esbc
            if (dmax.le.targ) then
               ztl=-1.000001d0
            else
               ztl=soldec(norm1,beta1,a1,norm2,beta2,a2,r,pom,zi,targ)
            endif
         endif
         targ=1.d0-sbc-esbc
         if (dmax.le.targ) then
            zth=-1.000001d0
         else
            zth=soldec(norm1,beta1,a1,norm2,beta2,a2,r,pom,zi,targ)
         endif
         if (zth.le.0) then
            print *,'   z for target dec: ',zt,ztl,zth
         else
            print *,'   z for target dec:',zt,ztl,zth
         endif
         write(4,*) label,r,pom,1.d0-dmax,zt,ztl,zth
         print *,'############################'
         print *,'############################'
      end do
 1    continue
      close (2)
      close (3)
      close (4)
      end



