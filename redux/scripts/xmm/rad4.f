      program virialrad
c
c   plots number of galaxies as a function of
c   distance from cluster centre for galaxies from apm catalogue
c   and fits a curve to the points.(NFW model using nag e04fdf)
c   Scott Porter bham 22 May 2003
c

c-setup-------------------------------------------

      implicit none

      integer max,n,abellmax
      integer nplot,n77
      parameter (max=3302175,nplot=10,abellmax=5000)
      integer hour1,deg2,min1,min2
      real*8 sec1,sec2,bjsel,bjsela(max),binsize
      real*8 ra,dec,raa(max),deca(max)
      real*8 rab(abellmax),decb(abellmax),bjselb(abellmax)
      integer icand,izero,nradbins
      parameter(icand=3302175,izero=0,nradbins=nplot)
      character*80 title,fstr1*7
      character*11 oname1,oname2
      character*5 oname3
      character*20 cned
      integer inmag,ierr4
      integer i,j,ij,ik,jj,ip(nradbins),ip2(nradbins),n2
      character*1 cdsign1,cdsign2
      real*8 rs1,theta,dyda(2)
      real*8 data(icand),datarad(icand),data_rad(nradbins,icand)
      real*8 datacum(icand),data_cum(nradbins,icand)
      real*8 radial(icand),radial1(icand),radial2(icand)
      real*8 data1(icand),data2(icand)
      real*8 zz,angular,distlum
      real*8 xv(10),vert1(10),vert2(10)
      real*4 xmin,xmax,ymin,ymax
      real*4 xfit(100),yfit(100)
      integer ih1,im1,jh1,jm1
      integer ss1,ivel
      character*11 name,zname
      character*5 cvned
      integer ivned,n4
      real*8 rs2,vgood,abellrad,angstep
      real*8 radcut(nradbins),distcut(nradbins)
      integer ss2,nmag2,jprev
      integer ih2,im2,jh2,jm2,n3
      real*8 mag1,mag2
      real*8 al,ab,glat,glong,angcent
      real*8 coscent(3),racent,decent,abcent,alcent,racent2,decent2
      integer nmatch,kk,ierr91,ierr5
      real*8 srdsq,searchrad,diffsq,distsq
      real*8 currdiff,radiff,decdiff
      real*8 pi,rad_deg,angle,hh
      character sign*1
      real*8 radius(nplot),numarea(nplot),radiusg(nplot)
      REAL*8 XPT(nplot),YPT(nplot)
      INTEGER SYMBOL    
      REAL*8 error(nplot),area(nplot)

      INTEGER MA
      REAL*8 SPREAD
      PARAMETER(MA=3,SPREAD=0.001)
      INTEGER ia(MA),idum,iter,itst,k,mfit
      REAL*8 alamda,chisq,gasdev,ochisq,x(nplot),y(nplot),sig(nplot),
     &a(MA),covar(MA,MA),alpha(MA,MA),gues(MA)     
      real*8 halflight
      real*8 ra2000,dec2000
      integer nnn,n5,clusmax
      PARAMETER (CLUSMAX=20)
      real*8 var1(clusmax),var2(clusmax),var3(clusmax),var4(clusmax)
      character*11 fred(clusmax)
      integer graph
      integer istat,pgopen
      integer n87,n88,n89
      real*8 xxx(nplot)

      real*4 xpt4(nplot),ypt4(nplot),sig4(nplot)
      
      
c-new e04fdf -------------------------------------------------

      integer mfree,nn,liw,lw
      parameter(mfree=ma,liw=10)
      parameter (lw=7*mfree+mfree*mfree+2*mfree*nplot+3*nplot+
     &mfree*(mfree-1)/2)
      character*5 cmstar,calpha,sum
      real*8 fsumsq
      integer iw(liw)
      real*8 w(lw)
      integer ifail
      real*4 absfitmin,absfitmax
      common/xanderror/sig,xxx,y
      common/rangers/absfitmin,absfitmax
      real*8 muppit(nplot)
      common/muppit/muppit
c------------------------------------------------------------
     
      hh=0.7  ! Hubble constant
      pi=4.*atan(1.)
      rad_deg=180./pi

      	absfitmin=0.05     
	absfitmax=1.55

c-read in xray centre of cluster----------------------------
        
cc        open(91,name='master_optical.out',status='old')
cc        read(91,*)
cc        do while (ierr91.eq.0)
cc           read(91,991,iostat=ierr91)fstr1,ra2000,dec2000,
cc     +         ivel,isig,r500
cc 991       format(a7,15x,f9.3,f11.3,i5,8x,i4,24x,f5.2)

       open(91,name='test',status='old')
c       open(7,name='fit_pisces_cetus',status='new')

 
        ierr91=0
    
        nnn=0

         do while (ierr91.eq.0)

            nnn=nnn+1

             read(91,991,iostat=ierr91)fstr1,ra2000,dec2000
c           write(6,991,iostat=ierr91)fstr1,ra2000,dec2000
          
991       format(a5,1x,f7.3,1x,f7.3)      

          oname3=fstr1
          
          write(6,*)"===================================="
          write(6,*)"calculating variables for cluster ",oname3
          write(6,*)"===================================="

           if(ierr91.eq.0) then

c	racent=ra2000/rad_deg
c	decent=dec2000/rad_deg

c	racent= (01.+(02./60.)+(45.0/3600.))*15./rad_deg  !a133 J2000
c	decent= (-21.-(53./60.)-(00./3600.))/rad_deg    

c          write(6,*)"old centres are",racent2,decent2
c        write(6,*)"centres are",racent,decent

c-convert centre in galactis lon and lat------------------------------

c        call ratob(racent,decent,abcent,alcent)
c	call cosines(alcent,abcent,coscent)
c	write(6,*)'Centre of cluster (l,b)' ,alcent*rad_deg,abcent*rad_deg

c-read in apm galaxies--------------------------------------------
     
        graph=nnn+10

        do j=1,10

         open(graph,name=oname3,status='old')
         read(graph,73)i,xpt(i),ypt(i),error(i)
         write(6,73)i,xpt(i),ypt(i),error(i)
 73      format (I2,2x,f10.8,2x,f10.6,2x,f10.7)
         enddo

c       oname2='apm'
c       open(unit=4,name=oname2,status='unknown')
cc       open(unit=4,name='/data1/somak/DATA/apm.dat',status='unknown')
        
c       n=0
c       n2=0
c       ierr4=0
      
c       do while (ierr4.eq.0)

c       read(4,100,iostat=ierr4)hour1,min1,sec1,sign,deg2,
c     &min2,sec2,bjsel
  
cc       write(6,100)hour1,min1,sec1,sign,deg2,
cc     &min2,sec2,bjsel      

c100   FORMAT (1x,I2,1x,I2,1x,F5.2,2x,A1,I2,1x,I2,1x,F4.1,
c     &2x,F5.2)
c      n=n+1
                      
c--for each close gal convert into ra and dec in radians------------------------
 
c      ra=15.*(hour1+real(min1)/60.+sec1/3600.)/rad_deg
c      dec=(ABS(deg2)+real(min2)/60.+sec2/3600.)/rad_deg
c      if (sign.eq.'-')dec=-dec

cc       write(6,100)hour1,min1,sec1,sign,deg2,
cc     &min2,sec2,bjsel   

cc      write(6,*)ra,dec

c-convert from 1950 to 2000----------------------------

c      call precess(ra,dec,2000.0)
     
c-------outputs galactic long and lat in radians-----------
     
c        call ratob(ra,dec,ab,al)

c	glong=al*rad_deg   !in degrees
c	glat=ab*rad_deg
c	angcent=angle(al,ab,coscent)
        
cc       write(6,*)angcent
        
c         Radial(n)=3600.*angcent*rad_deg    !arcsec
cc        write(6,*)radial(n)

c         raa(n)=ra
c        deca(n)=dec
c        bjsela(n)=bjsel
      
c         write(6,*)radial(n5)
         
c        enddo

c        write(6,*)"number read in =  ",n
      

c-find size of abell radius in H=70 --------------

c        zz=0.0566
c        call proper_dist(zz,0.5,distlum,1.5,angular)
cc        write(6,*)'Distance (assuming H=70) in Mpc= ',distlum/(100*hh)
c        abellrad=angular
c        write(6,*)'Angular Size of Abell radius (1.5 Mpc, H=100)= ',
c     +         abellrad,' arcsec'   

c-only take galaxies within the abbel radius ----------
   
c       n2=0
c       do i=1,n

cc          write(6,*)radial(i),abellrad

c          if(radial(i).le.abellrad) then
c             n2=n2+1

c        rab(n2)=raa(i)
c        decb(n2)=deca(i)
c        bjselb(n2)=bjsela(i)
c        radial2(n2)=radial(i)

c          endif
c          enddo

c        write(6,*)"number inside abell radius = ",n2
      
c-bin data------------------------------------------
              
c         angstep=abellrad/real(nradbins)
c         binsize=1.5/real(nplot)

c        do j=1,nradbins

c        ip(j)=0
c        ip2(j)=0
c        radcut(j)=real(j)*angstep
c        distcut(j)=real(j)*1.5/(real(nradbins)*hh)
      
c         do i=1,n2

c           if((j.eq.1.and.radial2(i).le.radcut(1)).or.
c     +          (j.gt.1.and.(radial2(i).gt.radcut(j-1))
c     +          .and.(radial2(i).le.radcut(j)))) then

c              ip(j)=ip(j)+1
              
c           endif

c        enddo
             
c        radius(j)=real(j)*binsize
c        radiusg(j)=  radius(j) -binsize/2.

c        If (j.gt.1) then
c             area(j)=(pi*(radius(j)**2))-(pi*(radius(j-1)**2))
c        else
c             area(j)=(pi*(radius(j)**2))
c         endif

c        error(j)=(sqrt(real(ip(j))))/area(j)

c         numarea(j)=real(ip(j))/area(j)
 
c         write(6,*)"an:",j,"= number,radius,area,num/area,error"
c        write(6,500)ip(j),radius(j),area(j),numarea(j),error(j)

c 500    format(I3,1x,f8.4,1x,f8.4,1x,f8.4,1x,f8.4)

c        enddo

           DO  i=1,nplot
c           XPT(i)=radiusg(i)
c           YPT(i)=numarea(i)

c           graph=nnn+10
c            open(graph,name=oname3,status='new')

c           write(graph,88)i,xpt(i),ypt(i),error(i)
c 88         format (I2,2x,f10.8,2x,f10.6,2x,f10.7)

        xxx(i)=xpt(i)
        xpt4(i)=sngl(xpt(i))
        muppit(i)=xpt(i)
         

        y(i)=ypt(i)
        ypt4(i)=sngl(ypt(i))

        sig(i)=error(i)
        sig4(i)=sngl(error(i))

         enddo
     
c-plotting---------------------------------
 
        xmin=0.
        xmax=1.6
        ymin=0.
        ymax=420.0

c-title-------------------------------

c        ISTAT = PGOPEN(oname3//'.ps/PS')
c       IF (ISTAT .LE. 0 ) STOP    
       call pgbegin(0,'?',1,1)
ccc        call pgpage
        call pgsch(0.4)
        CALL PGSLW(3)
        CALL PGSVP (0.0, 1.0, 0.9, 1.0)
        
        call pgsch(1.0)
        call pgmtxt('T',-1.0,0.5,0.5,
     & 'Pisces-Cetus')
         call pgmtxt('T',-2.0,0.5,0.5,
     & oname3)

c-points------------------------------

        call pgsch(0.9)
        CALL PGSLW(3)

         CALL PGSVP (0.1, 0.9, 0.1, 0.9)
cc        CALL PGSwin (xmin,xmax,ymin,ymax)

        CALL PGswin (xmin,xmax,ymin,ymax)
        CALL PGBOX  ('TNBC', 0.15, 5.0, 'BCTNV', 30.0, 1.0)

        call pgsch(0.9)
        call pgmtxt('L',3.0,0.5,0.5,'Number/Area')
         call pgsch(0.8)
        call pgmtxt('B',3.0,0.5,0.5,'Radius(Mpc)')
          
        call PGPT (nplot,XPT4, YPT4, 2)
       call pgerrb(6,nplot,xpt4,ypt4,sig4,2.0)


c-e04fdf---------------------------------------    
    
c-guess some values--------------------------

         a(1)=100.0d0
         a(2)=0.2d0
         a(3)=60.0d0

c---------------------------------------------

        ifail=-1

	call e04fdf(nplot,mfree,a,fsumsq,iw,liw,w,lw,ifail)
        
     	if(ifail.ne.0) write (6,99999) ifail
	if(ifail.ne.1) then
		write (6,99998) fsumsq
		write (6,99997) (a(j),j=1,mfree)
	endif

       
	call pgsls(2)
	call fitdraw(a,mfree,8)

	stop
  010	stop 'error reading input file, not found'
99997  format(' AT THE POINT',3F12.4)
99998  format(///' ON EXIT, THE SUM OF SQUARES IS',F12.4)
99999  format(///' IFAIL=',I3)

c-shutdown-----------------------------

       endif

       close(4)
       call pgiden
       call pgend
       enddo
    
       END

c-----------------------------------------------
c-subroutines----------------------------------
c--------------------------------------------

	function angle(al,ab,coscent)
c
c	angle between (al,ab) and coscent.........angles in radians
c
	real coscent(3),cosgal(3)
c
	call cosines(al,ab,cosgal)
	dot= cosgal(1)*coscent(1)+cosgal(2)*coscent(2)+cosgal(3)*coscent(3)
	if(dot.gt.1) dot=1.0
	if(dot.lt.-1) dot=-1.0
	angle=acos(dot)
	return
	end

c+++++++++++++++++++++++++++++++++++++
c
	subroutine cosines(al,ab,coscent)
c
	real*4 coscent(3)
	coscent(1)=cos(al)*cos(ab)
	coscent(2)=sin(al)*cos(ab)
	coscent(3)=sin(ab)

	return
	end

c+++++++++++++++++++++++++++++++++++++++++++

      subroutine ratob(ra,dec,glat,glong)
c		all angles in radians
      implicit none
      REAL fact, ra, dec, glat, glong, factor
      REAL a, d, c, t1, t2, s, al
c      fact=atan(1.)/45.
      factor=atan(1.)/45.
      a=(ra-(282.25*factor))
      d=dec
      c=cos(d)
      s=sin(d)
      t1=c*sin(a)
      t2=t1*0.4601998+s*0.88781
      t1=-t1*0.88781+s*0.4601998
c      glat=asin(t1)/fact
      glat=asin(t1)
      t1=c*cos(a)
c      al=atan(t2/t1)/fact
      al=atan2(t2,t1)
c      if(t1.lt.0.) al=180.+al
      glong=al+(33.*factor)
      if(glong.lt.0.) glong=glong+(360.*factor)
	glong=glong

      return
      end

c+++++++++++++++++++++++++++++++++++++++++++++++++++

      FUNCTION select(k,n,arr)
      INTEGER k,n
      REAL select,arr(n)
      INTEGER i,ir,j,l,mid
      REAL a,temp
      l=1
      ir=n
1     if(ir-l.le.1)then
        if(ir-l.eq.1)then
          if(arr(ir).lt.arr(l))then
            temp=arr(l)
            arr(l)=arr(ir)
            arr(ir)=temp
          endif
        endif
        select=arr(k)
        return
      else
        mid=(l+ir)/2
        temp=arr(mid)
        arr(mid)=arr(l+1)
        arr(l+1)=temp
        if(arr(l+1).gt.arr(ir))then
          temp=arr(l+1)
          arr(l+1)=arr(ir)
          arr(ir)=temp
        endif
        if(arr(l).gt.arr(ir))then
          temp=arr(l)
          arr(l)=arr(ir)
          arr(ir)=temp
        endif
        if(arr(l+1).gt.arr(l))then
          temp=arr(l+1)
          arr(l+1)=arr(l)
          arr(l)=temp
        endif
        i=l+1
        j=ir
        a=arr(l)
3       continue
          i=i+1
        if(arr(i).lt.a)goto 3
4       continue
          j=j-1
        if(arr(j).gt.a)goto 4
        if(j.lt.i)goto 5
        temp=arr(i)
        arr(i)=arr(j)
        arr(j)=temp
        goto 3
5       arr(l)=arr(j)
        arr(j)=a
        if(j.ge.k)ir=j-1
        if(j.le.k)l=i
      endif
      goto 1
      END

c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine proper_dist(z,q0,distlum,dlin,angdist)
c
c   Given q_0, find luminosity distance and angular radius for a redshift z
c   in units of km/s
c   angular distance is in *h arcsec if H=100h
c   Somak Raychaudhury IUCAA May 1997
c
      pi=4.0*atan(1.0)
      qfac=q0*z+(1.0-q0)*(1.0-sqrt(1.0+2*q0*z))
      distlum=300000.*qfac/(q0*q0)
      angdist=dlin*((1.0+z)*(1.0+z))/distlum
      angdist=180.*3600.*100.*angdist/pi
      return
      end

c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine precess(ra,dec,date)
c
c     Input: ra, dec in radians (equinox 1950)
c     Output: ra, dec in radians (epoch 'date' in decimal years)
c     Uses constants and formulae from Ast Almanac
c     Warning: input not stored
c
c     Somak Raychaudhury, IoA 1987
c
      rtod = 45./atan(1.)
      rad = ra*rtod
      decd = dec*rtod
      t = (date-1950.)/100.
      tm = t*( 1.2805276 +t*( 0.0003875 +t*.0000100) )
      tn = t*( 0.5567376 +t*( 0.0001183 +t*.0000117) )
      ramd = rad -0.5*( tm +tn*sin(ra)*tan(dec) )
      ram = ramd/rtod
      decmd = decd -0.5*tn*cos(ram)
      decm = decmd/rtod
      rad = rad +tm +tn*sin(ram)*tan(decm)
      decd = decd +tn*cos(ram)
      ra = rad/rtod
      dec = decd/rtod
c
      return
      end

c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

	real*8 function atanh(x)

        real*8 x

        atanh=0.5d0*(dlog((1.d0+x)/(1.d0-x)))

	end 

c++++++++++++++++++++++++++++++++++++++++++++++++++++++

	 subroutine LSFUN1(nplot,mfree,a,fvecc)

         implicit none
         integer npo,nplot,mfree,i
         parameter(npo=10)
         real*8 fvecc(nplot),sig(npo),y(npo),xxx(npo)
         real*8 x(npo),a(mfree),NFW
         common/xanderror/sig,xxx,y
         external NFW

                

c                write(6,*)"calls lsfun1"

c          write(6,*)"a= ",a     
c         write(6,*)"xxx= ",xxx
c           write(6,*)"sig= ",sig
c             write(6,*)"mfree= ",mfree 
c               write(6,*)"nplot= ",nplot 
c                write(6,*)"initial fvecc= ",fvec 
               
c-residuals------------------------              

	do i=1,nplot
        x(i)=xxx(i)/a(2)
        write(6,*)a(1),a(2),a(3)
c        write(6,*)i,xxx(i),a(2),x(i)
     
c           write(6,*)"x(i) in lsfun= ",x(i)

	 fvecc(i)= (y(i)-  NFW(x(i),a,mfree))/sig(i)
c          write(6,*)"loop= ",i,"  fvecc= ",fvecc(i)

     
	enddo

c       write(6,*)"+++++++++++++++++++++++++++++++"

	return
	end

c+++++++++++++++++++++++++++++++++++++++++++++++++++++

      real*8 function NFW(x,a,mfree)
      
      implicit none
      integer mfree,npo,j
      parameter(npo=10)
      real*8 fvecc(npo)
      real*8 x,a(mfree),atanh
                


c-model----------------------------------

              
         If (x.gt.1.d0)then

c            write(6,*)"x into nfw = ",x
c            write(6,*)"a into nfw = ",a

	 NFW= ((((2.0d0*a(1)*a(2))/((x*x)-1.d0))*
     &     (1.0d0 - ((2.0d0/dsqrt((x*x)-1)))
     +     *datan(dsqrt((x-1.d0)/(x+1.d0)))))
     &     +a(3))    

c         write(6,*)"NFW1= ",NFW
            

           else if (x.lt.1.d0) then

c         write(6,*)"x in nfw = ",x
c         write(6,*)"a into nfw = ",a   


         NFW= (((2.d0*a(1)*a(2))/((x*x)-1))*
     &    (1.d0 - ((2.d0/dsqrt(1.d0-(x*x)))
     +    *atanh(dsqrt((1.d0-x)/(1.d0+x)))))
     &    +a(3))      

c            write(6,*)"NFW2= ",NFW

         else

        NFW=0.d0

c         write(6,*)"NFW3= ",NFW

        
            endif

c            write(6,'(''NFW '',5f10.3)')x,(a(j),j=1,3),NFW

	return

	end

c++++++++++++++++++++++++++++++++++++++++++++++++++

 	subroutine fitdraw(a,mfree,ncolour)

        implicit none
        integer kk,npttt,mfree,ncolour
        real absfitmin,absfitmax,range
	parameter(npttt=101)
	real*8 a(mfree),yy,NFW
	real*8 xx(npttt),xxx(npttt),x2
        real*4 fitfun(npttt),XXS(npttt)
	external NFW
        common/rangers/absfitmin,absfitmax
        real*4 fudgefactor
        parameter (fudgefactor=0.1)


c        write(6,*)"a going into fit= ",a
c        write(6,*)"calls fitdraw" 
     
	do kk=1,npttt
	 range=absfitmax-absfitmin

c         write(6,*)"range= ",range

	 xx(kk)= (absfitmin+range*(kk-1)/(npttt-1))
         x2=xx(kk)/a(2)
         
c         write(6,*)"xx= ",xx(kk)

	 yy=NFW(x2,a,mfree)

c         write(6,*)"yy= ",yy

         xxs(kk)=sngl(xx(kk))
c		fitfun(kk)=yy
c		if(yy.gt.0) then
c			fitfun(kk)=alog10(sngl(yy))
			fitfun(kk)=sngl(yy)
c		else
c			fitfun(kk)=-5.
c		endif
c		  nptth=nptth+1

c           write(6,*)"xxs= ",xxs(kk)              
c            write(6,*)"fitfun= ",fitfun(kk) 

                        write(6,*)kk,xxs(kk),fitfun(kk)

	enddo

	 call pgsci(ncolour)
      
	 call pgline(npttt,xxs,fitfun)
	return
	end

c++++++++++++++++++++++++++++++++++++++++++++++++++++
