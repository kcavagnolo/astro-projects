c      fitting program for an XLF using maximum likelihood funxction
c      instead of Chi2 fitting to determine the slope of the X-ray 
c      point sources. 

       program ml_fit
       parameter(N=27,N1=3,N2=10,alpmin=0.1,alpmax=1.5)
       parameter(D=9.08,pc=3.09E16,pi=3.14159,NDIM=2)
       parameter(MAXCLS=100000,LENWRK=500)
       real*8 f(N),ferru(N),ferrd(N),ferrt(N),test,ans
       real*8 logf(N),errlogf(N),norm,aa(NDIM),ba(NDIM)
       real*8 a,b,c,xvar,sigvar,alpvar,L(N2),L0,alp(N2)
       real*8 acc,eps,finest,WRKSTR(LENWRK),NL(N2),NL0
       real*8 fun1,fun2,fun3,FUNCTN,LT(N2)
       real*8 d01baf
       real t1,t2
       external d01baf,d01bax,d01gbf
       external fun1,fun2,fun3,FUNCTN
       integer i,ifail,nstor,MINCLS
       common /vars/ xvar,sigvar,alpvar      
       conv=d*1E6*pc*100
       MINCLS=1000

       open(unit=1,file='xlf_all.txt',status='old')
5      FORMAT(3X,E10.5,1X,E10.5,1X,E10.5)
       do i=1,N
         read(1,5) f(i),ferru(i),ferrd(i)
         ferrt(i)=sqrt(((f(i)-ferrd(i))**2) + ((ferru(i)-f(i))**2))
         logf(i)=log10(f(i))
c         errlogf(i)=ferrt(i)/(f(i)*log(10.))
c         logf(i)=(2*log10(conv)+log10(4*pi))+logf(i)
         f(i)=f(i)*4*pi*conv**2              ! converting
         ferrt(i)=ferrt(i)*4*pi*conv**2      ! into erg/s
c         write(*,*) 'f(i),ferr(t)=',f(i),ferrt(i)
       enddo

       a=0.0e0        
       b=1.0e0
       ifail=1
       nstor=32
       aa(1)=1.0D45    
       do i=1,N
        if (f(i).lt.aa(1)) then      
        aa(1)=f(i)                ! determining lower limit 
        endif                     ! of the first integral.
       enddo
       aa(2)=0.0
       ba(1)=1.0D40
       ba(2)=1.0D40  
       eps=1.0E37  
c       write(*,*) 'aa,bb=',aa(1),aa(2),ba(1),ba(2)
c       setting up 2 nested do-loops. The first do-loop
c       is for various values of alpha (the slope of 
c       the xlf). Effectively, this will be f(alpha) to
c       put into the minimising routine.
     
       do 10 j=1,N2
        alpvar=alpmin+(alpmax-alpmin)*(j-1)/(N2-1)
        alp(j)=alpvar
        L0=0.0e0
        NL0=0.0e0
         do 11 i=1,N
          xvar=f(i)
          sigvar=ferrt(i)
          ans=d01baf(d01bax,a,b,nstor,fun1,ifail) 
c          write(*,*) 'ans=',ans        
          if (ifail.ne.0) then
           write(*,*) 'ifail=',ifail
          endif
          if (ifail.le.1) then
           L(j)=ans+L0
           L0=L(j)
          endif   
          call d01gbf(NDIM,aa,ba,MINCLS,MAXCLS,fun2,eps,
     &    acc,LENWRK,WRKSTR,finest,ifail) 
          if (ifail.ne.0) then
           write(*,*) 'ifail=',ifail
          endif
          if (ifail.eq.0) then
           NL(j)=finest+NL0
           NL0=NL(j)
          endif
           write(*,*) 'i,j,ifail,NL(j)',i,j,ifail,NL(j)
11       continue
c        write(*,*) 'j,L(j),alp(j)=',j,L(j),alp(j) 
c         write(*,*) 'normalisation=',j,NL(j)
c         LT(j)=L(j)/NL(j)
c         write(*,*) LT(j)
10     continue      

c------plot data? (i=1)
       i=0
       if (i.eq.1) then
       call pgbegin(0,'/XW',1,1)
       call pgscf(3)
       call pgslw(3)
       call pgenv(alpmin,alpmax,1.0E-35,1.0E-36,0,0)
       do j=1,N2
       t1=alp(j)
       t2=L(j)
       call pgpt1(t1,t2,9)
c       write(*,*) 'alp,L=',alp(j),L(j)
       enddo
       call pgend      
       endif

       end

       real*8 function fun1(x)
       parameter(pi=3.1415926)
       real*8 x,xvar,sigvar,alpvar
       common /vars/ xvar,sigvar,alpvar
c       write(*,*)'xvar,sigvar,alpvar=',xvar,sigvar,alpvar
       fun1=((sqrt(2*pi)*sigvar)**(-1))*(x**-alpvar)
     & *exp(-((x-xvar)**2)/(2*(sigvar**2)))
       end       
      
       real*8 function fun2(NDIM,X)
       parameter(pi=3.1415926)
       integer NDIM
       real*8 sigvar,alpvar,xvar,X(NDIM)
       common /vars/ sigvar,alpvar,xvar
       fun2=((sqrt(2*pi)*sigvar)**(-1))*(X(2)**-alpvar)
     & *exp(-((X(2)-X(1))**2)/(2*(sigvar**2)))
       end

