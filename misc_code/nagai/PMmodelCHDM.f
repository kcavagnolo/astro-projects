C-----------------------------------------------------
C                                Spectrum of perturbations: P(k), sigma_8,V(50h-1Mpc)  
C        Anatoly Klypin December 1997   
C 	                 <<<  CHDM 2 neutrino model  >>>    Holtzman    appoximation  
C-----------------------------------------------------
C                            Om   = Omega_0                Omc   = Omega_cdm_present
C                            Omb = Omega_baryon     Omnu =Omega_neutrino_present
C                            Ocurv = 0
C                            hsmall = H_0/100km/s/Mpc = the Hubble constant
C                            ns    = slope of the power spectum (ns=1 for Harr-Zeld)
C                            AEXPN = expansion parameter = 1/(1+z), z =redshift
C                            All calculations are done in real Mpc,
C                            Only final outputs are scaled to  h^{-1} 
C-----------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (hsmall=0.5, pi =3.14159265)
      COMMON / TRUNCOM/ Omb,Omc,Omhot,Par(18),AEXPN,Sn,ns
      COMMON / Coeff/   deltac22,deltac,rf,Uklow,Ukup
      DIMENSION  Hms(5)
      DATA        Hms/3.d+10,1.d+11,3.d+11,2.d+12,1.d+15/
      REAL*8      INTG,ns,Nhalos1,Nhalos2,Nhalos3
      Character     Name*16,Answer*1,Header*45,StartFile*16
      REAL*8      INPUT
      EXTERNAL   PS,P,Ptophat,Pgauss,Vtophat,dxi,P2cold
      DATA        StartFile/'InStart.dat'/ 
C                                                     Constants
      write (*,'(" Enter Quadrupole in muK and slope n =",$)')
      read  (*,*) Q2,ns
      RH    =6.e+3/hsmall    ! radius of the horizon
      H     =100.*hsmall     ! Hubble constant
c                 A     =1.2*(pi*Q2*1.d-6/2.726*RH**2)**2 ! normalization for n=1
      Q     =Q2*1.e-6/2.726       ! gives Q2 in DT/T
      A     =Q**2*(4.*pi)/5./fact(ns,2)*RH**(ns+3.) ! = amplitude
      Sn    =A/(2.*pi**2)
      AEXPN =1.
      AEXP  =AEXPN
c                                                                             Cosmological model      
      write (*,*) '================< Enter Output File Name'
      read  (*,'(A)') Name
      write (*,*)     Name
      OPEN(1,file=Name,status='unknown')
      OPEN(2,file='chdm.fit',status='old')
      Call MODEL
      Om      =1.0
      deltac  =INPUT(' Enter delta_c (gaussian filter) =')
      deltac22=deltac**2/2.
c                                             bias parameter      
      rf      =8./hsmall
      Sigma8  = sqrt(Sn*(
     .          INTG(Ptophat,1.d-4,1.d-2)+
     .          INTG(Ptophat,1.d-2,2.d-1)+
     .          INTG(Ptophat,2.d-1,2.d+1)))      
      write (*,10) deltac,Q2,hsmall,Sigma8,Sn,ns,RH
      write (1,10) deltac,Q2,hsmall,Sigma8,Sn,ns,RH
 10   Format(' Press-Schecter approximation for delta_c=',f8.3,/
     .       ' Q2=',f7.3,' Hubble=',f7.3,' Sigma_8 =',G11.3,/
     .       ' normalization constant=',G11.3,' n=',f7.2,
     .       ' Horizon=',G11.3)
      rf =50./hsmall
      Veloc = H*sqrt(Sn*(
     .          INTG(Vtophat,1.d-4,1.d-2)+
     .          INTG(Vtophat,1.d-2,2.d-1)+
     .          INTG(Vtophat,2.d-1,2.d+1)))      
      write (*,30) Veloc,rf*hsmall
      write (1,30) Veloc,rf*hsmall
 30   Format(' Bulk Velocity =',F8.2,' for top-hat radius=',f8.1,
     .        ' Mpc/h')

      write (1,*) ' k/h      Pk*h^3(z=0) Pk(z=30) Power Spectrum'
      DO i=1,110
        z=0.
        AEXPN=1./(1.+z)
        w =1.e-4*10.**(i/20.)
        Pk0  =P(w)*Sn*hsmall**3*(2.*pi**2)
        AEXPN=0.03226
        Pk1  =P(w)*Sn*hsmall**3*(2.*pi**2) ! /AEXPN**2
        write (1,14) w/hsmall,Pk0,Pk1
      EndDo
 14   Format(G11.3,5G12.4)
c                                                     
      write (*,'(" Enter Box(Mpc/h) and N particles =>",$)')
      read  (*,*) Box, NROW
      Box   = Box/hsmall     ! scale to real Mpc
      Uklow =2.*pi/Box
      Ukup  =2.*pi/Box*(NROW/2.)
      write (1,12) Box*hsmall,NROW, Uklow,Ukup
 12   Format(' Box=',f7.1,' Nparticles=',i4,' Klimits=',2f8.3)
         z=30.
         AEXPN=1./(1.+z)
         sigBox =sqrt(Sn*INTG(P2cold,Uklow/sqrt(2.),Ukup))
         write (*,*)' Amplitude of fluctuations in Box: sigma_cold=',
     &                 sigBox
         write (*,76)
         write (1,76)
 76   Format('   z      ',3(2x,'Mass    n(>Mass)',4x),
     .       'All for h=1')
      DO i=0,20
         z       =i/4.
         AEXPN   =1/(1+z)
         HaloM   =Hms(2)
         Nhalos1 =PreSch(HaloM,z,r,hsmall)
         HaloM   =Hms(3)
         Nhalos2 =PreSch(HaloM,z,r,hsmall)
         HaloM   =Hms(4)
         Nhalos3 =PreSch(HaloM,z,r,hsmall)
         WRITE(*,77) z,Hms(2)*hsmall,Nhalos1,
     .                  Hms(3)*hsmall,Nhalos2,
     .                  Hms(4)*hsmall,Nhalos3 
         WRITE(1,77) z,Hms(2)*hsmall,Nhalos1,
     .                  Hms(3)*hsmall,Nhalos2,
     .                  Hms(4)*hsmall,Nhalos3 
 77      Format(f8.2,3(2G11.3))
      ENDDO
C............................................................................................      
      WRITE (*,'(A,$)') ' Do you need a file to run PMstart (Yes/No) '
      READ  (*,'(a)') Answer
      IF(Answer.eq.'Y'.or.Answer.eq.'y')Then ! prepare input file
         OPEN(30,file=StartFile,status='old')        !  parameters of a run
         HEADER='N=128x256 L=20h-1CDMsig=0.239 z=30-----------'
         write (*,*) '------ Enter Header up to 45 characters'
         write (*,*) '       Example:'
         write (*,'(A)')  HEADER 
         read  (*,'(A)')  HEADER
         write (30,'(A)') HEADER
         ASTEP =INPUT(' Step of the expansion parameter  =')
         write (30,*) ASTEP,'    Step in dAEXPN    '
         write (30,*) sigBox,'    DRho/rho in box of CDM '
         write (30,*) ns,   '    Slope of P(k)     '
         write (30,*) Box*hsmall,'    Box in  Mpc/h   '
         Nseed =INPUT(' Enter random seed integer 1-2^31 =')
         write (30,*) Nseed,'    Random seed       '
         Nspecies =INPUT(' Enter number of neutrino/cdm_part =')
         write (30,*) Nspecies,'  number of neutrino/cdm_part'
         write (*,*) ' Results were written to file: ',StartFile
         CLOSE (30)
      ENDIF

 90     STOP
	END

C--------------------------------------------- Press-Schecter n(>M,z)
C               gives N(>M) normalized for h=1.
C               M =HaloM is for hsmall (not scaled to h=1) INPUT
C               r is the filter size (not scaled to h=1)   OUTPUT
C               Upper limit for integral is at the radius
C               where the contribution to N(>M) is 10^-8 of
c               that at the low limit r        
      REAL*8 FUNCTION PreSch(HaloM,z,r,hsmall)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (pi=3.14159265d+0)
      COMMON / TRUNCOM/ Omb,Omc,Omhot,Par(18),AEXPN,Sn,ns
      COMMON / Coeff/   deltac22,deltac,rf,Uklow,Ukup
      REAL*8      INTG
      Real*8 ns
      EXTERNAL PS
         r=6.12d-5*HaloM**0.333333/hsmall**0.6667 ! radius of filter
         AEXPN=1./(1.+z)
c                                       find limits of integration         
         p0 =PS(r)
         rup=r*25.
         ruplim=rup
         ind=0
         Do j=1,30
            If(rup.gt.r)Then
            p1 =PS(rup)
            If(p1.gt.1.d-9*p0.and.ind.eq.0)Then
               ind =1
               ruplim=rup
            EndIf
            rup =rup/1.1
            EndIf
         EndDo
         S =INTG(PS,r,ruplim)
         PreSch =deltac/(2.*pi**2)*S/hsmall**3      
      RETURN
      END
C--------------------------------------------- Contribution to PS
      REAL*8 FUNCTION PS(r)
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON / TRUNCOM/ Omb,Omc,Omhot,Par(18),AEXPN,Sn,ns
      COMMON / Coeff/   deltac22,deltac,rf,Uklow,Ukup
      REAL*8              INTG1, ns 
      EXTERNAL P4gauss,Pgauss
         rf     =r
         Uplimit=MIN(6.0d0/rf,40.0D+0) !  upper limit for integrals
         Uplimit=MIN(Uplimit,Ukup)
         sigma2 =Sn*INTG1(Pgauss,Uklow,Uplimit)
         sigma  =sqrt(sigma2)
         epsilon=Sn*INTG1(P4gauss,Uklow,Uplimit)/sigma2
         PS=epsilon*EXP(-deltac22/sigma2)/sigma/rf**2
      RETURN
      END
c-----------------------------------  Pcold(k)*k^2
      REAL*8 FUNCTION P2cold(WK)
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON / TRUNCOM/ Omb,Omc,Omhot,Par(18),AEXPN,Sn,ns
      COMMON / Coeff/   deltac22,deltac,rf,Uklow,Ukup
      Real*8 ns
        P2cold=WK**2*Pcold(WK)*AEXPN**2
      RETURN
      END
c-----------------------------------  P(k)*k^2*gauss(rf)
      REAL*8 FUNCTION Pgauss(WK)
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON / TRUNCOM/ Omb,Omc,Omhot,Par(18),AEXPN,Sn,ns
      COMMON / Coeff/   deltac22,deltac,rf,Uklow,Ukup
      Real*8               ns
        Pgauss=WK**2*P(WK)*EXP(-(rf*WK)**2)
      RETURN
      END
c-----------------------------------  P(k)*k^4*gauss(rf)
      REAL*8 FUNCTION P4gauss(WK)
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON / TRUNCOM/ Omb,Omc,Omhot,Par(18),AEXPN,Sn,ns
      COMMON / Coeff/   deltac22,deltac,rf,Uklow,Ukup
      Real*8               ns
        P4gauss=WK**4*P(WK)*EXP(-(rf*WK)**2)
      RETURN
      END
C-------------------------------------- P*k^2*Top-Hat Filter
      REAL*8 FUNCTION Ptophat(wk)
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON / TRUNCOM/ Omb,Omc,Omhot,Par(18),AEXPN,Sn,ns
      COMMON / Coeff/   deltac22,deltac,rf,Uklow,Ukup
      Real*8               ns
	     IF (wk.lt.1.d-4) THEN
            Ptophat =wk**ns*wk**2
        ELSE
            X=wk*rf
	         TOPHAT=( (SIN(X)-x*COS(X))*3./X**3 )**2
            Ptophat=P(wk)*wk**2*TOPHAT
        ENDIF
      RETURN
      END
C-------------------------------------- P*Top-Hat Filter
      REAL*8 FUNCTION Vtophat(wk)
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON / TRUNCOM/ Omb,Omc,Omhot,Par(18),AEXPN,Sn,ns
      COMMON / Coeff/   deltac22,deltac,rf,Uklow,Ukup
      Real*8               ns
	     IF (wk.lt.1.d-4) THEN
            Vtophat =wk**ns
        ELSE
            X=wk*rf
	         TOPHAT=( (SIN(X)-x*COS(X))*3./X**3 )**2
            Vtophat=P(wk)*TOPHAT
        ENDIF
      RETURN
      END

C________________________________________Read parameters of the model
      SUBROUTINE MODEL
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON / TRUNCOM/ Omb,Omcold,Omhot,Par(18),AEXPN,Sn,ns
      DIMENSION          ParC(11),ParH(7)
      Real*8               ns
      character*80          a
      Equivalence (ParC(1),Par(1)),(ParH(1),Par(12))

      Do i=1,1                   ! read the header
         Read (2,'(A)') a
      EndDo
      Read (2,*) Om,Omb,Omc,Omnu,h,ParC,ParH
      Omhot  =Omnu
      Omcold =1. -Omhot
      Write (*,20)Omb,Omc,Omnu,h,Par
      Write (1,20)Omb,Omc,Omnu,h,Par
 20   Format(' Model: O_b=',F5.2,' O_cold=',F5.2,' O_nu=',F5.2,
     .       ' h=',F5.2,/(7G11.3))
      Return
      End
C--------------------------:  Power spectrum CHDM cold + hot
      REAL*8 FUNCTION P(wk)
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON / TRUNCOM/ Omb,Omc,Omhot,Par(18),AEXPN,Sn,ns
      Real*8               ns
         P =(AEXPN*(Omc*Sqrt(Pcold(wk)) +Omhot*Sqrt(Phot(wk))))**2         
      Return
      End
C--------------------------:  Power spectrum CHDM cold
      REAL*8 FUNCTION Pcold(wk)
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON / TRUNCOM/ Omb,Omc,Omhot,Par(18),AEXPN,Sn,ns
      Real*8               ns
      DIMENSION          PP(6)
      DATA   PP/1.,-1.464,19.81,154.5,143.6,0.961/

      If(AEXPN.GT.0.035)THEN
       x =AEXPN**Par(10)
       sk=wk**0.33333 
       Pcold = LOG(1.+Par(1)*x*wk) / (Par(1)*x) /
     .         (1.+sk*(Par(18)
     .            +sk*(Par(2)*(1.+Par(3)*x)
     .            +sk*(Par(4)*(1.+Par(5)*x)
     .            +sk*(Par(6)*(1.+Par(7)*x)
     .            +sk*(Par(8)*(1.+Par(9)*x**2) ))))) )**2
      Else                      ! z=30
         sk =sqrt(wk)
         Pcold=wk/
     .        (1.+sk*(PP(2) +sk*(PP(3) +sk*(PP(4)
     .           +sk*PP(5) ))))**(2.*PP(6))
      EndIf
       Pcold =Pcold*wk**(ns-1.)
      RETURN
      END
C----------------------Neutrino:   Cold * correction factor=hot
      REAL*8 FUNCTION Phot(WK)
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON / TRUNCOM/ Omb,Omc,Omhot,Par(18),AEXPN,Sn,ns
      Real*8               ns
      DIMENSION          PP(6)
      DATA PP/1.442,-0.651,4.03,1.553,-35.50,141.7/

      If(AEXPN.GT.0.035)THEN
         sk =sqrt(wk)
        y  =(wk*AEXPN**Par(17))**0.3333
      Phot =  Pcold(WK)  * 
     .                 exp(-Par(12)*y**2)
     .                /(1.+   y*(Par(13)+y*
     .                          (Par(14)+y*
     .                          (Par(15)+y*Par(16) ))))
      Else                      ! z=30
           sk =(wk)**0.333
         Phot =  Pcold(wk)  * 
     .                    exp(-PP(1)*wk)
     .                   /(1.+sk*(PP(2) +sk*(PP(3)
     .                       +sk*(PP(4) +sk*(PP(5) 
     .                       +sk*PP(6)    )))))
      EndIf
      Phot =ABS(Phot)
      RETURN
      END
C-------------------------------------------- Simpson integration
      REAL*8 FUNCTION INTG(FUNC,A,B)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (EPS=2.0d-4, JMAX=20)
      EXTERNAL FUNC
      OST=-1.E30
      OS= -1.E30
      ST =0.
      DO 11 J=1,JMAX
        CALL TRAPZD(FUNC,A,B,ST,J)
        INTG=(4.0d0*ST-OST)/3.0d0
        IF (ABS(INTG-OS).Le.EPS*ABS(OS).and.J.GT.6) RETURN
        OS=INTG
        OST=ST
11    CONTINUE
      WRITE (1,*)'Integration did not converge'
      RETURN
      END
C----------------------------------------------
      SUBROUTINE TRAPZD(FUNCC,A,B,S,N)
	   IMPLICIT REAL*8 (A-H,O-Z)
        SAVE IT
        EXTERNAL FUNCC
      IF (N.EQ.1) THEN
        S=0.5d0*(B-A)*(FUNCC(A)+FUNCC(B))
        IT=1
      ELSE
        TNM=IT
        DEL=(B-A)/TNM
        X=A+0.5D0*DEL
        SUM=0.0D0
        DO 11 J=1,IT
          SUM=SUM+FUNCC(X)
          X=X+DEL
11      CONTINUE
        S=0.5D0*(S+(B-A)*SUM/TNM)
        IT=2*IT
      ENDIF
      RETURN
      END

C-------------------------------------------- Simpson integration
      REAL*8 FUNCTION INTG1(FUNC,A,B)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (EPS=2.0d-4, JMAX=17)
      OST=-1.d30
      OS= -1.d30
      ST =0.
      DO 11 J=1,JMAX
        CALL TRAPZD1(FUNC,A,B,ST,J)
        INTG1=(4.0D0*ST-OST)/3.0D0
        IF (ABS(INTG1-OS).Le.EPS*ABS(OS)) RETURN
        OS=INTG1
        OST=ST
11    CONTINUE
      WRITE (1,*)'Integration did not converge'
      RETURN
      END
C----------------------------------------------
      SUBROUTINE TRAPZD1(FUNCC,A,B,S,N)
	   IMPLICIT Real*8 (A-H,O-Z)
        SAVE IT
        EXTERNAL FUNCC
      IF (N.EQ.1) THEN
        S=0.5D0*(B-A)*(FUNCC(A)+FUNCC(B))
        IT=1
      ELSE
        TNM=IT
        DEL=(B-A)/TNM
        X=A+0.5D0*DEL
        SUM=0.0D0
        DO 11 J=1,IT
          SUM=SUM+FUNCC(X)
          X=X+DEL
11      CONTINUE
        S=0.5D0*(S+(B-A)*SUM/TNM)
        IT=2*IT
      ENDIF
      RETURN
      END

C------------------------------  Gamma Function
c                  two expansions: |x|<1 and |x|>1
c                  for x<0 : G(x)G(-x)=-pi/(xsin(pi*x))
      
      REAL*8 Function gamma(x)
      IMPLICIT REAL*8 (A-H,O-Z)
      DATA pi,c1,c3,c5,c7,c9/3.14159265d+0,0.422784335d+0,
     .        0.067352301d+0,7.385551d-3,1.192754d-3,2.23155d-4/ 

      If(x.lt.0.)Then
         isign =-1
         x= -x
      Else
         isign =1
      EndIf
      z=x-1.
        If(z.lt.0.9999d+0)Then
           z2=z*z
           pz=pi*z
           If(abs(z).lt.1.d-4)Then
              p2 =pz*pz
              ratio=1./(1.-p2/6.*(1.-p2/20.*(1.-p2/42.)))
           Else
              ratio=pz/sin(pz)
           EndIf
           gamma= sqrt(ratio*(1.-z)/(1.+z))*
     .            exp(z*(c1-z2*(c3+z2*(c5+z2*(c7+z2*c9)))))
        Else
           z2 =x*x
           gamma=sqrt(2.*pi/x)/exp(x)*(x**x)*
     .      exp((1.-
     .        (1./30.-
     .          (1./105.-
     .            (1./140.-(1./99.-(691./30030.-1./13./4./z2)/z2)/z2)
     .            /z2)
     .          /z2)
     .        /z2)/x/12.)
        EndIf
        If(isign.eq.-1) Then
           gamma =-pi/(x*sin(pi*x)*gamma)
           x=-x
        EndIf
        
        Return
        End
C------------------------------------            exact factor
      REAL*8 Function fact(n,l)
      IMPLICIT REAL*8 (A-H,O-Z)
      Real*8 n
         eps =(n-1.)/2.
         fact =gamma(3.-n)*gamma(l+eps)/
     .          (2.**(1.-n)*gamma(1.5-eps)**2*gamma(l+2-eps))
      Return
      End
C---------------------------------     Delt^2/k*sin(kr)/kr
      REAL*8 FUNCTION dxi(x)
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON / Coeff/   deltac22,deltac,rf,Uklow,Ukup
       dxi =x**2*P(x)*sin(x*rf)/(x*rf)
      Return
      End
C---------------------------------- Read in variables      
      REAL*8 FUNCTION INPUT(text)
C-----------------------------------------
      Character text*(*)
          write (*,'(A,$)')text
          read (*,*) x
          INPUT =x
      Return
      End
