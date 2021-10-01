C---------------------------------------------------
C                           Spectrum of perturbations: P(k), sigma_8,V(50h-1Mpc)
C     Anatoly Klypin December 1997
C                                                       Holtzman           appoximation  
C                                             or       BBKS-style + Hu&Sugiyama approx
C                            Needs cdm.fit file -- parameters of models and fitting 
C-----------------------------------------------------
C                            Om   = Omega_0                Omc   = Omega_cdm_present
C                            Omb = Omega_baryon     Omnu =Omega_neutrino_present
C                            Ocurv = Omega_curvature_present
C                            hsmall = H_0/100km/s/Mpc = the Hubble constant
C                            ns    = slope of the power spectum (ns=1 for Harr-Zeld)
C                            AEXPN = expansion parameter = 1/(1+z), z =redshift
C                            All calculations are done in real Mpc,
C                            Only final outputs are scaled to  h^{-1} 
C-----------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON / TRUNCOM/ Om,Omb,Omc,Omnu,Ocurv,Par(6),AEXPN,Sn,ns,hsmall
      COMMON / Coeff/   deltac22,deltac,rf,Uklow,Ukup
      CHARACTER         Name*16,Answer*1,Header*45,StartFile*16
      REAL*8              INTG,ns,INPUT,P2,P,Ptophat,Pgauss,Vtophat
      EXTERNAL           INTG,P2,P,Ptophat,Pgauss,Vtophat
      DATA                StartFile/'InStart.dat'/
      WRITE (*,*) '================< Enter File Name for output file: '
      READ  (*,'(a)') Name
      OPEN(1,file=Name)

      OPEN(2,file='cdm.fit') !  parameters of models
C                                                     Constants
      PI     =3.1415926535
      AEXPN =1.
      WRITE (*,'(A,$)') ' Enter sigma_8 and slope n: '
      READ  (*,*) Sigma8,ns
      CALL MODEL(Line)
      H      =100.*hsmall     ! Hubble constant
      DO a=0.1,1.,0.1
         CALL AGE(t0,GrowthDen,GrowthVel,a)
      ENDDO
      AEXPN =1.
      a =AEXPN
      write (*,*) ' a=',a,' Aexpn=',AEXPN
      CALL AGE(t0,GrowthDen_0,GrowthVel_0,a)
      rf    =8./hsmall        ! top-hat for sigma_8
      Sn    = (Sigma8)**2 / 
     &          INTG(Ptophat,1.d-5,2.d+1) ! normalization of P(k)
c                                             bias parameter      
      Sigma8 = sqrt(Sn*(
     &          INTG(Ptophat,1.d-5,2.d+1)))
      write (*,10) hsmall,Sigma8,ns  ! check if all is self-consistent
      write (1,10) hsmall,Sigma8,ns
 10   Format(' Hubble=',f7.3,' Sigma_8 =',G11.3,
     &       ' Slope n=',f7.2)
      rf =50./hsmall           ! top-hat for bulk velocity
      Veloc = GrowthVel*H*
     &           sqrt(Sn*(INTG(Vtophat,1.d-5,1.d+0)))      
      write (*,30) Veloc,rf*hsmall
      write (1,30) Veloc,rf*hsmall
 30   Format(' Bulk Velocity =',F8.2,
     .       'km/s for radius top-hat=',f8.1,'h-1Mpc')
      write (1,*) ' k/h        Pk*h^3   Power Spectrum at z=',1/AEXPN-1.
      DO i=1,110
        w =1.e-4*10.**(i/20.)
        Pk0  =P(w)*Sn*hsmall**3*(2.*pi**2) 
        write (1,14) w/hsmall,Pk0
      ENDDO
 14   Format(2G11.4,5G12.4)
C..........................................................Power spectrum and  
C                                                         Amplitude of fluctuations in a Box at redshift z
      WRITE (*,'(A,$)')
     &         'Enter Box(h^-1Mpc), Nparticles(1D) and redshift:'
      READ  (*,*) Box, NROW, z
      Box   =Box/hsmall           ! scale it to real Mpc
      Uklow =2.*pi/Box            ! frequency range for integrals
      Ukup  =2.*pi/Box*(NROW/2.)
      AEXPN =1./(1.+z)            ! expansion parameter
      a     =AEXPN
      CALL AGE(t0,GrowthDen,GrowthVel,a)
      sigma =(GrowthDen/GrowthDen_0)*
     &             sqrt(Sn*INTG(P2,Uklow/sqrt(2.),Ukup))
      WRITE (*,40) z,sigma,NROW,Box*hsmall
 40   format('  z=',f8.3,' delta\rho/rho in box=',f9.5,/
     .       5X,'Particles=',i4,' Box=',f8.2)
      write (1,*) ' k/h        Pk*h^3   Power Spectrum at z=',1/AEXPN-1.
      DO i=1,130
        w =1.e-4*10.**(i/20.)
        Pk0  =(GrowthDen/GrowthDen_0)**2*P(w)*Sn*hsmall**3*(2.*pi**2) 
        write (1,14) w/hsmall,Pk0
      ENDDO
C............................................................................................      
      WRITE (*,'(A,$)') ' Do you need a file to run PMstart (Yes/No) '
      READ  (*,'(a)') Answer
      IF(Answer.eq.'Y'.or.Answer.eq.'y')Then ! prepare input file
         OPEN(30,file=StartFile)        !  parameters of a run
         write(30,*) 'Yes           Read parameters from cdm.fit'
         HEADER='N=128x256 L=20h-1CDMsig=0.239 z=30-----------'
         write (*,*) '------ Enter Header up to 45 characters'
         write (*,*) '       Example:'
         write (*,'(A)') HEADER 
         read  (*,'(A)') HEADER
         write (30,'(A)') HEADER
         write (30,*) AEXPN,'    Expansion Parameter'
         ASTEP =INPUT(' Step of the expansion parameter  =')
         write (30,*) ASTEP,'    Step in dAEXPN    '
         write (30,*) sigma,'    DRho/rho in box   '
         write (30,*) ns,   '    Slope of P(k)     '
         write (30,*) Box*hsmall,'    Box in  Mpc/h   '
         write (30,*) Ocurv,'    Omega_curvature   '
         Nseed =INPUT(' Enter random seed integer 1-2^31 =')
         write (30,*) Nseed,'    Random seed       '
         write (30,*) Line,'     Line number in cdm.fit'
         write (30,*) 0
         write (*,*) ' Results were written to file: ',StartFile
         CLOSE (30)
      ENDIF
      STOP
	   END
c-----------------------------------  Pcold(k)*k^2
      REAL*8 FUNCTION P2(WK)
C---------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON / TRUNCOM/ Om,Omb,Omc,Omnu,Ocurv,Par(6),AEXPN,Sn,ns,hsmall
      COMMON / Coeff/   deltac22,deltac,rf,Uklow,Ukup
      Real*8 ns
        P2=WK**2*P(WK)
      RETURN
      END

C---------------------------------------------
C                                                                 Power spectrum
C                                                                wk = k= in real Mpc
      REAL*8 FUNCTION P(wk)
C---------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON / TRUNCOM/ Om,Omb,Omc,Omnu,Ocurv,Par(6),AEXPN,Sn,ns,hsmall
      COMMON / FACT/    qqscaleb
      Real*8 ns
      If(Par(6).ne.0.)Then   ! Holtzman approx
        sk= sqrt(wk)
        P = wk**ns /
     .         (1.+sk*(Par(2)
     .            +sk*(Par(3)
     .            +sk*(Par(4)
     .            +sk*(Par(5) )))) )**(2.*Par(6))
      Else                   ! BBKS + Sugiyama approx
c        Gammaeff =Om*hsmall/exp(Omb*(1.+sqrt(hsmall/0.5)/Om))
c        Q = wk/hsmall/Gammaeff
        Q = wk*qqscaleb
        P = wk**ns*( LOG(1.+Par(1)*Q)/(Par(1)*Q) )**2/
     .          sqrt(1.+Q*(Par(2)+
     .                  Q*(Par(3)**2+
     .                  Q*(Par(4)**3+
     .                  Q*(Par(5)**4) ))))
      EndIf
      RETURN
      END
C-------------------------------------------------
C                                  Age of the Universe: t0 (z=0)
C                                  Expansion parameter: a =1/(1+z)
C                                  Growth_Rate_Density at a: GrowthDen
C                                     GrowthDen =\delta\rho/\rho
C                                     normalized to GrowthDen =a for Omega=1
C                                     GrowthDen < 1 at a=1 for Lcdm or OCDM
C                                  Growth_Rate_velocity at a: GrowthVel
C                                     GrowthVel = V_peculiar= (a/delta)(d delta/d a)
      SUBROUTINE AGE(t0,GrowthDen,GrowthVel,a)
C---------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (zero =1.d-12)
      COMMON / TRUNCOM/ Om,Omb,Omc,Omnu,Ocurv,Par(6),AEXPN,Sn,ns,hsmall
      Common /OmegRat/ OmLOm0,OmcOm0
      Real*8  INTG,ns
      External Hnorm,Hage,Hgrow

      Oml    =Max(Abs(1.-Om-Ocurv),zero)
      OmLOm0 =Oml/Om
      OmcOm0 =Ocurv/Om
      t0     =9.766/hsmall/sqrt(Om)*INTG(Hage,zero,a)
      z      =1./a-1.
      Hubble = hsmall*sqrt(Om/a**3)*Hnorm(a)
         ww  =INTG(Hgrow,zero,a)
      GrowthVel = -(1.5+OmcOm0*a)/Hnorm(a)**2+
     &                 sqrt(a**5)/Hnorm(a)**3/ww
      GrowthDen =2.5*Hnorm(a)/sqrt(a**3)*ww
         write (*,20) t0,z,a,Hubble,GrowthDen,GrowthVel
         write (1,20) t0,z,a,Hubble,GrowthDen,GrowthVel
 20      format(' Age=',f8.3,' z=',f6.2,' a=',f6.3,
     .          ' Hubble/100=',f8.3,' GrowthRateDen=',f7.4,
     .          ' GrowthRateVelocity=',f7.4)
      Return
      End
c-----------------------------------  P(k)*k^2*gauss(rf)
      REAL*8 FUNCTION Pgauss(WK)
C---------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON / TRUNCOM/ Om,Omb,Omc,Omnu,Ocurv,Par(6),AEXPN,Sn,ns,hsmall
      COMMON / Coeff/   deltac22,deltac,rf,Uklow,Ukup
      Real*8 ns
        Pgauss=WK**2*P(WK)*EXP(-(rf*WK)**2)
      RETURN
      END
c-----------------------------------  P(k)*k^4*gauss(rf)
      REAL*8 FUNCTION P4gauss(WK)
C---------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON / TRUNCOM/ Om,Omb,Omc,Omnu,Ocurv,Par(6),AEXPN,Sn,ns,hsmall
      COMMON / Coeff/   deltac22,deltac,rf,Uklow,Ukup
      Real*8 ns
        P4gauss=WK**4*P(WK)*EXP(-(rf*WK)**2)
      RETURN
      END
C-------------------------------------- P*k^2*Top-Hat Filter
      REAL*8 FUNCTION Ptophat(wk)
C---------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON / TRUNCOM/ Om,Omb,Omc,Omnu,Ocurv,Par(6),AEXPN,Sn,ns,hsmall
      COMMON / Coeff/   deltac22,deltac,rf,Uklow,Ukup
      Real*8 ns
	   IF (wk.lt.1.d-4) THEN
            Ptophat =wk**ns*wk**2
        ELSE
            X      =wk*rf
	         TOPHAT =( (SIN(X)-x*COS(X))*3./X**3 )**2
            Ptophat=P(wk)*wk**2*TOPHAT
        ENDIF
      RETURN
      END
C-------------------------------------- P*Top-Hat Filter
      REAL*8 FUNCTION Vtophat(wk)
C---------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON / TRUNCOM/ Om,Omb,Omc,Omnu,Ocurv,Par(6),AEXPN,Sn,ns,hsmall
      COMMON / Coeff/   deltac22,deltac,rf,Uklow,Ukup
      Real*8 ns
   	IF (wk.lt.1.d-4) THEN
            Vtophat =wk**ns
        ELSE
            X=wk*rf
	    TOPHAT=( (SIN(X)-x*COS(X))*3./X**3 )**2
            Vtophat=P(wk)*TOPHAT
        ENDIF
      RETURN
      END
C-------------------------------------- Linear Pair-wise V
      REAL*8 FUNCTION Vpairwise(wk)
C---------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON / TRUNCOM/ Om,Omb,Omc,Omnu,Ocurv,Par(6),AEXPN,Sn,ns,hsmall
      COMMON / Coeff/   deltac22,deltac,rf,Uklow,Ukup
      Real*8 ns
            X        =wk*rf
            Vpairwise=P(wk)*(1.-sin(X)/X)
      RETURN
      END
C________________________________________Read parameters of the model
C                                 Om       = Omega_0
C                                 Omb     = Omega_baryon
C                                 Omnu  = Omega_neutrino
C                                 hsmall  = hubble constant
C                                 Par      = fitting parameters
C                                 Par(6) = 0  --- bbks-style+Hu&Sugiyama
C                                 Par(6) ne 0 --- holtzman-style
      SUBROUTINE MODEL(Line)
C---------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON / TRUNCOM/ Om,Omb,Omc,Omnu,Ocurv,Par(6),AEXPN,Sn,ns,hsmall
      COMMON / FACT/    qqscaleb
      Real*8 ns,INPUT
      Character             Header*79      

      Line =INPUT(' Enter Line Number in  cdm.fit=   ')
      Read(2,'(A)') Header
      If(Line.gt.2)Then
         Do i=1,Line -2
            Read (2,*) a
         EndDo
      EndIf
      Read (2,*) Om,Omb,Omc,Omnu,hsmall,Par
       
      Ocurv =INPUT(' Enter Omega curvature at z=0  =>')
      theta = 2.726/2.7  ! = T_cmb/2.7
      Ob0   =Omb/Om      ! Hu&Sugiyama fitting
      Omh2  =Om*hsmall**2
      a1    =(46.9*Omh2)**0.670*(1.+(32.1*Omh2)**(-0.532))
      a2    =(12.0*Omh2)**0.424*(1.+(45.*Omh2)**(-0.582))
      alpha =1./a1**Ob0/a2**(Ob0**3)
      qqscaleb = theta**2/(1.-Ob0)**0.60/sqrt(alpha)/Omh2

      Write (*,20)Om,Omb,Omc,Omnu,hsmall,Par
      Write (1,20)Om,Omb,Omc,Omnu,hsmall,Par
 20   Format(' Model: Om_0=',F5.3,' Om_baryon=',F5.3,
     .       ' Om_cold=',F5.3,' O_nu=',F5.3,
     .       ' hsmall=',F4.2,/8x,'Parameters=',(6G9.3))
      Return
      End
C-------------------------------------------- Simpson integration
      REAL*8 FUNCTION INTG(FUNC,A,B)
C---------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (EPS=5.0d-4, JMAX=22)
      EXTERNAL FUNC
      OST=-1.D30
      OS= -1.D30
      ST =0.
      DO 11 J=1,JMAX
        CALL TRAPZD(FUNC,A,B,ST,J)
        INTG=(4.0d0*ST-OST)/3.0d0
        IF (ABS(INTG-OS).Le.EPS*ABS(OS)) RETURN
        OS=INTG
        OST=ST
11    CONTINUE
      WRITE (*,*)'Integration did not converge'
      RETURN
      END
C----------------------------------------------
      SUBROUTINE TRAPZD(FUNCC,A,B,S,N)
C---------------------------------------
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
C---------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (EPS=1.0d-3, JMAX=22)
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
c      WRITE (1,*)'Integration did not converge'
      RETURN
      END
C----------------------------------------------
      SUBROUTINE TRAPZD1(FUNCC,A,B,S,N)
C---------------------------------------
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
C---------------------------------- Read in variables      
      REAL*8 FUNCTION INPUT(text)
C-----------------------------------------
      Character text*(*)
          write (*,'(A,$)')text
          read (*,*) x
          INPUT =x
      Return
      End
C-----------------------------------------
      REAL*8 FUNCTION sinhh(x)
C-----------------------------------------
      IMPLICIT REAL*8 (A-H,P-Z)
         sinhh =0.5*(exp(MIN(x,33.d+0))-exp(-MIN(x,33.d+0)))
      RETURN
      END
C---------------------------------------
      REAL*8 FUNCTION Hh(x)
C---------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
         Hh =(sqrt(x/(1.d+0 +x**3)))**3 
      Return
      End
C---------------------------------------
      REAL*8 FUNCTION Hnorm(x)
C---------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      Common /OmegRat/ OmLOm0,OmcOm0
         Hnorm =sqrt(1.d+0 
     &                  +OmLOm0*x**3
     &                  +OmcOm0*x )
      Return
      End
C---------------------------------------
      REAL*8 FUNCTION Hage(x)
C---------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
         Hage =sqrt(x)/Hnorm(x) 
      Return
      End
C---------------------------------------
      REAL*8 FUNCTION Hgrow(x)
C---------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
         Hgrow =(sqrt(x)/Hnorm(x))**3 
      Return
      End
