C _______________________ START 3-D SIMULATIONS
C
C                         Klypin, August 1993
C
      INCLUDE 'PMparameters.h'
C		   NROW = number of particles in 1 dimension
C		   NGRID= number of cells        in 1 dimension
C		   Npage =  number of particles per page = NROW**2
C
      COMMON / GRID/	GRX(NROW,NROW,NROW),GRY(NROW,NROW,NROW),
     +			GRZ(NROW,NROW,NROW)
      COMMON / GRIV/	GVX(NROW,NROW,NROW),GVY(NROW,NROW,NROW),
     +			GVZ(NROW,NROW,NROW)
      COMMON / ROW2/	XPAR2(NPAGE),YPAR2(NPAGE),ZPAR2(NPAGE),
     +			VX2(NPAGE),VY2(NPAGE),VZ2(NPAGE)
      COMMON / TRUNCOM/ Om,Omb,Omc,Omnu,Par(6),ns,qqscaleb,QSCALE
      COMMON / KINENRG/ SKINE,SX,SY,SZ,SX2,SY2,SZ2
      Character  Hd*5,Tail*4,Nm*40
      DATA PI		/3.1415926535/

      Hd  ='PMcrs'
      Tail='.DAT'
     
      CALL InitValues(NBYTE,SCALEL)
      write (*,*) ' InitValues is done. N bytes=',NBYTE

      OPEN(UNIT=9,FILE='PMcrd.DAT',form='unformatted',status='unknown')
      write (9)             ! this clears old header file
      CLOSE (9)
      CALL RDTAPE                  ! this opens files on disk
      OPEN(UNIT=16,FILE='RESNEW.DAT',status='unknown')
      write (16,*) ' Seed=',Nseed
       AEXP0 = AEXPN
       AEXPV = AEXPN - ASTEP/2.
C			                   get a realization of spectrum in /GRID/
       NRAND =Nseed
       CALL SPECTR(ALPHA,NRAND)
C			                        get the displacement vector by FFT
       IFOUR = INT(ALOG(FLOAT(NROW))/ALOG(2.)+0.5)
       Write (*,*) ' Ifour =',IFOUR,' Nrow=',NROW
       CALL VECTOR(IFOUR)
	    write (*,*) ' SPECTR is done. IFOUR = ',IFOUR,' Alpha=',ALPHA
        Fact =  sqrt(Om0+Oml0*AEXPV**3+Ocurv*AEXPV)
       VCONS = -ALPHA/(2.*PI/NGRID)*(AEXPV/AEXP0)*SQRT(AEXPV)*Fact
       XCONS =	 ALPHA/(2.*PI/NGRID)*(AEXPN/AEXP0)
       QFACT =  FLOAT(NGRID)/FLOAT(NROW)
                      SKINE = 0.
                      SX    = 0.
                      SY    = 0.
                      SZ    = 0.
                      SX2   = 0.
                      SY2   = 0.
                      SZ2   = 0.

C			           find x,v and write to file, page by page
      DO  KROW = 1, NROW
      	CALL SETXV(XCONS,VCONS,KROW)
	      CALL WRIROW(KROW,1)
      ENDDO
C                                     EKIN is kinet.energy at A(i-1/2)
      EKIN = 0.5*SKINE/AEXPV**2*PARTW
      DP   = NROW**3
      SX   = SX/DP
      SY   = SY/DP
      SZ   = SZ/DP
      SX2  = SQRT(SX2/DP)
      SY2  = SQRT(SY2/DP)
      SZ2  = SQRT(SZ2/DP)
      WRITE (*,'('' Displacement of a particle: MEAN'',3F9.4)')
     .	     SX,SY,SZ
      WRITE (*,'('' in cell units               RMS '',3F9.4)')
     .	     SX2,SY2,SZ2
      WRITE (16,'('' Displacement of a particle: RMS'',3F9.4)')
     .	     SX2,SY2,SZ2
      Veloc=sqrt( SKINE/AEXPV**2/NROW**3)*SCALEL/NGRID*
     .        100.*hubble
      WRITE (*,'('' Ekin='',E12.4,'' Veloc_cold(km/s)='',F8.2)')
     .       EKIN,Veloc
      WRITE (16,'('' Ekin='',E12.4,'' Veloc_cold(km/s)='',F8.2)')
     .       EKIN,Veloc
C			       write header and control data -------
      CALL WRTAPE

      STOP
      END
C--------------------------------------------------
C                                                     sqrt(Power spectrum)
C                                                           k = (2pi/L) wk
      REAL  FUNCTION TRUNF(wk)
C--------------------------------------------------
      INCLUDE 'PMparameters.h'
      PARAMETER (NSPEC =NROW/2)
      COMMON / TRUNCOM/ Om,Omb,Omc,Omnu,Par(6),ns,qqscaleb,QSCALE
      Real                 ns,k
      IF (WK.GE.FLOAT(NSPEC)) THEN
	       TRUNF =0.
	       RETURN
      ENDIF
      If(Par(6).ne.0.)Then   ! Holtzman approx
        k = QSCALE*wk
        sk= sqrt(k)
        TRUNF = k**(ns/2.) /
     .         (1.+sk*(Par(2)
     .            +sk*(Par(3)
     .            +sk*(Par(4)
     .            +sk*(Par(5) )))) )**(Par(6))
      Else                   ! BBKS + Sugiyama approx
c        Gammaeff =Om*hsmall/exp(Omb*(1.+sqrt(hsmall/0.5)/Om))
c        Q = wk/hsmall/Gammaeff
        k = QSCALE*wk
        Q = k*qqscaleb
        TRUNF = k**(ns/2.)* LOG(1.+Par(1)*Q)/(Par(1)*Q)/
     .          sqrt(sqrt(1.+Q*(Par(2)+
     .                  Q*(Par(3)**2+
     .                  Q*(Par(4)**3+
     .                  Q*(Par(5)**4) )))))
      EndIf
      RETURN
      END
C*********************************************************************
C	   Power spectrum for CDM. Wk-  wavenumber(Mpc^-1, h=0.5)
C                         P = wk *T^2(k)
C                         QSCALE = 2pi/Box_size,  QS =hubble**(-2)
c      FUNCTION TRUNF(WK)
c      COMMON / TRUNCOM/ QSCALE,g,gy
c      COMMON / TRUNCDM/ QS,A1,A2,A3,A4,A5,A32,A43,A54
c       Q = QSCALE*QS*WK
c       TRUNF = sqrt( WK*
c     +		 (LOG(1.+A1*Q)/(A1*Q))**2/
c     +		 SQRT(1.+Q*(A2 +Q*(A32   +Q*(A43+Q*A54)))) )
c
c      RETURN
c      END

C*********************************************************************
C			  INITIALIZE CONSTANTS:
C			      Scalel    =length of square in MPC
C			      AEXPN =expansion factor (current)
C			      ASTEP	  =increment of expansion factor: AEXPN =AEXP_0+N*ASTEP
C                     PARTW	=weight of a particle: total 'mass' per cell must be unity
c                                          even for LCDM or OCDM 
C			      AMPLT	=amplitude of perturbations inside the simulation box
C			      NBYTE	=record length in bytes
C
      SUBROUTINE InitValues(NBYTE,SCALEL)
      INCLUDE 'PMparameters.h'
      COMMON / TRUNCOM/ Om,Omb,Omc,Omnu,Par(6),ns,qqscaleb,QSCALE
      COMMON / FERMI  / Vscale
      Real                 ns      
      Character            Answer*1
      Real                 INPUT
      DATA PI		         /3.1415926535/

       Write (*,*) 'Would you like to use a model provided in cdm.fit '
       Write (*,*) 'or your own model?' 
       Write (*,'(A,$)') ' Enter Yes for cdm.fit;  No for your model ='
       Read  (*,'(A)')  Answer
       HEADER='N=128x256 L=20h-1CDMsig=0.239 z=30-----------'
       write (*,*) '------ Enter Header for the run up to 45 characters'
       write (*,*) '       Example:'
       write (*,'(A)') HEADER                                   
       read  (*,'(A)') HEADER
       write (*,'(A)') HEADER
       AEXPN =INPUT(' Initial expansion parameter (0-1)=')
       ASTEP =INPUT(' Step of the expansion parameter  =')
       AMPLT =INPUT(' Amplitude of density fluctuations=')
       ns    =INPUT(' Slope of the Power spectrum     n=')
       SCALEL=INPUT(' Box size (Mpc/h)                 =')
       Ocurv =INPUT(' Omega_curvature   at present     =')
       Nseed =INPUT(' Random seed (Integer  1-2^31)    =') 
       If(Answer.eq.'Y' .or. Answer.eq.'y')Then
          CALL MODEL(hsmall)
          hubble=hsmall
          Om0   =Om
          Oml0  =1. -Om0 -Ocurv
       Else
          If(Answer.eq.' ')Then
             write (*,*) ' There is a blank space in the',
     &          ' beginning of your answer'
             write (*,*) ' Remove it and start again. Bye'
             stop
          Endif 
       write (*,*) ' You use your cosmological model.'
       write (*,*) ' Be sure you provide routine TRUNF'
       hubble=INPUT(' The Hubble constant (h=H/100)    =')
       Om0   =INPUT(' Omega_0 matter    at present     =')
       Oml0  =INPUT(' Omega_lambda      at present     =')
       EndIf
       SCALEL=SCALEL/hubble   ! scale it to real megaparsecs      
       NBYTE = NPAGE*6*4
       Nspecies =0
       W     = (FLOAT(NGRID)/FLOAT(NROW))**3
       PARTW = W 
c                                 W1    = (W -Partw)/Nspecies
       ISTEP = 0
       TINTG = 0.
       AU0   = 0.
       AEU0  = 0.
       EKIN  = 0.
       EKIN1 = 0.
       EKIN2 = 0.
       NROWC = NROW
       NGRIDC= NGRID
	     QSCALE = 2.*PI/SCALEL
	     QS     = hubble**(-2)
        write (*,*) ' Qscale=',QSCALE,SCALEL,ns

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
      SUBROUTINE MODEL(hsmall)
C---------------------------------------
      INCLUDE 'PMparameters.h'
      COMMON / TRUNCOM/ Om,Omb,Omc,Omnu,Par(6),ns,qqscaleb,QSCALE
      Real                 ns,INPUT
      Character             Header1*79

      OPEN(2,file='cdm.fit',status='unknown')
      Line1 =INPUT(' Enter Line Number in cdm.fit     =')
      Read(2,'(A)') Header1
      If(Line1.gt.2)Then
         Do i=1,Line1-2
            Read (2,*) a
         EndDo
      EndIf
      Read (2,*) Om,Omb,Omc,Omnu,hsmall,Par
      CLOSE (2) 
      
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
C------------------------------------------------
C                             Make a realization of spectrum of perturbations:
C                             ALPHA  = normalization factor for displacements
C                             NRAND = seed for random numbers 
      SUBROUTINE SPECTR(ALPHA,NRAND)
C------------------------------------------------
      INCLUDE 'PMparameters.h'
      PARAMETER (NSPEC = NROW/2 ) ! No waves sharter than Ny
      COMMON / GRID/	GRX(NROW,NROW,NROW),GRY(NROW,NROW,NROW),
     +			         GRZ(NROW,NROW,NROW)
C						    Set spectrum
      SUMM = 0.
      DO  MK3 = 1,NROW
      DO  MK2 = 1,NROW
      DO  MK1 = 1,NROW
	     GRX(MK1,MK2,MK3) =0.
	     GRY(MK1,MK2,MK3) =0.
	     GRZ(MK1,MK2,MK3) =0.
      ENDDO
      ENDDO
      ENDDO

      DO  k = 1,NROW
         ksign = -1
         kz    = k +NSPEC
         k3    = k - 1
         If(k3.GT.NSPEC)k3=k3-NSPEC
           IF(kz.gt.NROW)THEN
              kz    =kz -NROW
              ksign =1
           ENDIF
           WK3= K3**2
c	        write (*,*) '  K=',k,' K3=',K3,' Nspec=',NSPEC
           DO  j = 1,NROW
              jsign = -1
              jz    = j +NSPEC
              j3    = j - 1
              If(j3.GT.NSPEC)j3=j3-NSPEC
                IF(jz.gt.NROW)THEN
                   jz    =jz -NROW
                   jsign =1
                ENDIF
                WJ3= j3**2
                DO  i = 1,NROW
                   isign = -1
                   iz    = i +NSPEC
                   i3    = i - 1
                   If(i3.GT.NSPEC)i3=i3-NSPEC
                     IF(iz.gt.NROW)THEN
                        iz    =iz -NROW
                        isign =1
                     ENDIF
                     WI3= i3**2

	       IF(i3+j3+k3.EQ.0)THEN
		         GRX(1,1,1) = 0.
		         GRY(1,1,1) = 0.
		         GRZ(1,1,1) = 0.
	       ELSE
		  WD = WI3 + WJ3 + WK3
		  WK = SQRT(WD)
		  TS = TRUNF(WK) * GAUSS(NRAND)
		  TRX = TS / WD
		  GRX(iz, j, k) = i3 * TRX *isign
		  GRY( i,jz, k) = j3 * TRX *jsign
		  GRZ( i, j,kz) = k3 * TRX *ksign
		  SUMM = SUMM + TS**2
	       ENDIF
	    ENDDO
	   ENDDO
      ENDDO
      IF(SUMM.LE.0.)WRITE (*,*) ' Error!!! Summ over spectrum = 0'
         write (*,*) ' ==== SPECTR ===  SUMM=',SUMM
      ALPHA = AMPLT / SQRT(SUMM) *sqrt(8.)
      RETURN
      END
C------------------------------------------------
C		                        Define coordinates and velosities for
C		                        all particles of current row (given q2)
C			                       x = q - amplt*b(t)/b(t0)*S(q1,q2)
C			                       momentum = a**2*deriv x
C                     
C		                        Twidled variables:
C		                        x= q - {1/(2 pi Ngrid)}{Alpha*a(t)/a(0)}*S
C		                        P=	- {1/(2 pi Ngrid)}{Alpha*a(t)**1.5/a(0)}*S
C                     
C		                        q(i) = Ngrid/Nrow*(i-1) + 1
C		                        x = q - xcons*S
C		                        P =	   vcons*S
      SUBROUTINE SETXV(XCONS,VCONS,K)
C------------------------------------------------
      INCLUDE 'PMparameters.h'
      COMMON / KINENRG/ SKINE,SX,SY,SZ,SX2,SY2,SZ2
      COMMON / GRID/	GRX(NROW,NROW,NROW),GRY(NROW,NROW,NROW),
     +			GRZ(NROW,NROW,NROW)

      QFACT = FLOAT(NGRID)/FLOAT(NROW)
      XMAX  = FLOAT(NGRID) + 1.
      XSHF  = FLOAT(NGRID)
      Q3    = QFACT*(K-1.) +1.
      DO  J = 1,NROW
	      Q2 = QFACT*(J-1.) +1.
	   DO  I = 1,NROW
	      Q1 = QFACT*(I-1.) +1.
            IJ    =      I +(J-1)*NROW
	      DX	      =      XCONS*GRX(I,J,K)
	      DY	      =      XCONS*GRY(I,J,K)
	      DZ	      =      XCONS*GRZ(I,J,K)
C           the factor 0.5 in the following 3 lines places unperturbed
c           particle in the middle of a cell. This reduces the shot noise
C          Small number 1.E-4 is to avoid placing some particles
C          exactly in cell nodes. Does not make much differentce.
	      XPAR(IJ) = Q1 - DX    + 1.E-4  +0.5 
	      YPAR(IJ) = Q2 - DY    + 1.E-4  +0.5
	      ZPAR(IJ) = Q3 - DZ    + 1.E-4  +0.5
	      VX(IJ)   =      VCONS*GRX(I,J,K)
	      VY(IJ)   =      VCONS*GRY(I,J,K)
	      VZ(IJ)   =      VCONS*GRZ(I,J,K)
C                                Periodical boundary conditions
	       IF(XPAR(IJ).GT.XMAX)	XPAR(IJ)=XPAR(IJ)-XSHF
	       IF(XPAR(IJ).LE.1.)	XPAR(IJ)=XPAR(IJ)+XSHF
	       IF(YPAR(IJ).GT.XMAX)	YPAR(IJ)=YPAR(IJ)-XSHF
	       IF(YPAR(IJ).LE.1.)	YPAR(IJ)=YPAR(IJ)+XSHF
	       IF(ZPAR(IJ).GT.XMAX)	ZPAR(IJ)=ZPAR(IJ)-XSHF
	       IF(ZPAR(IJ).LE.1.)	ZPAR(IJ)=ZPAR(IJ)+XSHF
	      SX	      = SX    + DX
	      SY	      = SY    + DY
	      SZ	      = SZ    + DZ
	      SX2      = SX2   + DX**2
	      SY2      = SY2   + DY**2
	      SZ2      = SZ2   + DZ**2
	      SKINE    = SKINE + VX(IJ)**2 +VY(IJ)**2 +VZ(IJ)**2
	   ENDDO
      ENDDO
      RETURN
      END
C------------------------------------------------
C                                              				   FFT of the spectrum
      SUBROUTINE VECTOR(IFOUR)
C------------------------------------------------
      INCLUDE 'PMparameters.h'
      COMMON / GRID/	GRX(NROW,NROW,NROW),GRY(NROW,NROW,NROW),
     +			GRZ(NROW,NROW,NROW)
      CALL SETF67(4,IFOUR)   ! initialize FFT
C				       x-component
      DO K=1,NROW
	   DO J=1,NROW
	    DO I=1,NROW
	       Zf(I) =GRX(I,J,K)
	    ENDDO
	    CALL FOUR67(4,IFOUR)
	    DO I=1,NROW
	       GRX(I,J,K) = Yf(I)
	    ENDDO
	   ENDDO
      ENDDO
      DO K=1,NROW
	   DO I=1,NROW
	    DO J=1,NROW
	       Zf(J) =GRX(I,J,K)
	    ENDDO
	    CALL FOUR67(4,IFOUR)
	    DO J=1,NROW
	       GRX(I,J,K) = Yf(J)
	    ENDDO
	   ENDDO
      ENDDO
      DO J=1,NROW
	   DO I=1,NROW
	    DO K=1,NROW
	       Zf(K) =GRX(I,J,K)
	    ENDDO
	    CALL FOUR67(4,IFOUR)
	    DO K=1,NROW
	       GRX(I,J,K) = Yf(K)/8.
	    ENDDO
	   ENDDO
      ENDDO
				       write (*,*) ' x is done'
C				       Y-component
      DO K=1,NROW
	   DO J=1,NROW
	    DO I=1,NROW
	       Zf(I) =GRY(I,J,K)
	    ENDDO
	    CALL FOUR67(4,IFOUR)
	    DO I=1,NROW
	       GRY(I,J,K) = Yf(I)
	    ENDDO
	   ENDDO
      ENDDO
      DO K=1,NROW
	   DO I=1,NROW
	    DO J=1,NROW
	       Zf(J) =GRY(I,J,K)
	    ENDDO
	    CALL FOUR67(4,IFOUR)
	    DO J=1,NROW
	       GRY(I,J,K) = Yf(J)
	    ENDDO
	   ENDDO
      ENDDO
      DO J=1,NROW
	   DO I=1,NROW
	    DO K=1,NROW
	       Zf(K) =GRY(I,J,K)
	    ENDDO
	    CALL FOUR67(4,IFOUR)
	    DO K=1,NROW
	       GRY(I,J,K) = Yf(K)/8.
	    ENDDO
	   ENDDO
      ENDDO
				       write (*,*) ' y is done'
C				       Z-component
      DO K=1,NROW
	   DO J=1,NROW
	    DO I=1,NROW
	       Zf(I) =GRZ(I,J,K)
	    ENDDO
	    CALL FOUR67(4,IFOUR)
	    DO I=1,NROW
	       GRZ(I,J,K) = Yf(I)
	    ENDDO
	   ENDDO
      ENDDO
      DO K=1,NROW
	   DO I=1,NROW
	    DO J=1,NROW
	       Zf(J) =GRZ(I,J,K)
	    ENDDO
	    CALL FOUR67(4,IFOUR)
	    DO J=1,NROW
	       GRZ(I,J,K) = Yf(J)
	    ENDDO
	   ENDDO
      ENDDO
      DO J=1,NROW
	   DO I=1,NROW
	    DO K=1,NROW
	       Zf(K) =GRZ(I,J,K)
	    ENDDO
	    CALL FOUR67(4,IFOUR)
	    DO K=1,NROW
	       GRZ(I,J,K) = Yf(K)/8.
	    ENDDO
	   ENDDO
      ENDDO
				       write (*,*) ' z is done'
      RETURN
      END
