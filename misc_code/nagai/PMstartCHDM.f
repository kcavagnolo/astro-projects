C _______________________ START 3-D SIMULATIONS
C
C                         Klypin, Jenuary 1997
C
      INCLUDE 'PMparameters.h'
C		   NROW = number of particles per row
C		   NGRID= number of cells on a side (normal grid)
C		   Npage =  number of points per page of the grid
C
      COMMON / GRID/	GRX(NROW,NROW,NROW),GRY(NROW,NROW,NROW),
     +			GRZ(NROW,NROW,NROW)
      COMMON / GRIV/	GVX(NROW,NROW,NROW),GVY(NROW,NROW,NROW),
     +			GVZ(NROW,NROW,NROW)
      COMMON / ROW2/	XPAR2(NPAGE),YPAR2(NPAGE),ZPAR2(NPAGE),
     +			VX2(NPAGE),VY2(NPAGE),VZ2(NPAGE)
      COMMON / KINENRG/ SKINE,SX,SY,SZ,SX2,SY2,SZ2
      DATA PI		/3.1415926535/
      CALL InitValues(NBYTE,SCALEL)  ! Set parameters of the run
         write (*,*) '  InitValues is done. N bytes=',NBYTE
         write (*,*) '  Nspecies =',Nspecies
         write (16,*) ' Seed=',Nseed
      OPEN(UNIT=9,FILE='PMcrd.DAT',form='unformatted',status='unknown')
      write (9)                ! this   clears old header file
      CLOSE (9)
      CALL RDTAPE                  ! this opens files on disk         
      OPEN(UNIT=16,FILE='RESNEW.DAT',status='unknown') ! open protocol
C
        AEXP0 = AEXPN            ! expansion parameter for coords
        AEXPV = AEXPN - ASTEP/2. !                     for veloc
C----                        Cold Particles --------------------------      
        NRAND =Nseed             ! store the initial seed
      CALL SPECTR(ALPHA,NRAND) ! get a realization of spectrum
        IFOUR = INT(ALOG(FLOAT(NROW))/ALOG(2.)+0.5)
        write (*,*) ' Ifour =',IFOUR,' Nrow=',NROW
      CALL VECTOR(IFOUR)       ! get displacement vector by FFT
   	write (*,*) ' SPECTR is done. IFOUR = ',IFOUR,' Alpha=',ALPHA
        VCONS = -ALPHA/(2.*PI/NGRID)*(AEXPV/AEXP0)*SQRT(AEXPV)
        XCONS =	ALPHA/(2.*PI/NGRID)*(AEXPN/AEXP0)
        QFACT = FLOAT(NGRID)/FLOAT(NROW)
        write (*,*) ' Aexp=',AEXPN,AEXP0,AEXPV,ASTEP
                      SKINE = 0.   ! set counters to zero
                      SX    = 0.
                      SY    = 0.
                      SZ    = 0.
                      SX2   = 0.
                      SY2   = 0.
                      SZ2   = 0.
      DO  KROW = 1, NROW  ! find x,v and write to file, page by page
	     CALL SETXV(XCONS,VCONS,KROW)
	     CALL WRIROW(KROW,1)
      ENDDO
      EKIN = 0.5*SKINE/AEXPV**2*PARTW !kinet.energy at A(i-1/2)
      DP   = NROW**3
      SX   = SX/DP                    ! mean displacement
      SY   = SY/DP
      SZ   = SZ/DP
      SX2  = SQRT(SX2/DP)             ! rms displacement
      SY2  = SQRT(SY2/DP)
      SZ2  = SQRT(SZ2/DP)
      WRITE (*,'('' Displacement of a particle: MEAN'',3F9.4)')
     .	     SX,SY,SZ
      WRITE (*,'(''                             RMS '',3F9.4)')
     .	     SX2,SY2,SZ2
      WRITE (16,'('' Displacement of a particle: MEAN'',3F9.4)')
     .	     SX2,SY2,SZ2
      Veloc=sqrt( SKINE/AEXPV**2/NROW**3)*SCALEL/NGRID*50.
      WRITE (*,'('' Ekin='',E12.4,'' Veloc_cold(km/s)='',F8.2)')
     .       EKIN,Veloc
      WRITE (16,'('' Ekin='',E12.4,'' Veloc_cold(km/s)='',F8.2)')
     .       EKIN,Veloc

C----                        Neutrino ------------------------------      
        NRAND =Nseed       !  Restore initial seed
      CALL SPECNU(NRAND) !  Renormalize the spectrum for neutrino
      CALL VECTOR(IFOUR) !   get displacement vector by FFT
	     write (*,*) ' Neutrino SPECTRUM is done.'
C			     find x,v and write to file, page by page
         Svel =SKINE
         SKINE=0.
         DO  KROW = 1, NROW
           Do js=1,Nspecies,2
              CALL SETXVN(XCONS,VCONS,KROW,NRAND)
              CALL WRIROW(KROW,js+1)
              If(Nspecies.ne.js)Then ! write another pair of
                 Do I=1,NPAGE        ! particles
                    XPAR(I)=XPAR2(I)
                    YPAR(I)=YPAR2(I)
                    ZPAR(I)=ZPAR2(I)
                    VX(I)  =VX2(I)
                    VY(I)  =VY2(I)
                    VZ(I)  =VZ2(I)
                 EndDo
                 CALL WRIROW(KROW,js+2)
              EndIf
           EndDo
         ENDDO
      W    = (FLOAT(NGRID)/FLOAT(NROW))**3         
      EKIN = 0.5*(Svel*PARTW+SKINE*(W-PARTW)/Nspecies)/AEXPV**2
      Veloc= sqrt( SKINE/AEXPV**2/NROW**3/Float(Nspecies))
     .           *SCALEL/NGRID*50. 
      WRITE (16,'('' Ekin='',E12.4,'' Veloc_hot(km/s)='',F8.2)')
     .       EKIN,Veloc
      WRITE (*,'('' Ekin='',E12.4,'' Veloc_hot(km/s)='',F8.2)')
     .       EKIN,Veloc
      CALL WRTAPE    !   write header and control data
      STOP
      END
C************************** MDM:    0.1-0.3-0.6    7eV
c      FUNCTION TRUNF(wk)
c      INCLUDE 'PMparameters.h'
c      PARAMETER (NSPEC =NROW/2)
c      COMMON / TRUNCOM/ QSCALE, ns
c      DATA A1,A2,A3,A4,A5 
c     .         /3.93, - 60.2, 409., -17.1, 12.5/
c       IF (WK.GE.FLOAT(NSPEC)) THEN
c	  TRUNF =0.
c	  RETURN
c       ENDIF
c       Q = QSCALE *WK
c       sk=sqrt(Q)
c       TRUNF = sqrt(LOG(1.+A5*g*Q) / (A5*g)) /
c     .         (1.+sk*(A1*(1.-g/3.2)
c     .            +sk*(A2*(1.-g/3.5)
c     .            +sk*(A3*(1.-g/5.)
c     .            +sk*(A4*(1.-g**2/15.) )))) )
c      RETURN
c      END
C************************** MDM: Om_nu=0.2, z=30. 
      FUNCTION TRUNF(wk)      
      INCLUDE 'PMparameters.h'
      PARAMETER (NSPEC =NROW/2)
      COMMON / TRUNCOM/ QSCALE, ns
      DIMENSION          PP(6)
      REAL                ns
      DATA   PP/1.,-1.464,19.81,154.5,143.6,0.961/
       IF (WK.GE.FLOAT(NSPEC)) THEN
	      TRUNF =0. ! truncate above nyquist frequency
	      RETURN
       ENDIF
       Q = QSCALE*WK
       sk= sqrt(Q)
       TRUNF= sqrt(wk**ns/
     .        (1.+sk*(PP(2) +sk*(PP(3) +sk*(PP(4)
     .           +sk*PP(5) ))))**(2.*PP(6)) )
      RETURN
      END
C**************************:  Approx for Growth rate for MDM cold
      FUNCTION GrPcold(wk)
c      PARAMETER (C1=0.805,C2=0.195,C3=0.3,C4=0.70803)
      PARAMETER (C1=0.8736,C2=1.-C1,C3=0.0891,C4=0.7347)
      COMMON / TRUNCOM/ QSCALE, ns
      REAL                ns
         Q = QSCALE*WK
         GrPcold =C1+C2/(1. +exp(LOG(Q/C3)/C4))
      Return
      End
C**************************:  Approx for Growth rate for MDM hot
      FUNCTION GrPhot(wk)
c      PARAMETER (C1=1.80,C2=-0.8,C3=1.4,C4=1.065)
      PARAMETER (C1=1.75,C2=-0.75,C3=0.2165,C4=0.88)
      COMMON / TRUNCOM/ QSCALE, ns
      REAL                ns
         Q = QSCALE*WK
         GrPhot =C1+C2/(1. +exp(LOG(Q/C3)/C4))
      Return
      End
C************************** Neutrino:   Cold * correction factor
      FUNCTION TRUNU(WK)
      INCLUDE 'PMparameters.h'
      COMMON / TRUNCOM/ QSCALE, ns
      DIMENSION PP(6)
      REAL                ns
      DATA PP/1.442,-0.651,4.03,1.553,-35.50,141.7/
         Q = QSCALE*WK
         sk= (Q)**0.33333
         TRUNU =TRUNF(WK)  *
     .                    sqrt( exp(-PP(1)*Q)
     .                   /(1.+sk*(PP(2) +sk*(PP(3)
     .                       +sk*(PP(4) +sk*(PP(5) 
     .                       +sk*PP(6)    ))))) )
      RETURN
      END
C*************************************** Velocity distribution
      SUBROUTINE VELCT(Wx,Wy,Wz,NRAND)
      COMMON / FERMI  / Vscale
C                                       get random unit vector
 1    x =(RANDd(NRAND)-0.5)*2.
      y =(RANDd(NRAND)-0.5)*2.
      z =(RANDd(NRAND)-0.5)*2.
      r2 =x**2 +y**2 +z**2
            If(r2 .gt. 1.)goto1
C                                      Fermi distribution            
      p  =RANDd(NRAND)
      v  =Vscale *Vfermi(p)/sqrt(r2)
      Wx =x *v
      Wy =y *v
      Wz =z *v

      Return
      End
C*********************************************************************
C			  INITIALIZE CONSTANTS:
C			 Scalel -length of square in MPC
C			 AEXPN	-expansion factor (current)
C			 ASTEP	-increment of expansion factor
C			 PARTW	-weight of a particle
C			 AMPLT	-amplitude of perturbations
C			 NBYTE	-record length in bytes
C
      SUBROUTINE InitValues(NBYTE,SCALEL)
      INCLUDE 'PMparameters.h'
      COMMON / TRUNCOM/ QSCALE, ns
      COMMON / FERMI  / Vscale
      REAL                INPUT, ns
      DATA PI		 /3.1415926535/
C                          '123456789012345678901234567890123456789012345'   
       HEADER='N=256x512 L=50/hsig=0.174totCHDM2nu z0=30Ns=3'
       Om0   = 1.         ! \Omega_0 at z=0
       Oml0  = 0.         ! \Omega_lambda at z=0
       Ocurv = 0.
       hubble= 0.5          
       write (*,*) '------ Enter Header for the run up to 45 characters'
       write (*,*) '       Example:'
       write (*,'(A)') HEADER
       read  (*,'(A)') HEADER
       write (*,'(A)') HEADER
       AEXPN =1./(1.+30.)   !  z_init = 30.
       write (*,*) ' Initial expansion parameter     =',AEXPN
       ASTEP =INPUT(' Step of the expansion parameter  =')
       AMPLT =INPUT(' Amplitude of density fluctuations=')
       ns    =INPUT(' Slope of the power spectrum      =')
       SCALEL=INPUT(' Box size (Mpc/h)                 =')
       Nseed =INPUT(' Random seed (Integer  1-2^31)    =') 
       Nspecies =INPUT(' Number of neutrino/cold_part(1-6)=') 
       SCALEL=SCALEL/hubble   ! scale it to real megaparsecs  
       write (*,*) ' Aexpn=',AEXPN,' dA=',ASTEP
C                          Velocity scale for neutrino thermal vel.
C                          Vrms = 3.59*Vscale
C                          No dependance on Aexpn (P, not V)       
c       Vscale= 7.2/(SCALEL*50./NGRID)
c       Vscale= 8.82/(SCALEL*50./NGRID)  ! m_nu=5.72eV, Omega_nu=0.25
       Vscale= 22.0/(SCALEL*50./NGRID) ! m_nu=2.29eV, Omega_nu=0.1x2
       NBYTE = NPAGE*6*4
       W     = (FLOAT(NGRID)/FLOAT(NROW))**3
       PARTW = W *0.80       ! this is for 20% neutrino
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
      RETURN
      END
C*********************************************************************
C		    Set spectrum of perturbations:
      SUBROUTINE SPECTR(ALPHA,NRAND)
      INCLUDE 'PMparameters.h'
      PARAMETER (NSPEC = NROW/2 )
      COMMON / GRID/	GRX(NROW,NROW,NROW),GRY(NROW,NROW,NROW),
     +			GRZ(NROW,NROW,NROW)
      COMMON / GRIV/	GVX(NROW,NROW,NROW),GVY(NROW,NROW,NROW),
     +			GVZ(NROW,NROW,NROW)
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
                  TV = GrPcold(WK)
		  GRX(iz, j, k) = i3 * TRX *isign
		  GRY( i,jz, k) = j3 * TRX *jsign
		  GRZ( i, j,kz) = k3 * TRX *ksign
		  GVX(iz, j, k) = TV * GRX(iz, j, k)
		  GVY( i,jz, k) = TV * GRY( i,jz, k)
		  GVZ( i, j,kz) = TV * GRZ( i, j,kz)
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
C*********************************************************************
C		    Set spectrum of perturbations:
      SUBROUTINE SPECNU(NRAND)
      INCLUDE 'PMparameters.h'
      PARAMETER (NSPEC = NROW/2 )
      COMMON / GRID/	GRX(NROW,NROW,NROW),GRY(NROW,NROW,NROW),
     +			GRZ(NROW,NROW,NROW)
      COMMON / GRIV/	GVX(NROW,NROW,NROW),GVY(NROW,NROW,NROW),
     +			GVZ(NROW,NROW,NROW)
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

		  TS = TRUNU(WK) * GAUSS(NRAND)
		  TRX = TS / WD
                  TV = GrPhot(WK)
		  GRX(iz, j, k) = i3 * TRX *isign
		  GRY( i,jz, k) = j3 * TRX *jsign
		  GRZ( i, j,kz) = k3 * TRX *ksign
		  GVX(iz, j, k) = TV * GRX(iz, j, k)
		  GVY( i,jz, k) = TV * GRY( i,jz, k)
		  GVZ( i, j,kz) = TV * GRZ( i, j,kz)
		  SUMM = SUMM + TS**2
	       ENDIF
	    ENDDO
	   ENDDO
      ENDDO
      IF(SUMM.LE.0.)WRITE (*,*) ' Error!!! Summ over spectrum = 0'
         write (*,*) ' ==== SPECTR ===  SUMM=',SUMM
      RETURN
      END
C*********************************************************************
C		   Define coordinates and velosities for
C		   all particles of current row (given q2)
C			  x = q - amplt*b(t)/b(t0)*S(q1,q2)
C			  momentum = a**2*deriv x
C
C		   Twidled variables:
C		   x= q - {1/(2 pi Ngrid)}{Alpha*a(t)/a(0)}*S
C		   P=	- {1/(2 pi Ngrid)}{Alpha*a(t)**1.5/a(0)}*S
C
C		   q(i) = Ngrid/Nrow*(i-1) + 1
C		   x = q - xcons*S
C		   P =	   vcons*S
      SUBROUTINE SETXV(XCONS,VCONS,K)
      INCLUDE 'PMparameters.h'
      COMMON / KINENRG/ SKINE,SX,SY,SZ,SX2,SY2,SZ2
      COMMON / GRID/	GRX(NROW,NROW,NROW),GRY(NROW,NROW,NROW),
     +			GRZ(NROW,NROW,NROW)
      COMMON / GRIV/	GVX(NROW,NROW,NROW),GVY(NROW,NROW,NROW),
     +			GVZ(NROW,NROW,NROW)

      QFACT = FLOAT(NGRID)/FLOAT(NROW)
      XMAX  = FLOAT(NGRID) + 1.
      XSHF  = FLOAT(NGRID)
      Q3    = QFACT*(K-1.) +1.
      DO  J = 1,NROW
	     Q2 = QFACT*(J-1.) +1.
	     DO  I = 1,NROW
	       Q1       = QFACT*(I-1.) +1.
          IJ       =      I +(J-1)*NROW
	       DX	    =      XCONS*GRX(I,J,K)
	       DY	    =      XCONS*GRY(I,J,K)
	       DZ	    =      XCONS*GRZ(I,J,K)
	       XPAR(IJ) = Q1 - DX       + 1.E-4 +0.5
	       YPAR(IJ) = Q2 - DY       + 1.E-4 +0.5
	       ZPAR(IJ) = Q3 - DZ       + 1.E-4 +0.5
	       VX(IJ)   =      VCONS*GVX(I,J,K)
	       VY(IJ)   =      VCONS*GVY(I,J,K)
	       VZ(IJ)   =      VCONS*GVZ(I,J,K)
	       IF(XPAR(IJ).GT.XMAX)	XPAR(IJ)=XPAR(IJ)-XSHF
	       IF(XPAR(IJ).LE.1.)	XPAR(IJ)=XPAR(IJ)+XSHF
	       IF(YPAR(IJ).GT.XMAX)	YPAR(IJ)=YPAR(IJ)-XSHF
	       IF(YPAR(IJ).LE.1.)	YPAR(IJ)=YPAR(IJ)+XSHF
	       IF(ZPAR(IJ).GT.XMAX)	ZPAR(IJ)=ZPAR(IJ)-XSHF
	       IF(ZPAR(IJ).LE.1.)	ZPAR(IJ)=ZPAR(IJ)+XSHF
	       SX	    = SX    + DX
	       SY	    = SY    + DY
	       SZ	    = SZ    + DZ
	       SX2      = SX2   + DX**2
	       SY2      = SY2   + DY**2
	       SZ2      = SZ2   + DZ**2
	       SKINE    = SKINE + VX(IJ)**2 +VY(IJ)**2 +VZ(IJ)**2
	     ENDDO
      ENDDO
      RETURN
      END
C*********************************************************************
C		   Define coordinates and velosities for
C		   neutrino in the current row 
C
      SUBROUTINE SETXVN(XCONS,VCONS,K,NRAND)
      INCLUDE 'PMparameters.h'
      COMMON / KINENRG/ SKINE,SX,SY,SZ,SX2,SY2,SZ2
      COMMON / GRID/	GRX(NROW,NROW,NROW),GRY(NROW,NROW,NROW),
     +			GRZ(NROW,NROW,NROW)
      COMMON / GRIV/	GVX(NROW,NROW,NROW),GVY(NROW,NROW,NROW),
     +			GVZ(NROW,NROW,NROW)
      COMMON / ROW2/	XPAR2(NPAGE),YPAR2(NPAGE),ZPAR2(NPAGE),
     +			VX2(NPAGE),VY2(NPAGE),VZ2(NPAGE)

      QFACT = FLOAT(NGRID)/FLOAT(NROW)
      XMAX  = FLOAT(NGRID) + 1.
      XSHF  = FLOAT(NGRID)
      Q3    = QFACT*(K-1.) +1.
            
      DO  J = 1,NROW
	     Q2 = QFACT*(J-1.) +1.
	     DO  I = 1,NROW
	       Q1 = QFACT*(I-1.) +1.
          IJ       =      I +(J-1)*NROW
	       DX	    =      XCONS*GRX(I,J,K)
	       DY	    =      XCONS*GRY(I,J,K)
	       DZ	    =      XCONS*GRZ(I,J,K)
	       XPAR(IJ) = Q1 - DX       + 1.E-4 +0.5
	       YPAR(IJ) = Q2 - DY       + 1.E-4 +0.5
	       ZPAR(IJ) = Q3 - DZ       + 1.E-4 +0.5
          CALL VELCT(Wx,Wy,Wz,NRAND)
	       VX(IJ)   =      VCONS*GVX(I,J,K) +Wx
	       VY(IJ)   =      VCONS*GVY(I,J,K) +Wy
	       VZ(IJ)   =      VCONS*GVZ(I,J,K) +Wz
	       VX2(IJ)  =      VCONS*GVX(I,J,K) -Wx
	       VY2(IJ)  =      VCONS*GVY(I,J,K) -Wy
	       VZ2(IJ)  =      VCONS*GVZ(I,J,K) -Wz
	          IF(XPAR(IJ).GT.XMAX)	XPAR(IJ)=XPAR(IJ)-XSHF
	          IF(XPAR(IJ).LE.1.)	XPAR(IJ)=XPAR(IJ)+XSHF
	          IF(YPAR(IJ).GT.XMAX)	YPAR(IJ)=YPAR(IJ)-XSHF
	          IF(YPAR(IJ).LE.1.)	YPAR(IJ)=YPAR(IJ)+XSHF
	          IF(ZPAR(IJ).GT.XMAX)	ZPAR(IJ)=ZPAR(IJ)-XSHF
	          IF(ZPAR(IJ).LE.1.)	ZPAR(IJ)=ZPAR(IJ)+XSHF
          XPAR2(IJ)=      XPAR(IJ)
          YPAR2(IJ)=      YPAR(IJ)
          ZPAR2(IJ)=      ZPAR(IJ)
	       SKINE    = SKINE + VX(IJ)**2 +VY(IJ)**2 +VZ(IJ)**2
     .                     + VX2(IJ)**2+VY2(IJ)**2+VZ2(IJ)**2
   	  ENDDO
      ENDDO
      RETURN
      END
C*********************************************************************
C				   FFT of the spectrum
      SUBROUTINE VECTOR(IFOUR)
      INCLUDE 'PMparameters.h'
      COMMON / GRID/	GRX(NROW,NROW,NROW),GRY(NROW,NROW,NROW),
     +			GRZ(NROW,NROW,NROW)
      COMMON / GRIV/	GVX(NROW,NROW,NROW),GVY(NROW,NROW,NROW),
     +			GVZ(NROW,NROW,NROW)
      CALL SETF67(4,IFOUR)
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
	    DO I=1,NROW
	       Zf(I) =GVX(I,J,K)
	    ENDDO
	    CALL FOUR67(4,IFOUR)
	    DO I=1,NROW
	       GVX(I,J,K) = Yf(I)
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
	    DO J=1,NROW
	       Zf(J) =GVX(I,J,K)
	    ENDDO
	    CALL FOUR67(4,IFOUR)
	    DO J=1,NROW
	       GVX(I,J,K) = Yf(J)
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
	    DO K=1,NROW
	       Zf(K) =GVX(I,J,K)
	    ENDDO
	    CALL FOUR67(4,IFOUR)
	    DO K=1,NROW
	       GVX(I,J,K) = Yf(K)/8.
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
	    DO I=1,NROW
	       Zf(I) =GVY(I,J,K)
	    ENDDO
	    CALL FOUR67(4,IFOUR)
	    DO I=1,NROW
	       GVY(I,J,K) = Yf(I)
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
	    DO J=1,NROW
	       Zf(J) =GVY(I,J,K)
	    ENDDO
	    CALL FOUR67(4,IFOUR)
	    DO J=1,NROW
	       GVY(I,J,K) = Yf(J)
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
	    DO K=1,NROW
	       Zf(K) =GVY(I,J,K)
	    ENDDO
	    CALL FOUR67(4,IFOUR)
	    DO K=1,NROW
	       GVY(I,J,K) = Yf(K)/8.
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
	    DO I=1,NROW
	       Zf(I) =GVZ(I,J,K)
	    ENDDO
	    CALL FOUR67(4,IFOUR)
	    DO I=1,NROW
	       GVZ(I,J,K) = Yf(I)
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
	    DO J=1,NROW
	       Zf(J) =GVZ(I,J,K)
	    ENDDO
	    CALL FOUR67(4,IFOUR)
	    DO J=1,NROW
	       GVZ(I,J,K) = Yf(J)
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
	    DO K=1,NROW
	       Zf(K) =GVZ(I,J,K)
	    ENDDO
	    CALL FOUR67(4,IFOUR)
	    DO K=1,NROW
	       GVZ(I,J,K) = Yf(K)/8.
	    ENDDO
	   ENDDO
      ENDDO
				       write (*,*) ' z is done'
      RETURN
      END

C_________________________________________ Fermi distribution
C                                y - random number (0-1)
C                            dn = v^2dv/(exp(v)+1)/1.8036
C                                 \int_0^\infty dn =1      
      FUNCTION Vfermi(y)
         z  = y**0.333
         d  = MAX(1.-y,1.e-6)
            Vfermi = -1.15*log(d) +3.7e-4+z*(2.219+0.396*z) 
         IF( y.gt. 0.35)Then
            IF(y .gt. 0.55)Then
               Vfermi =Vfermi+2.04-1.937*sqrt(d)-1.24*y
            Else
               Vfermi =Vfermi+(2.04-1.937*sqrt(d)-1.24*y)*
     .                        (y-0.35)/0.2               
            EndIf
         Endif
      RETURN
      END
