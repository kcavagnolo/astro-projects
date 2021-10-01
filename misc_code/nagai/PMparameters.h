      IMPLICIT REAL*8 (D)
      PARAMETER (NROW=1024, NGRID =256, NPAGE=NROW**2, NMAX=NGRID/2)
      PARAMETER (NRECL= NPAGE*6, NARR =MAX(2*NROW+1,NGRID+1))
      PARAMETER ( NF67=MAX(NROW,NGRID/2))
      COMMON / CONTROL/ AEXPN,AEXP0,AMPLT,ASTEP,ISTEP,PARTW,
     +                  TINTG,EKIN,EKIN1,EKIN2,AU0,AEU0,
     +                  NROWC,NGRIDC,Nspecies,Nseed,Om0,Oml0,hubble,Wp5,
     +                  Ocurv,extras(100)
      COMMON / HEADDR/  HEADER
      CHARACTER*45      HEADER
      COMMON /FOURAR/Zf(NARR),Yf(NARR)
      COMMON /F67COM/
     +                 IBC,      IP,       ISL,     L1,     N2,
     +                 N3,       N4,        N7,
     +                 SI(NF67),    INDEX(NF67)
      COMMON / ROW /	XPAR(NPAGE),YPAR(NPAGE),ZPAR(NPAGE),
     +			VX(NPAGE),VY(NPAGE),VZ(NPAGE)
      DIMENSION         RECDAT(NRECL)
      EQUIVALENCE       (RECDAT(1),XPAR(1))








