        SUBROUTINE SETSPC(GAMIN,INOPT)
C
C  J.P.Leahy, N.R.A.O. 9/July/88
C  Version 0.4  26/Nov/89
c
C  editted by c.c. 4/90  to work under unix.
c  also, removed dbesks, and replaced with dgamma from NR
C
C  Sets up parameters for routines which return values of
C  synchrotron spectra.
C  GAMIN = Initial power-law index of electron spectrum.
C  INOPT = 1: Pacholczyk's B(x_T,Gamma). Aged spectrum at
C             single pitch angle.
C        = 2: Jaffe & Perola spectrum. Aged spectrum, pitch-angle
C             scattering, averaged over pitch-angle.
C        = 3: Kardashev-Pacholczyk spectrum [BT(xt_T,Gamma)].
C             Aged spectrum, no pitch-angle scattering, averaged
C             over pitch-angle.
C        = 4: Continuous injection, one pitch angle [Pacholczyk's
C             C(x_T,Gamma)].
C        = 5: Continuous injection, pitch-angle scattering, averaged
C             over pitch angle. [CJ(xt_T,Gamma)].
C        = 6: Continuous injection, no pitch-angle scattering,
C             averaged over pitch angle. Pacholczyk's CT(xt_T,Gamma)
c
c   needs PACHOL.INCL,  GSCONSTS.INCL
c 
        REAL GAMIN, GAMMA, CO
        INTEGER INOPT, MOD1
 
        INTEGER NX
        PARAMETER (NX = 20)
        REAL A1, B1, C1(NX), A2, B2, C2(NX)
        INTEGER M1, M2
        COMMON /BCC/ A1, B1, C1, M1, A2, B2, C2, M2
        SAVE BCC
 
        REAL A1T, B1T, C1T(NX), A2T, B2T, C2T(NX)
        INTEGER M1T, M2T
        COMMON /BTCC/ A1T, B1T, C1T, M1T, A2T, B2T, C2T, M2T
        SAVE BTCC
 
        INCLUDE 'PACHOL.INCL'
 
        REAL BAPR, PACHB, BTAPR, CTAPR, PACHBT, PACHCT
        EXTERNAL BAPR, PACHB, BTAPR, CTAPR, PACHBT, PACHCT
 
C  Save input option in /PACHOL/
        IOPT = INOPT
 
        IF (IOPT .GT. 6) THEN
          WRITE(*,*) '***ERROR: SETSPC: Unknown IOPT:',IOPT
        ELSE
          IF (IOPT .LE. 2) THEN
            MOD1 = IOPT
            GAMMA = GAMIN
          ELSE IF (IOPT .EQ. 3) THEN
            MOD1 = 1
            GAMMA = GAMIN
          ELSE IF (IOPT .LE. 5) THEN
            MOD1 = IOPT - 3
            GAMMA = GAMIN + 1
          ELSE IF (IOPT .EQ. 6) THEN
            MOD1 = 1
            GAMMA = GAMIN + 1
          END IF
 
          CALL SETPAC(GAMMA,MOD1)
 
C  Set relative error tolerance for fitted function:
          ERR = 5E-5
C  Initialise cross-over point for Chebyshev fitting:
          IF (GAMMI2 .GE. 1) THEN
            CO = -0.325
          ELSE IF (MODE .EQ. 1) THEN
            CO = -1.0
          ELSE
            CO = -0.7
          END IF
 
C  Find range of analytic approximation at low x:
          LOW = .TRUE.
          A1 = -4.0
          B1 = CO
          xxx = -6.0
          yyy = 0.5
          CALL SETEND(A1,xxx,yyy,ERR,BAPR)
C  Find Chebyshev coefficients:
          CALL CHEBFT(A1,B1,C1,NX,PACHB,ERR,M1)
C  Save lower bound of Chebyshev fit in common /CIDAT/:
          CBMIN = 10.0**A1
C  High x:
          LOW = .FALSE.
          A2 =  CO
          B2 =  1.6
          xxx = 2.0
          yyy = -0.2
          CALL SETEND(B2,xxx,yyy,ERR,BAPR)
          CALL CHEBFT(A2,B2,C2,NX,PACHB,ERR,M2)
          WRITE (*,*) 'SETSPC: Using',M1,M2,
     *                ' Chebyshev polynomials for B(x).'
 
C  Set up for KP spectrum if requested:
          IF (IOPT .EQ. 3) THEN
            CALL SETKP(GAMMA)
            ERR = 5E-4
C  Low x:
            LOW = .TRUE.
            A1T = -4.0
            B1T = 0.0
            xxx = -6.0
            yyy = 0.5
            CALL SETEND(A1T,xxx,yyy,ERR,BTAPR)
            CALL CHEBFT(A1T,B1T,C1T,NX,PACHBT,ERR,M1T)
C  High x:
            LOW = .FALSE.
            A2T = B1T
            B2T = 1.6
            xxx = 2.0
            yyy = -0.2
            CALL SETEND(B2T,xxx,yyy,ERR,BTAPR)
            CALL CHEBFT(A2T,B2T,C2T,NX,PACHBT,ERR,M2T)
            WRITE (*,*) 'SETSPC: Using',M1T,M2T,
     *                  ' Chebyshev polynomials for BT(x).'
 
C  Set up for CT spectrum if requested:
          ELSE IF (IOPT .EQ. 6) THEN
            CALL SETCT(GAMMA)
            ERR = 5E-4
C  Find range of analytic approximations:
            LOW = .TRUE.
            A1T = -4.0
            B1T = -0.5
            xxx = -6.0
            yyy = 0.5
            CALL SETEND(A1T,xxx,yyy,ERR,CTAPR)
            CALL CHEBFT(A1T,B1T,C1T,NX,PACHCT,ERR,M1T)
            LOW = .FALSE.
            A2T = B1T
            B2T = 1.0
            xxx = 2.0
            yyy = -0.2
            CALL SETEND(B2T,xxx,yyy,ERR,CTAPR)
            CALL CHEBFT(A2T,B2T,C2T,NX,PACHCT,ERR,M2T)
            WRITE (*,*) 'SETSPC: Using',M1T,M2T,
     *                  ' Chebyshev polynomials for CT(x).'
          END IF
        END IF
 
        RETURN
        END
C
        SUBROUTINE SETPAC(GAMMA,MOD1)
C
C  J.P.Leahy, N.R.A.O.
C
C  Version 2.0  26/Nov/89
C
C  Sets various scaling parameters in the common block /PACHOL/
C  GAMMA = power-law index of electron spectrum
C  MOD1  = 1 for Pacholcyzk's B(x_T,Gamma) = aged spectrum at
C            single pitch angle
C        = 2 for Jaffe & Perola spectrum: Isotropic pitch angle
C            distribution, aged spectrum, averaged over pitch
C            angle.
 
        DOUBLE PRECISION INFTY, XMAX
        PARAMETER (XMAX = 78D0, INFTY = 150D0)
        DOUBLE PRECISION DGAMMA
        EXTERNAL DGAMMA
        external c8, c9
C  Common Pachol, associatied declarations, and F2Kn parameters.
        INCLUDE 'GSCONSTS.INCL'
        INCLUDE 'PACHOL.INCL'
 
C  Check inputs:
        IF (GAMMA .LT. 2.0) THEN
          WRITE(*,*) 'GAMMA out of range: must be .GE. 2.0'
          STOP
          RETURN
        END IF
 
        RTINF = DSQRT(INFTY)
        GAMMI1 = GAMMA-1.0
        GAMMI2 = GAMMA-2.0
C
C  Set scale factors for analytic approximation for small x.
C
        B1K0 = C8(GAMMA)
        B1K1 = - GAMMI2 * C8(GAMMI1)
        IF (GAMMA .LE. 2.4) THEN
          B1K2 = 0.0
        ELSE
          B1K2 = GAMMI2*(GAMMA-3.0) * C8(GAMMI2) / 2.0
        END IF
 
C  And for large x:
        MODE = MOD1
        IF (MODE .EQ. 1) THEN
          B2K0 = F2K0 * DGAMMA(GAMMI1) * 2.0**(-GAMMI2)
          B2K1 = F2K1 - ((GAMMI2*GAMMI1) / 4.0)
          B2K2 = F2K2 - (GAMMI1/4.0) * ( F2K1 * (GAMMA+2.0)
     *           - (GAMMI2 * GAMMA * (GAMMA+1.0) / 8.0))
        ELSE IF (MODE .EQ. 2) THEN
          B2K0 = PI * DGAMMA(GAMMI1) * 2.0**(-GAMMI1)
          B2K1 = FT2K1 - ((GAMMA*GAMMI1) / 4.0)
          B2K2 = FT2K2 - (GAMMI1/4.0) * ( FT2K1 * (GAMMA+4.0)
     *           - (GAMMA * (GAMMA+1.0) * (GAMMA+2.0) / 8.0))
 
C Adjust small-x constants to allow for angle-averaging:
          B1K0 = B1K0 * C9(GAMMA)
          B1K1 = B1K1 * C9(GAMMA - 1.0)
          B1K2 = B1K2 * C9(GAMMA - 2.0)
        ELSE
          WRITE (*,*) 'SETPAC: Unknown MODE:', MODE
        END IF
 
        RETURN
        END
C
        SUBROUTINE SETKP(GAMMA)
 
        REAL GAMMA
        DOUBLE PRECISION DGAMMA, beta
        EXTERNAL DGAMMA, beta
        external c8,c9
        INCLUDE 'GSCONSTS.INCL'
        INCLUDE 'PACHOL.INCL'
 
C  Scaling amplitudes for low- and high-frequency power laws:
 
        BT1K0 = C8(GAMMA) * C9(GAMMA)
        BT1K1 = -GAMMI2 * C8(GAMMI1) * C9(GAMMA + 3.0)
        BT1K2 = GAMMI2 * (GAMMA - 3.0) * C8(GAMMI2) * C9(GAMMA + 6.0)
     *          / 2.0
        BT2K1 = DGAMMA((GAMMA+1e0)/3e0) * DGAMMA((GAMMA+6e0)/3e0)
     *          * BETA( (GAMMA+5e0) / 3e0, GAMMA - 1e0)
     *          / (GAMMA + 2.0)
        BT2K2 = DGAMMA((GAMMA+2e0)/3e0) * DGAMMA((GAMMA+7e0)/3e0)
     *          * BETA( (GAMMA+9e0) / 3e0, GAMMA - 1e0)
     *          / (2.0 * (GAMMA + 3.0))
        BT2K3 = DGAMMA((GAMMA+3e0)/3e0) * DGAMMA((GAMMA+8e0)/3e0)
     *          * BETA( (GAMMA+13e0) / 3e0, GAMMA - 1e0)
     *          * 3.0 / (8.0 * (GAMMA + 4.0))
        BT2K4 = DGAMMA((GAMMA+4e0)/3e0) * DGAMMA((GAMMA+9e0)/3e0)
     *          * BETA( (GAMMA+17e0) / 3e0, GAMMA - 1e0)
     *          * 5.0 / (16.0 * (GAMMA + 5.0))
 
        GKP = (2.0*GAMMA + 1.0)/3.0
        RETURN
        END
C
        SUBROUTINE SETCT(GAMMA)
 
        REAL GAMMA
        DOUBLE PRECISION DGAMMA, beta
        EXTERNAL DGAMMA, beta
        external c8, c9
        INCLUDE 'GSCONSTS.INCL'
        INCLUDE 'PACHOL.INCL'
 
C  Scaling amplitudes for low- and high-frequency power laws:
 
        BT1K0 = C8(GAMMA) * C9(GAMMA - 4.0)
        BT1K1 = - GAMMI2 * C8(GAMMI1) * C9(GAMMI1)
        BT1K2 = GAMMI2 * (GAMMA - 3.0) * C8(GAMMI2) * C9(GAMMA + 2.0)
     *          / 4.0
        BT2K1 = DGAMMA(GAMMA/3e0) * DGAMMA((GAMMA+5e0)/3e0)
     *          * BETA( (GAMMA+1e0) / 3e0, GAMMA - 1e0)
     *          / (GAMMA + 1.0)
        BT2K2 = DGAMMA((GAMMA+1e0)/3e0) * DGAMMA((GAMMA+6e0)/3e0)
     *          * BETA( (GAMMA+5e0) / 3e0, GAMMA - 1e0)
     *          / (2.0 * (GAMMA + 2.0))
        BT2K3 = DGAMMA((GAMMA+2e0)/3e0) * DGAMMA((GAMMA+7e0)/3e0)
     *          * BETA( (GAMMA+9e0) / 3e0, GAMMA - 1e0)
     *          * 3.0 / (8.0 * (GAMMA + 3.0))
        BT2K4 = DGAMMA((GAMMA+3e0)/3e0) * DGAMMA((GAMMA+8e0)/3e0)
     *          * BETA( (GAMMA+13e0) / 3e0, GAMMA - 1e0)
     *          * 5.0 / (16.0 * (GAMMA + 4.0))
 
        GKP = (GAMMA + 1.0)/6.0
        RETURN
        END
C
        SUBROUTINE SETEND(A1,SA0,SFACT,ERR,FUNC)
C
C  Finds a value A1 close to the point where ABS(FUNC(A1)-1) = ERR.
        REAL A1, SA0, SFACT, ERR, A0, FACT, T1
        LOGICAL START
 
        REAL FUNC
        EXTERNAL FUNC
 
        A0 = SA0
        FACT = SFACT
        START = .TRUE.
 
 10     CONTINUE
        IF (ABS(FACT) .LT. 0.01) RETURN
          IF (.NOT. START) FACT = 0.5 * FACT
          T1 = ABS(FUNC(A1))
C  Check for underflow of FUNC:
          IF (T1. EQ. 0.0) THEN
            A0 = A1
          ELSE IF (T1 .GT. 0.0 .AND.
     *             ABS(T1-1.0)/MIN(T1,1.0) .LT. ERR) THEN
            A0 = A1
          ELSE IF (START) THEN
            FACT = 0.5 * (A1-A0)
            START = .FALSE.
          END IF
          A1 = A0 + FACT
        GO TO 10
 
        END
C
        SUBROUTINE CHEBFT(A,B,C,N,FUNC,ERR,M)
C
C  Chebyshev fit: Given a function FUNC, lower and upper limits
C  of the interval [A,B], and a maximum degree N, this routine
C  computes the N coefficients C_k such that
C
C  FUNC(x) \approx [ \Sum_{k=1}:{N} C_k T_{k-1}(y) ] - C_1 / 2,
C
C  where y and x are related by
C
C  y = ( x - (b+a)/2 ) / ( (b-a)/2 ).
C
C  This routine is to be used with moderately large N (e.g. 30 or 50),
C  the array of C's subsequently to be truncated at the smaller value
C  m such that C_{m+1} and subsequent elements are negligible.
C
C  A "Numerical Recipes" routine. See Press et al. (1986)
C  Modified to check its own error level J.P.Leahy N.R.A.O.
C  Version 1.1 9/July/88
 
        DOUBLE PRECISION PI
        REAL TOPR
        PARAMETER (NMAX = 50, PI = 3.141592653589793D0, TOPR = 1E38)
 
        DOUBLE PRECISION SUM
        REAL C(N), F(NMAX), A, B, BMA, BPA, Y
        REAL ERR, FMIN, ER1
        INTEGER M
 
        EXTERNAL FUNC
 
        BMA = 0.5D0 * (B-A)
        BPA = 0.5D0 * (B+A)
 
C  We evaluate the function at the N zeroes of T_{N}
C  Also find minimum value.
        FMIN = TOPR
        DO 11,  K = 1,N
          Y = COS(PI*(K-0.5D0)/N)
          F(K) = FUNC(Y*BMA+BPA)
          IF (F(K) .LT. FMIN) FMIN = F(K)
 11     CONTINUE
 
C  Set maximum allowed error:
        ER1 = ERR * FMIN
 
        FAC = 2.0 / N
 
        DO 13,  J = 1,N
          SUM = 0D0
          DO 12,  K = 1,N
            SUM = SUM + F(K)*COS((PI*(J-1))*((K-0.5D0)/N))
 12       CONTINUE
          C(J) = FAC*SUM
 13     CONTINUE
 
C  Set M so that C(I) < ER1 for all I > M
        DO 10,  I = N,1,-1
          IF(ABS(C(I)) .GT. ER1) THEN
            M = I
            RETURN
          END IF
 10     CONTINUE
 
        M = 0
        RETURN
        END
C
        FUNCTION BAPR(A)
C
C  Trys out low- and high- X approximation for B(x) or JP(x)
        REAL A
        DOUBLE PRECISION X, XX, XINV
 
        INCLUDE 'PACHOL.INCL'
 
        X = 1D1**(0.5*A)
        XX = X*X
        IF (LOW) THEN
          BTEST = B1K0 + X*(B1K1 + X*B1K2)
        ELSE
          XINV = 1D0 / XX
          BTEST = 1 + XINV*(B2K1 + XINV*B2K2)
        END IF
 
        B = PACHB(A)
        BAPR = B / BTEST
 
D       WRITE(*,*) 'BAPR: XX:',XX,' PACHB:',B, ' BAPR:',BAPR
        RETURN
        END
C
        FUNCTION PACHB(XLG)
C
C  J.P.Leahy, N.R.A.O.  23/May/88
C
C  Version 0.3 1/July/88
C
C  Subroutine SETPAC must have been called before this routine.
C  When MODE = 1 in the common block PACHOL, PACHB
C  returns a value of Pacholczyk's function B(x_T,Gamma), which
C  is the spectrum of an aged, single pitch-angle population of
C  electrons with initial energy power-law GAMMA, observed at
C  scaled frequency x_T, where x_T = nu / nu_T, and nu_T is the
C  critical frequency corresponding to the most energetic
C  electrons remaining.
C  The function can be written most simply in terms of the variable
C  X = Sqrt(x_T):
C
C                         1-Gamma  /inf   2        Gamma-2
C       B(x ,Gamma) =  2 X         |   F(Z ) (Z - X)       dZ
C          T                       /X
C
C  For convenience in Chebyshev parameterization, PACHB returns
C  not B but the ratio of B to a relatively simple analytic
C  function, BSCAL:
C
C                              1-Gamma
C  Small X:  BSCAL(X,Gamma) = X
C
C                              3-2Gamma      -2
C  Large X:  BSCAL(X,Gamma) = X         exp(X  ) B2K0(Gamma)
C
C  These two modes are switched by the logical variable LOW.
C  The constant B2K0 is calculated in SETPAC
C  and passed via the common /PACHOL/.
C
C  When MODE = 2, PACHB returns a value of the Jaffe & Perola
C  spectrum, which is obtained by replacing F(X) with its
C  integral over angle, denoted FT(X). The constant B2K0 is
C  set to a different value in this case by SETPAC.
C  Also in this case the high-x scaling contains X:(2-2Gamma)
C  rather than X:(3-2Gamma).
 
        INTEGER  IER
        REAL XLG
        DOUBLE PRECISION XG1, X,
     *                   AERR, FIFTH, ERROR
 
        DOUBLE PRECISION KERNB, SQUANK
        EXTERNAL KERNB, SQUANK
 
C  Scaling parameters in /PACHOL/:
        INCLUDE 'PACHOL.INCL'
 
C  Interpret frequency: XT, RTX are in COMMON PACHOL. X is defined
C  separately so it can be passed as an argument to SQUANK.
        RTX = 1D1**(0.5D0*XLG)
        XT = RTX * RTX
        X = RTX
 
C  Calculate BSCAL:
        IF (LOW) THEN
          BSCAL = EXP(XT)
        ELSE IF (MODE .EQ. 1) THEN
          BSCAL = B2K0 * X**(-GAMMI2)
        ELSE IF (MODE .EQ. 2) THEN
          BSCAL = B2K0 * X**(-GAMMI1)
        END IF
 
        AERR = 5D-7 * ABS(BSCAL)
C  Perform integral: KERNB returns a value of the kernel of B(x_T,Gamma)
C  multiplied by exp(XT).
        B = SQUANK(X,RTINF,AERR,FIFTH,ERROR,NO,KERNB)
        IF (ERROR .GT. 3D0*B)
     *    WRITE(*,*) 'Excessive error: XT =',XT,' ERROR =', ERROR
 
C  The following factor multiplies both B and BSCAL, so we leave
C  it out:
C       XG1 = RTX**(-GAMMI1)
C       B = 2.0 * XG1 * B
 
C  Final scaled result:
        IF (BSCAL .EQ. 0.0) THEN
          PACHB = 0.0
        ELSE
          PACHB = 2.0*B/BSCAL
        END IF
 
C Debug only:
D       WRITE(UNIT = 2, FMT = 201) X*X, B, BSCAL, PACHB
D201      FORMAT(1X,1PE13.6,3E13.6)
 
        RETURN
        END
C
        DOUBLE PRECISION FUNCTION KERNB(Z)
 
        DOUBLE PRECISION Z, ZZ
        REAL FFUN, FTFUN
        EXTERNAL FFUN, FTFUN
 
        INCLUDE 'PACHOL.INCL'
 
        ZZ = Z*Z
        IF (MODE .EQ. 1) THEN
          IF (GAMMI2 .NE. 0.0) THEN
            KERNB = DEXP(XT - ZZ) * FFUN(ZZ) * (Z - RTX)**(GAMMI2)
          ELSE
            KERNB = DEXP(XT - ZZ) * FFUN(ZZ)
          END IF
        ELSE IF (MODE .EQ. 2) THEN
          IF (GAMMI2 .NE. 0.0) THEN
            KERNB = DEXP(XT - ZZ) * FTFUN(ZZ) * (Z - RTX)**(GAMMI2)
          ELSE
            KERNB = DEXP(XT - ZZ) * FTFUN(ZZ)
          END IF
        END IF
 
        RETURN
        END
C
        FUNCTION BFUN(XX)
C
C  Returns value of B(x,Gamma) (MODE = 1), or the Jaffe & Perola
C  spectrum (MODE = 2), based on Chebyshev coefficients
C  stored in common /BCC/. This is set up by SETSPC, which
C  must have been called first.
 
        DOUBLE PRECISION XX, X
        INTEGER NX
        PARAMETER (NX = 20)
        external chebev 
        REAL C1(NX), C2(NX), A1, B1, A2, B2
        INTEGER M1, M2
        COMMON /BCC/ A1, B1, C1, M1, A2, B2, C2, M2
 
        INCLUDE 'PACHOL.INCL'
 
        X = DSQRT(XX)
        XXLG = LOG10(XX)
 
C  Calculate B:
        IF (XXLG .LT. A2) THEN
          BSCAL = X**(-GAMMI1)
          IF (XXLG .LT. A1) THEN
            BFUN = BSCAL *
     *             (B1K0 + X*(B1K1 + X*(B1K2)))
          ELSE
            BFUN = BSCAL * CHEBEV(A1,B1,C1,M1,XXLG)
          END IF
        ELSE
          IF (MODE .EQ. 2) THEN
            BSCAL = B2K0 * EXP(-XX) * XX**(-GAMMI1)
          ELSE
            BSCAL = B2K0 * EXP(-XX) * X**(1.0-2.0*GAMMI1)
          END IF
          IF (XXLG .LE. B2) THEN
            BFUN = BSCAL * CHEBEV(A2,B2,C2,M2,XXLG)
          ELSE
            BFUN = BSCAL * (1 + (B2K1 + B2K2/XX)/XX)
          END IF
        END IF
 
        RETURN
        END
C
        FUNCTION BTAPR(A)
C
C  Trys out low- and high- X approximation for BT(x,Gamma)
        REAL A
        DOUBLE PRECISION X, XX, XINV
 
        INCLUDE 'PACHOL.INCL'
 
        X = 1D1**(0.5*A)
        XX = X*X
        IF (LOW) THEN
          BTEST = BT1K0 + X*(BT1K1 + X*(BT1K2))
        ELSE
          ZZ = (XX/2.0)**(-2.0/3.0)
          BTEST = (BT2K1 + ZZ*(BT2K2 + ZZ*(BT2K3 + ZZ*BT2K4)))
        END IF
 
        BT = PACHBT(A)
        BTAPR = BT / BTEST
 
D       WRITE(*,*) 'BTAPR: XX:',XX,' PACHBT:',BT, ' BTAPR:',BTAPR
        RETURN
        END
C
        FUNCTION PACHBT(XLG)
C
C  J.P.Leahy, N.R.A.O.  6/July/88
C
C  Version 0.1 26/Nov/89
C
C  Subroutine SETKP must have been called before this routine. Also the
C  setup for the parameters of routine BFUN must have been done.
C  PACHBT returns a (scaled) value of Pacholczyk's function
C  {B tilde}({x tilde}_T,Gamma) [hereafter BT(XT_T,Gamma)], which is the
C  spectrum of an aged, initially isotropic population of electrons with
C  initial energy power-law GAMMA, observed at
C  scaled frequency XT_T, where XT_T = nu / {nu tilde}_T,
C  and {nu tilde}_T is the critical frequency corresponding to the most
C  energetic electrons remaining at pitch angle pi/2. It is assumed
C  that no pitch-angle scattering occurs, and the magnetic field is
C  tangled so that the observed spectrum is integrated over all pitch
C  angles.
C  The function can be written most simply in terms of the function
C  B(x_T,Gamma), which is produced by routines PACHB and BFUN (q.v.):
C
C                        /pi/2      2Gamma     3
C       BT(XT_T,Gamma) = |    (sin t)     B(sin t x_T,Gamma) dt
C                        /0
C
C  For convenience in Chebyshev parameterization, PACHBT returns
C  not B but the ratio of B to a relatively simple analytic
C  function, BSCAL:
C
C                            (1-Gamma)/2
C  XT<1: BSCAL(XT,Gamma) = XT
C
C                              -(2Gamma+1)/3
C  XT>1: BSCAL(XT,Gamma) = (XT/2)
C
C  These two modes are switched by the logical variable LOW.
C
        INTEGER  IER
        REAL XLG
        DOUBLE PRECISION AERR, FIFTH, ERROR
 
        DOUBLE PRECISION KERNBT, SQUANK, xxx
        EXTERNAL KERNBT, SQUANK
 
C  Scaling parameters in /PACHOL/:
        INCLUDE 'PACHOL.INCL'
        INCLUDE 'GSCONSTS.INCL'
 
C  Interpret frequency: XT is in COMMON PACHOL.
        XT = 1D1**(XLG)
 
C  Calculate BSCAL:
        IF (LOW) THEN
          BSCAL = XT**(-GAMMI1/2.0)
        ELSE
          BSCAL = (XT/2.0)**(-GKP)
        END IF
 
        AERR = 1D-5 * ABS(BSCAL)
C  Perform integral: KERNBT returns a value of the kernel of
C  BT(XT,Gamma).
        xxx = 0D0
        BT = SQUANK(xxx,PIO2,AERR,FIFTH,ERROR,NO,KERNBT)
        IF (ERROR .GT. 3D0*BT)
     *    WRITE(*,*) 'Excessive error: XT =',XT,' ERROR =', ERROR
 
C  Final scaled result:
        IF (BSCAL .EQ. 0.0) THEN
          PACHBT = 0.0
        ELSE
          PACHBT = BT / BSCAL
        END IF
 
C Debug only:
D       WRITE(UNIT = 2, FMT = 201) XT, BT, BSCAL, PACHBT
D201      FORMAT(1X,1PE13.6,3E13.6)
 
        RETURN
        END
C
        DOUBLE PRECISION FUNCTION KERNBT(THETA)
C
        DOUBLE PRECISION THETA, S, BARG
        external bfun
        INCLUDE 'PACHOL.INCL'
 
        S = SIN(THETA)
        BARG = XT * S*S*S
        IF (BARG .GT. 0D0) THEN
          KERNBT = S**(2.0*(GAMMI1+1.0)) * BFUN(BARG)
        ELSE
          KERNBT = 0D0
        END IF
 
        RETURN
        END
C
        FUNCTION BTFUN(XX)
C
C  Returns value of KP spectrum based on Chebyshev coefficients
C  stored in common /BTCC/. This is set up by SETSPC, which
C  must have been called first.
 
        DOUBLE PRECISION XX, X, Z, ZZ
        INTEGER NX
        PARAMETER (NX = 20)
        external chebev
        REAL C1(NX), C2(NX), A1, B1, A2, B2
        INTEGER M1, M2
        COMMON /BTCC/ A1, B1, C1, M1, A2, B2, C2, M2
 
        INCLUDE 'PACHOL.INCL'
 
        XXLG = LOG10(XX)
 
C  Calculate B:
        IF (XXLG .LT. A2) THEN
          X = DSQRT(XX)
          BSCAL = X**(-GAMMI1)
          IF (XXLG .LT. A1) THEN
            BTFUN = BSCAL *
     *             (BT1K0 + X*(BT1K1 + X*(BT1K2)))
          ELSE
            BTFUN = BSCAL * CHEBEV(A1,B1,C1,M1,XXLG)
          END IF
        ELSE
          Z  = XX / 2D0
          BSCAL = Z**(-GKP)
          IF (XXLG .LE. B2) THEN
            BTFUN = BSCAL * CHEBEV(A2,B2,C2,M2,XXLG)
          ELSE
            ZZ = Z**(-2.0/3.0)
            BTFUN = BSCAL *
     *               (BT2K1 + ZZ*(BT2K2 + ZZ*(BT2K3 + ZZ*BT2K4)))
          END IF
        END IF
 
        RETURN
        END
C
        FUNCTION CFUN(XX)
C
C  Returns a value of the spectrum of a source with continuous
C  injection of relativistic particles plus spectral ageing.
C  Based on the spectrum with no injection [B(x,Gamma)] via
C  the following, derivable from Pacholczyk:
C
C                      1-G/2
C     C(x ,Gamma) = { x     b(0,G) - B(x ,G) } / (G - 2)
C        T             T                T
C
C  where G = Gamma + 1, and b(x_T,Gamma) is the integral part
C  of B(x_T,Gamma); ie. with the x_T:{(1-Gamma/2)} divided out.
C  The program assumes that the coefficients for B have been
C  set up (via SETSPC) with the GAMMA parameter set to G.
C
        DOUBLE PRECISION XX
        external bfun
        INCLUDE 'PACHOL.INCL'
 
        IF (XX .LT. CBMIN) THEN
          X = SQRT(XX)
          CFUN = -X**(-GAMMI2)*(B1K1 + X*(B1K2)) / GAMMI2
        ELSE
          CFUN = (B1K0*XX**(-GAMMI1/2.0) - BFUN(XX)) / GAMMI2
        END IF
 
        RETURN
        END
C
        FUNCTION CTAPR(A)
C
C  Trys out low- and high- X approximation for CT(x,Gamma)
        REAL A
        DOUBLE PRECISION X, XX, XINV
 
        INCLUDE 'PACHOL.INCL'
 
        X = 1D1**(0.5*A)
        XX = X*X
        IF (LOW) THEN
          CTEST = -(BT1K1 + X*BT1K2)
        ELSE
          ZZ = (XX/2.0)**(-2.0/3.0)
          CCT   = (BT2K1 + ZZ*(BT2K2 + ZZ*(BT2K3 + ZZ*BT2K4)))
          CTEST = (BT1K0 - 2**(GAMMI1/2.0) * (XX/2.0)**(-GKP) * CCT)
        END IF
 
        CT = PACHCT(A)
        CTAPR = CT / CTEST
 
D       WRITE(*,*) 'CTAPR: XX:',XX,' PACHCT:',CT, ' CTAPR:',CTAPR
        RETURN
        END
C
        FUNCTION PACHCT(XLG)
C
C  J.P.Leahy, N.R.A.O.  26/Nov/89
C
C  Version 0.0
C
C  Subroutine SETKP must have been called before this routine. Also the
C  setup for the parameters of routine BFUN must have been done.
C  PACHCT returns a (scaled) value of Pacholczyk's function
C  {C tilde}({x tilde}_T,Gamma) [hereafter CT(XT_T,Gamma)], which is the
C  spectrum of an aged, initially isotropic population of electrons with
C  continuous injection at an initial energy power-law GAMMA, observed at
C  scaled frequency XT_T, where XT_T = nu / {nu tilde}_T,
C  and {nu tilde}_T is the critical frequency at pitch angle pi/2.
C  It is assumed
C  that no pitch-angle scattering occurs, and the magnetic field is
C  tangled so that the observed spectrum is integrated over all pitch
C  angles.
C  The function can be written most simply in terms of the function
C  C(x_T,Gamma), which is produced by routines PACHB and CFUN (q.v.):
C
C                        /pi/2      2Gamma     3
C       CT(XT_T,Gamma) = |    (sin t)     C(sin t XT_T,Gamma) dt
C                        /0
C
C      -Gamma/2(
C  = XT_T      (c_(Gamma+1)c_9(Gamma-3)
C    ----------(
C    Gamma - 1 (       /pi/2       Gamma/2     3                  )
C                    - |     (sin t)      b(sin t XT_T,Gamma+1) dt)
C                      /0                                         )
C
C  For convenience in Chebyshev parameterization, PACHCT returns
C  not CT but the term in brackets in the last expression, scaled
C  by a simple analytic function BSCAL
C
C  XT < 10**(-1/2) : BSCAL = sqrt(XT)
C
C  XT > 10**(-1/2) : BSCAL = 1
C
C  These two modes are switched by the logical variable LOW.
C  In the set up for this function, the value of the variable
C  GAMMA is actually gamma+1, to allow multiple use of the
C  subroutine for generating the function B.
C
        INTEGER  IER
        REAL XLG
        DOUBLE PRECISION AERR, FIFTH, ERROR
 
        DOUBLE PRECISION KERNCT, SQUANK, xxx
        EXTERNAL KERNCT, SQUANK
 
C  Scaling parameters in /PACHOL/:
        INCLUDE 'PACHOL.INCL'
        INCLUDE 'GSCONSTS.INCL'
 
C  Interpret frequency: XT is in COMMON PACHOL.
        XT = 1D1**(XLG)
 
C  Calculate BSCAL:
        IF (LOW) THEN
          BSCAL = SQRT(XT)
        ELSE
          BSCAL = 1.0
        END IF
 
        AERR = 1D-5
C  Perform integral: KERNCT returns a value of the kernel of
C  cT(XT,Gamma).
        xxx = 0D0
        CT = SQUANK(xxx,PIO2,AERR,FIFTH,ERROR,NO,KERNCT)
        AERR = 1D-5*ABS(BT1K0 - CT)
        IF (ERROR .GT. 1D1*AERR)
     *     CT = SQUANK(xxx,PIO2,AERR,FIFTH,ERROR,NO,KERNCT)
        IF (ERROR .GT. 1D1*AERR)
     *    WRITE(*,*) 'Excessive error: XT =',XT,' ERROR =', ERROR
 
C  Final result:
        IF (BSCAL .EQ. 0.0) THEN
          PACHCT = 0.0
        ELSE
          PACHCT = (BT1K0 - CT) / BSCAL
        END IF
 
C Debug only:
D       WRITE(UNIT = 2, FMT = 201) XT, CT, BSCAL, PACHCT
D201      FORMAT(1X,1PE13.6,3E13.6)
 
        RETURN
        END
C
        DOUBLE PRECISION FUNCTION KERNCT(THETA)
C
        DOUBLE PRECISION THETA, S, BARG
        external bfun
        INCLUDE 'PACHOL.INCL'
 
        S = SIN(THETA)
        BARG = XT * S*S*S
        IF (BARG .GT. 0D0) THEN
          KERNCT = (BARG * S)**(GAMMI1/2.0) * BFUN(BARG)
        ELSE
          KERNCT = 0D0
        END IF
 
        RETURN
        END
C
        FUNCTION CTFUN(XX)
C
C  Returns value of CT spectrum based on Chebyshev coefficients
C  stored in common /BTCC/. This is set up by SETSPC, which
C  must have been called first.
 
        DOUBLE PRECISION XX, X, Z, ZZ
        INTEGER NX
        PARAMETER (NX = 20)
        external chebev
 
        REAL C1(NX), C2(NX), A1, B1, A2, B2
        INTEGER M1, M2
        COMMON /BTCC/ A1, B1, C1, M1, A2, B2, C2, M2
 
        INCLUDE 'PACHOL.INCL'
 
        XXLG = LOG10(XX)
        X = DSQRT(XX)
        SCALE = X**(-GAMMI1) / GAMMI2
 
C  Calculate CT:
        IF (XXLG .LT. A1) THEN
          CTFUN = - SCALE * X*(BT1K1 + X*BT1K2)
        ELSE IF (XXLG .LE. B1) THEN
          CTFUN = SCALE * X * CHEBEV(A1,B1,C1,M1,XXLG)
        ELSE IF (XXLG .LE. B2) THEN
          CTFUN = SCALE * CHEBEV(A2,B2,C2,M2,XXLG)
        ELSE
          Z  = XX / 2D0
          ZZ = Z**(-2.0/3.0)
          CTF1 = 2**(GAMMI1/2.0) * Z**(-GKP)
     *           * (BT2K1 + ZZ*(BT2K2 + ZZ*(BT2K3 + ZZ*BT2K4)))
          CTFUN = SCALE * (BT1K0 - CTF1)
        END IF
 
        RETURN
        END
C
        FUNCTION SPVAL(X)
C
C  Version 1.0 13/July/88
C
        DOUBLE PRECISION X
        external bfun, btfun, cfun, ctfun
        INCLUDE 'PACHOL.INCL'
 
        IF (IOPT .LE. 2) THEN
          SPVAL = BFUN(X)
        ELSE IF (IOPT .EQ. 3) THEN
          SPVAL = BTFUN(X)
        ELSE IF (IOPT .LE. 5) THEN
          SPVAL = CFUN(X)
        ELSE IF (IOPT .EQ. 6) THEN
          SPVAL = CTFUN(X)
        ELSE
          WRITE (*,*) '***ERROR SPVAL: Unknown IOPT:',IOPT
        END IF
 
        RETURN
        END
 
        FUNCTION C8(GAMMA)
C
C  Returns the value of Pacholczyk's function c_8:
C
C        /infty  (gamma-3)/2
C  c_8 = |      x            F(x) dx
C        /0
C  If gamma < 1/3, this diverges.
C  The analytical solution is given by Pacholczyk, p. 95.
 
        REAL GAMMA
        DOUBLE PRECISION DGAMMA
        EXTERNAL DGAMMA
 
        C8 = ((3.0*GAMMA + 7.0) / (3.0*GAMMA + 3.0))
     *       * 2.0**( (GAMMA - 3.0) / 2.0)
     *       * DGAMMA( (3.e0*GAMMA - 1.e0) / 12.e0)
     *       * DGAMMA( (3.e0*GAMMA + 7.e0) / 12.e0)
 
        END
C
        FUNCTION C9(GAMMA)
C
C  Returns the value of Pacholczyk's function c_9:
C
C        /pi/2       (gamma+3)/2
C  c_9 = |     (sin t)          dt
C        /0
 
        REAL GAMMA
        DOUBLE PRECISION DGAMMA
        EXTERNAL DGAMMA
        INCLUDE 'GSCONSTS.INCL'
 
        C9 = (RTPI/2.0) * DGAMMA( (GAMMA + 5.e0) / 4.e0)
     *       / DGAMMA( (GAMMA + 7.e0) / 4.e0)
 
        END
C
        FUNCTION C10(GAMMA)
C
C  Returns the value of Pacholczyk's function c_10.
 
        REAL GAMMA, C8
        EXTERNAL C8
 
        C10 = C8( (4.0*GAMMA + 5.0) / 3.0)
 
        END
C
        DOUBLE PRECISION FUNCTION BETA(A,B)
C
C  Returns the value of the Beta function.
 
        DOUBLE PRECISION  DGAMMA
        external dgamma        

        BETA = DGAMMA(a) * DGAMMA(b) / DGAMMA(a+b)
 
        END
 
C
        BLOCK DATA FCCS
C
C  Defines Chebyshev coefficients used by FFUN and FTFUN
C  (strictly, data statements are not safe in subroutines).
 
        INTEGER MCB
        PARAMETER (MCB = 20)
 
        INTEGER M, MT
        REAL A, B, FCB(MCB), AT, BT, FCBT(MCB)
        DOUBLE PRECISION SX(4), SXT(4)
 
        COMMON /FCC/ SX, A, B, FCB, M
        COMMON /FCCT/ SXT, AT, BT, FCBT, MT
        SAVE FCC, FCCT
 
        DATA A, B, FCB /-0.95, 1.85,
     *       3.85560257101, -0.02605966150,  0.21262274019,
     *       0.00806263957, -0.01227759889,  0.00068383132,
     *       0.00111778802, -0.00013850592, -0.00010064830,
     *       0.00001783616,  0.00000919504, -0.00000199080,
     *      -0.00000088814,  0.00000018299,  0.00000006862,
     *      -0.00000003447, -0.00000002285, -0.00000001158,
     *      -0.00000001144, -0.00000001149/
        DATA SX /0.115D0, 0.125D0, 50D0, 65D0/
        DATA M /10/
 
        DATA AT, BT, FCBT /-1.8000,  1.9200,
     *       1.956940842, -0.003222166, -0.192578545,
     *       0.002092350,  0.020535998,  0.000607187,
     *      -0.002351074, -0.000329325,  0.000250968,
     *       0.000090135, -0.000018672, -0.000018811,
     *      -0.000000885,  0.000003227,  0.000000802,
     *      -0.000000441, -0.000000235,  0.000000049,
     *       0.000000049, -0.000000008 /
        DATA SXT /1.585D-2, 2D-2, 75D0, 83D0/
        DATA MT /12/
 
 
        END
C
        FUNCTION FFUN(X)
C
C  J.P.Leahy, N.R.A.O.,  20/May/88
C  Version 2.2 11/July/88
C
C  Returns value of the function F(x), scaled by exp(x).
C  The function is defined (Pacholczyk, p. 89) as
C
C           /inf
C  F(x) = x |    K_{5/3}(z) dz
C           /x
C
C  I use analytic approximations for low and high values of
C  X, and a Chebyshev fits for X near 1. The Chebyshev fit
C  was generated by the program FCHEB. The fit is made
C  to a scaled versions of F(x), specifically to
C  F(x) / x:{0.38} e:{-x}.
C
C  The Chebyshev fit was made over lg(x) rather than x,
C  to distribute the errors evenly over lg(x).
C  To avoid glitches in the solution at the change-over points,
C  (which can confuse subsequent integration routines), the
C  results of the two approximations are averaged near each
C  change-over point.
 
C  This maximum relative error of this routine is 10:-5.
 
        DOUBLE PRECISION X, SX(4), FSCAL
 
C  Coefficients for Chebyshev approximation.
        INTEGER MCB
        PARAMETER (MCB = 20)
        INTEGER M
        REAL A, B, FCB(MCB)
        COMMON /FCC/ SX, A, B, FCB, M
C  Numerical values are loaded in block data FCCS, so force linking
C  of this module:
        EXTERNAL FCCS, chebev
 
Constants for Ginzberg & Syrovatskii approximations:
        INCLUDE 'GSCONSTS.INCL'
C  These approximations are implemented as function calls XSS & XLL
        REAL XSS, XLL
        DOUBLE PRECISION XDUMMY
C  External function for small X approximation:
        EXTERNAL XSS
C  Statement function for large X approximation:
        XLL(XDUMMY) = F2K0 * DSQRT(XDUMMY) *
     *                (1.0 + ( F2K1 + F2K2/XDUMMY ) / XDUMMY)
 
C  Begin program:
 
C  Evaluate scaling function as necessary:
        IF (X.GE.SX(1) .AND. X.LT.SX(4)) THEN
          FSCAL = X**(0.38)
          XLG = DLOG10(X)
        END IF
C  Work out X with appropriate algorithm
        IF (X .LT. SX(1)) THEN
C  Small x approximation:
          FFUN = XSS(X)
 
        ELSE IF (X .LT. SX(2)) THEN
C  Average Chebyshev(1) & XSS approximations:
          CX  = SX(2) - SX(1)
          DX1 = (X - SX(1))/CX
          DX2 = (SX(2) - X)/CX
          FFUN = DX2 * XSS(X) +
     *           DX1 * FSCAL*CHEBEV(A,B,FCB,M,XLG)
 
        ELSE IF (X .LT. SX(3)) THEN
C  Use Chebyshev. CHEBEV is a version
C  of the "Numerical recipes" function.
          FFUN = FSCAL * CHEBEV(A,B,FCB,M,XLG)
 
        ELSE IF (X .LT. SX(4)) THEN
C  Average Chebyshev and XLL:
          CX = SX(4) - SX(3)
          DX1 = (X-SX(3)) / CX
          DX2 = (SX(4)-X) / CX
          FFUN = DX2 * FSCAL*CHEBEV(A,B,FCB,M,XLG) +
     *           DX1 * XLL(X)
 
        ELSE
C  Large x approximation:
          FFUN = XLL(X)
 
        END IF
 
        RETURN
        END
C
        FUNCTION XSS(X)
C
C  Ginzberg & Syrovatskii small X approximation.
 
        DOUBLE PRECISION X, Z, Z2, Z4
 
        INCLUDE 'GSCONSTS.INCL'
 
          Z  = (X / 2D0)**(1D0/3D0)
          Z2 = Z*Z
          Z4 = Z2*Z2
          XSS = F1K0 * Z * EXP(X) *
     *          (1.0 + Z2*(F1K1 + Z4*(F1K2 + Z4*F1K3)))
        RETURN
        END
C
        FUNCTION FTFUN(X)
C
C  J.P.Leahy, N.R.A.O.,  9/June/88
C  Version 2.2 11/July/88
C
C  Returns value of the function F twiddle(x), scaled by exp(x).
C  The function is defined as:
C
C           /pi/2   2
C  FT(TX) = |    sin (t) F(TX/sin(t)) dt
C           /0
C
C  I use analytic approximations for low and high values of
C  X, and a Chebyshev fits for X near 1. The Chebyshev fit
C  was generated by the program FTCHEB. The fit is made
C  to a scaled versions of F(x), specifically F(x) / (x:(0.15) exp(-x)).
C  which takes out most of the variation in F(x), allowing the
C  Chebyshev approximation to have roughly constant fractional
C  error.
C  The Chebyshev fit was made over lg(x) rather than x,
C  to distribute the errors evenly over lg(x).
C  To avoid glitches in the solution at the change-over points,
C  (which can confuse subsequent integration routines), the
C  results of the two approximations are averaged near each
C  change-over point.
C
C  This maximum relative error of this routine is 10:-5,
C  except for X > 76, where machine underflows limit the
C  accuracy.
 
        DOUBLE PRECISION X, SX(4), Z
 
C  Coefficients for Chebyshev approximation.
        INTEGER MCB
        PARAMETER (MCB = 20)
        REAL FCB(MCB), A, B
        COMMON /FCCT/ SX, A, B, FCB, M
        EXTERNAL FCCS, chebev
 
        REAL SCALE1, SCALE2, SCALE3, XLG
        INCLUDE 'GSCONSTS.INCL'
 
C  These approximations are implemented as function calls TXSS & XLL
        REAL TXSS, XLL
        DOUBLE PRECISION XDUMMY
C  External function for small X approximation:
        EXTERNAL TXSS
C  Statement function for large X approximation:
        XLL(XDUMMY) = PIO2 * (1.0 + (FT2K1 + FT2K2/XDUMMY)/XDUMMY)
 
C  Begin program:
 
C  Evaluate scaling function as necessary:
        IF (X.GE.SX(1) .AND. X.LT.SX(4)) THEN
          XLG = DLOG10(X)
          SCALE2 = X**(0.15)
        END IF
 
C  Work out X with appropriate algorithm
        IF (X .LT. SX(1)) THEN
C  Small x approximation:
          FTFUN = TXSS(X)
 
        ELSE IF (X .LT. SX(2)) THEN
C  Average Chebyshev(1) & TXSS approximations:
          CX  = SX(2) - SX(1)
          DX1 = (X - SX(1))/CX
          DX2 = (SX(2) - X)/CX
          FTFUN = DX2 * TXSS(X) +
     *            DX1 * SCALE2*CHEBEV(A,B,FCB,M,XLG)
 
        ELSE IF (X .LT. SX(3)) THEN
C  Use Chebyshev. CHEBEV is a version
C  of the "Numerical recipes" function.
          FTFUN = SCALE2 * CHEBEV(A,B,FCB,M,XLG)
 
        ELSE IF (X .LT. SX(4)) THEN
C  Average Chebyshev and XLL:
          CX = SX(4) - SX(3)
          DX1 = (X-SX(3)) / CX
          DX2 = (SX(4)-X) / CX
          FTFUN = DX2 * SCALE2*CHEBEV(A,B,FCB,M,XLG) +
     *            DX1 * XLL(X)
 
        ELSE
C  Large x approximation:
          FTFUN = XLL(X)
 
        END IF
 
        RETURN
        END
C
        FUNCTION TXSS(X)
C
C  Small X approximation.
 
        DOUBLE PRECISION X, Z, Z2
 
        INCLUDE 'GSCONSTS.INCL'
 
        Z = (X / 2D0)**(1D0/3D0)
        Z2 = Z*Z
        TXSS = FT1K0 * Z * EXP(X) *
     *          (1 + Z2*(FT1K1 + Z2*Z2*FT1K2))
        RETURN
        END
C
        FUNCTION CHEBEV(A,B,C,M,X)
C
C  Chebyshev evaluation: All arguments are input. C is an array of
C  Chebyshev coefficients, of length M, the first M elements output
C  from CHEBFT (which must have been called with the same A and B).
C  The Chebyshev polynomial is evaluated at a point Y determined
C  from X, A, and B, and the result is returned as the function value.
 
        INTEGER M
        REAL A, B, C(M), X, D, DD, Y, Y2, SV
 
        IF ( (X-A)*(X-B) .GT. 0.0) THEN
          WRITE(*,*) '***CHEBEV: X not in range.'
          WRITE(*,*) 'X: ',X,' Lower limit: ',A,' Upper limit: ',B
          STOP 8
        END IF
 
        D = 0.0
        DD = 0.0
 
C  Change of variable:
        Y = (2.0*X - A - B) / (B-A)
        Y2 = 2.0*Y
 
C  Clenshaw's recurrence
        DO 11,  J = M,2,-1
          SV = D
          D = Y2*D - DD + C(J)
          DD = SV
 11     CONTINUE
 
C  Last step is different:
        CHEBEV = Y*D-DD+0.5*C(1) 
        RETURN
        END
c
C        a program to calculate a gamma function
c        warning: this routine has not been screened to prevent overflows.
c        Taken from numerical recipies (Press etal.) 
c        edited by c.carilli 4/90
c
	DOUBLE PRECISION FUNCTION DGAMMA(XX)
	REAL*8 COF(6),STP,HALF,ONE,FPF,X,TMP,SER,GAMMLN
	DATA COF,STP/76.18009173D0,-86.50532033D0,24.01409822D0,
     *		-1.231739516D0,.120858003D-2,-.536382D-5,2.50662827465D0/
	DATA HALF,ONE,FPF/0.5D0,1.0D0,5.5D0/
	X=XX-ONE
	TMP=X+FPF
	TMP=(X+HALF)*LOG(TMP)-TMP
	SER=ONE
	DO J=1,6
		X=X+ONE
		SER=SER+COF(J)/X
	ENDDO
        DGAMMA = STP*SER*EXP(TMP)
c        GAMMLN=TMP+LOG(STP*SER)
c        print *,' stp,ser,tmp=',stp,ser,tmp
	RETURN
	END
      DOUBLE PRECISION FUNCTION SQUANK(A,BIG,ERROR,FIFTH,RUM,NO,FUN)
C----------------------------------------------------------------------
C  An automatic quadrature (function) subroutine.
C  Stands for ``Simpson quadrature used adaptively, noise killed.''
C  Integrates FUN(x) between x=A and x=BIG.
C  ERROR is the requested absolute error.
C  RUM is an estimate of the actual error achieved.
C  FIFTH is the value of a 5th-order adjustment term.
C  NO is the number of function evaluations that were required.
C
C  The code came from CACM Algorithm #379.
C                                      - F. Schwab
C----------------------------------------------------------------------
      INTEGER*4 LEV, LEVTAG, NIM, NOM, NUM
      DOUBLE PRECISION A, ADIFF, ADIFF1, BIG, CEPS, CEPSF, CEPST, CRIT,
     *   DIFF, EFACT, EPMACH, ERROR, EST, EST1, EST2, FACERR, FIFTH,
     *   FUN, FX1, FX2, FX3, FX4, FX5, QCEPS, RUM, SIM, SUM, THIRD, X1,
     *   X2, X3, X4, X5, XZERO
      DOUBLE PRECISION FX3ST(30), X3ST(30), ESTST(30), FX5ST(30),
     *   X5ST(30), PREDIF(30)
C   EPMACH is the machine's unit roundoff.
C   16**(-13) is appropriate for the IBM, and 1.39x10**(-17) for
C   the VAX (assuming double-precision).
      SAVE EPMACH
      DATA EPMACH/-1.0D0/
      IF (EPMACH.LE.0.0D0) CALL MACHND(EPMACH)
      SUM = 0D0
      SIM = 0D0
      CEPSF = 180D0*ERROR/(BIG-A)
      CEPS = CEPSF
      ADIFF = 0D0
      LEVTAG = -1
      FACERR = 1D0
      XZERO = A
      EFACT = 0D0
      NIM = 1
      LEV = 0
      X1 = A
      X5 = BIG
      X3 = .5D0*(A+BIG)
      FX1 = FUN(X1)
      FX3 = FUN(X3)
      FX5 = FUN(X5)
      NO = 3
      EST = FX1+FX5+4D0*FX3
      IF (CEPSF) 295,205,295
 205  LEVTAG = 0
      FACERR = 15D0
      CEPS = EPMACH*DABS(FX1)
      IF (FX1) 295,210,295
 210  CEPS = EPMACH*DABS(FX3)
      LEVTAG = 3
      IF (FX3) 295,215,295
 215  CEPS = EPMACH*DABS(FX5)
      IF (FX5) 295,220,295
 220  CEPS = EPMACH
 295  QCEPS = .25D0*CEPS
 300  CONTINUE
      X2 = .5D0*(X1+X3)
      X4 = .5D0*(X3+X5)
      FX2 = FUN(X2)
      FX4 = FUN(X4)
      NO = NO+2
      EST1 = FX1+4D0*FX2+FX3
      EST2 = FX3+4D0*FX4+FX5
      ADIFF1 = ADIFF
      DIFF = EST+EST-EST1-EST2
      IF (LEV-30) 305,800,800
 305  ADIFF = DABS(DIFF)
      CRIT = ADIFF-CEPS
      IF (CRIT) 700,700,400
 400  CONTINUE
      IF (ADIFF1-ADIFF) 410,410,500
 410  IF (LEV-5) 500,415,415
 415  EFACT = EFACT+CEPS*(X1-XZERO)*FACERR
      XZERO = X1
      FACERR = 15D0
      IF (ADIFF-2D0*CEPS) 420,420,425
 420  CEPS = ADIFF
      LEVTAG = 0
      GO TO 780
 425  IF (ADIFF1-ADIFF) 435,430,435
 430  CEPS = ADIFF
      GO TO 445
 435  CEPS = 2D0*CEPS
      IF (LEVTAG-3) 440,445,445
 440  LEVTAG = 2
 445  QCEPS = .25D0*CEPS
 500  CONTINUE
      NIM = 2*NIM
      LEV = LEV+1
      ESTST(LEV) = EST2
      X3ST(LEV) = X4
      X5ST(LEV) = X5
      FX3ST(LEV) = FX4
      FX5ST(LEV) = FX5
      PREDIF(LEV) = ADIFF
      X5 = X3
      X3 = X2
      FX5 = FX3
      FX3 = FX2
      EST = EST1
      GO TO 300
 700  CONTINUE
      IF (LEV) 400,400,705
 705  IF (LEVTAG) 800,710,710
 710  CEPST = 15D0*CEPS
      IF (CRIT) 715,800,800
 715  IF (LEVTAG-2) 720,740,750
 720  IF (ADIFF) 800,800,725
 725  IF (ADIFF-QCEPS) 730,800,800
 730  IF (ADIFF-CEPSF) 770,770,735
 735  LEVTAG = 0
      CEPS = ADIFF
      EFACT = EFACT+CEPST*(X1-XZERO)
      XZERO = X1
      GO TO 445
 740  LEVTAG = 0
      IF (ADIFF) 765,765,725
 750  LEVTAG = 0
      IF (ADIFF) 775,775,730
 765  CEPS = ADIFF1
      GO TO 775
 770  LEVTAG = -1
      FACERR = 1D0
      CEPS = CEPSF
 775  EFACT = EFACT+CEPST*(X1-XZERO)
      XZERO = X1
 780  CONTINUE
      QCEPS = .25D0*CEPS
 800  CONTINUE
      SUM = SUM+(EST1+EST2)*(X5-X1)
      IF (LEVTAG) 805,810,810
 805  SIM = SIM+DIFF*(X5-X1)
 810  CONTINUE
 905  NUM = NIM/2
      NOM = NIM-2*NUM
      IF (NOM) 910,915,910
 910  NIM = NUM
      LEV = LEV-1
      GO TO 905
 915  NIM = NIM+1
      IF (LEV) 1100,1100,1000
 1000 CONTINUE
      X1 = X5
      FX1 = FX5
      X3 = X3ST(LEV)
      X5 = X5ST(LEV)
      FX3 = FX3ST(LEV)
      FX5 = FX5ST(LEV)
      EST = ESTST(LEV)
      ADIFF = PREDIF(LEV)
      GO TO 300
 1100 CONTINUE
      EFACT = EFACT+CEPS*(BIG-XZERO)*FACERR
      RUM = EFACT/180D0
      THIRD = SUM/12D0
      FIFTH = -SIM/180D0
      SQUANK = THIRD+FIFTH
      RETURN
      END
      SUBROUTINE MACHND(U)
C----------------------------------------------------------------------
C   U IS THE SMALLEST POSITIVE NUMBER SUCH THAT (1.0+U) .GT. 1.0 .
C   U IS COMPUTED APPROXIMATELY AS A POWER OF 1./2.
C
C   THIS CODE IS COMPLETELY EXPLAINED AND DOCUMENTED IN THE TEXT,
C   COMPUTER SOLUTION OF ORDINARY DIFFERENTIAL EQUATIONS:  THE INITIAL
C   VALUE PROBLEM  BY L. F. SHAMPINE AND M. K. GORDON.
C----------------------------------------------------------------------
      DOUBLE PRECISION U, HALFU, TEMP1
      HALFU = 0.5D0
   50 TEMP1 = 1.0D0 + HALFU
      IF(TEMP1 .LE. 1.0D0) GO TO 100
      HALFU = 0.5D0*HALFU
      GO TO 50
  100 U = 2.0D0*HALFU
      RETURN
      END
