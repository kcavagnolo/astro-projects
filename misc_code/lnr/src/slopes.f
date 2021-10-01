C       From Edf@Binah.astro.psu.edu Tue Feb 28 17:31:27 1995
C       To: Bershady@Binah.astro.psu.edu
C
C     *                    Program Slopes                          *
C     *                                                            *
C     *  Program To Compute The Theoretical Regression Slopes      *
C     *  And Uncertainties (Via Delta Method), And Uncertainties   *
C     *  Via Bootstrap And Bivariate Normal Simulation For A       *
C     *  (X(I),Y(I)) Data Set With Unknown Population Distribution.*
C     *                                                            *
C     *     Written By Eric Feigelson, Penn State.  June 1991      *
C     *                                                            *
C     *    Input (From A File : File Name Format=<A9>              *
C     *         The File Contains X(I), Independent Variable, And  *
C     *         Y(I) With I Up To 1000.  Format 2f10.3             *
C     *                                                            *
C     *    Output -- File `Slopes.out' (Adjustable By User)        *
C     *                                                            *
C     *    Subroutines --                                          *
C     *         Datstt(Nmax,Ntot,X,Varx,Y,Vary,Varxy,Rho),         *
C     *              Calculates Simple Statistics Of Dataset       *
C     *         Sixlin(Nmax,Ntot,X,Y,A,Siga,B,Sigb), Calculates 6  *
C     *              Linear Fits Analytically                      *
C     *         Bootsp(N,X,Y,Xsim,Ysim,Iseed), Makes Bootstrap     *
C     *              Simulated Datasets. Calls Function Ran3.      *
C     *         Tulsim (Vers. 3.2) Bivariate Normal Simulations    *
C     *         (M. T. Boswell, C. 1989, Ms-Dos Only):  
C     *                Basget                                      *
C     *                Rbnorm(X4,Y4,N,Rxy(It),Rmu,Sd)              *
C     *                Bassav                                      *
C     *                                                            *
C     A Full Description Of These Methods Can Be Found In:
C       Isobe, T., Feigelson, E. D., Akritas, M. And Babu, G. J.,
C          Linear Regression In Astronomy I, Astrophys. J. 364, 104 
C          (1990)
C       Babu, G. J. And Feigelson, E. D., Analytical And Monte Carlo
C          Comparisons Of Six Different Linear Least Squares Fits,
C          Communications In Statistics, Simulation & Computation, 
C          21, 533 (1992)
C       Feigelson, E. D. And Babu, G. J., Linear Regression In 
C          Astronomy Ii, Astrophys. J. 397, 55 (1992).
C
C
C     *  The User May Change Parameter Nmax As Needed              *
c
c     Modifcation History
C     Jun    91   EDF    Initial Release
C     Apr    95   MAB    (M. A. Bershady). Remove requirement for formatted
c                        input and increase length of characters for longer   
c                        I/O file names.
c
       Parameter (Nmax = 500)
       Implicit Real*8 (A-H,O-Z)
       character*50 ifile,ofile
       Dimension X(Nmax),Y(Nmax),Xsim(Nmax),Ysim(Nmax)
       Dimension A(6),Siga(6),B(6),Sigb(6)
       Dimension Asim(6),Sasim(6),Bsim(6),Sbsim(6),
     +           Asum(6),Assum(6),Bsum(6),Bssum(6),
     +           Aavg(6),Sda(6),Bavg(6),Sdb(6)
C        Comment Out The Following Line If Tulsim Is Unavailable
C       Real*4 Avg4(2),Sd4(2),Rho4,X4(Nmax),Y4(Nmax)
C
C     *        Read The File Name In Which A Data Set Exists.
C
       write (*,*) ' Data File : '
       read(*,*) ifile
       write (*,*) ' Output File : '
       read(*,*) ofile
C
       Open (Unit=50, File=ifile, Status='Old', Form='Formatted')
C
C     *            Read The Data Set And Open Output File
C
       I=0
   15  Continue
         I=I+1
         X(I) = 0.0
         Y(I) = 0.0
c         Read(50,14,End=25) X(I),Y(I)
         Read(50,*,End=25) X(I),Y(I)
         Goto 15
   25  Continue
       Ntot=I-1
       Close (Unit = 50)
       Open (Unit=10, File=ofile, Status='Unknown')
       Write(10,28)
       Write(10,16)
       Write(10,17)
       Write(10,28)
       Write(10,19) File,Ntot
       Write(10,28)
C
C     * Optional Output Of Data
C
       Write(10,999) (X(J),Y(J),J=1,Ntot)
C
C     *         Compute Means, Standard Deviations And Correl. Coeff.
C
       Call Datstt(Nmax,Ntot,X,Y,Xavg,Varx,Yavg,Vary,Varxy,Rho)
       Sigx = Dsqrt(Varx)
       Sigy = Dsqrt(Vary)
       Rn = Dfloat(Ntot)
       Sumxx = Varx*Rn
       Sumyy = Vary*Rn
       Sumxy = Varxy*Rn
       Write(10,28)
       Write(10,21) Xavg, Sigx, Yavg, Sigy, Sumxx, Sumyy, Sumxy, Rho
C
C     *    Compute The Six Linear Regression With Analytic Error Analysis
C
       Call Sixlin(Nmax,Ntot,X,Y,A,Siga,B,Sigb)
       Write(10,28)
       Write(10,30)
       Write(10,32)
       Write(10,28)
       Write(10,34)
       Write(10,28)
       Write(10,36) A(1),Siga(1),B(1),Sigb(1)
       Write(10,38) A(2),Siga(2),B(2),Sigb(2)
       Write(10,40) A(3),Siga(3),B(3),Sigb(3)
       Write(10,42) A(4),Siga(4),B(4),Sigb(4)
       Write(10,44) A(5),Siga(5),B(5),Sigb(5)
       Write(10,46) A(6),Siga(6),B(6),Sigb(6)
C
C     *     Make Bootstrap Simulated Datasets, And Compute Averages
C     *     And Standard Deviations Of Regression Coefficients
C
       Write(*,101)
       Read(*,102) Nsim
       Write(*,103)
       Read(*,102) Iseed
       Rnsim = Dfloat(Nsim)
       Do 100 J = 1,6
          Asum(J) = 0.0
          Assum(J) = 0.0
          Bsum(J) = 0.0
          Bssum(J) = 0.0
          Sda(J) = 0.0
          Sdb(J) = 0.0
  100  Continue
       Do 110 I = 1, Nsim
          If(I.eq.1) Iseed = -1*Iabs(Iseed)
          Call Bootsp(Nmax,Ntot,X,Y,Xsim,Ysim,Iseed)
          Call Sixlin(Nmax,Ntot,Xsim,Ysim,Asim,Sasim,Bsim,Sbsim)
C                 Optional Detailed Printout
C                 Write(10,99) I,Asim(1),Sasim(1),Bsim(1),Sbsim(1)
          Do 120 J = 1,6
             Asum(J) = Asum(J) + Asim(J)
             Assum(J) = Assum(J) + Asim(J)*Asim(J)
             Bsum(J) = Bsum(J) + Bsim(J)
             Bssum(J) = Bssum(J) + Bsim(J)*Bsim(J)
  120     Continue
  110  Continue
       Do 130 J = 1,6
          Aavg(J) = Asum(J)/Rnsim
          Sdtest = Assum(J) - Rnsim*Aavg(J)*Aavg(J)
          If(Sdtest.gt.0.0) Sda(J) = Dsqrt(Sdtest/(Rnsim-1.0))
          Bavg(J) = Bsum(J)/Rnsim
          Sdtest = Bssum(J) - Rnsim*Bavg(J)*Bavg(J)
          If(Sdtest.gt.0.0) Sdb(J) = Dsqrt(Sdtest/(Rnsim-1.0))
  130  Continue
       Write(10,28)
       Write(10,28)
       Write(10,28)
       Write(10,28)
       Write(10,105) Nsim,Iseed
       Write(10,28)
       Write(10,34)
       Write(10,28)
       Write(10,36) Aavg(1),Sda(1),Bavg(1),Sdb(1)
       Write(10,38) Aavg(2),Sda(2),Bavg(2),Sdb(2)
       Write(10,40) Aavg(3),Sda(3),Bavg(3),Sdb(3)
       Write(10,42) Aavg(4),Sda(4),Bavg(4),Sdb(4)
       Write(10,44) Aavg(5),Sda(5),Bavg(5),Sdb(5)
       Write(10,46) Aavg(6),Sda(6),Bavg(6),Sdb(6)
C
C     *     Make Jackknife Simulated Datasets And Compute Averages And
C     *     Standard Deviations Of Regression Coefficients.  Note That
C     *     The S.d. Formulae Are Different Than For The Other Estimators.
C
       Rntot = Dfloat(Ntot)
       Do 150 J = 1,6
          Asum(J) = 0.0
          Assum(J) = 0.0
          Bsum(J) = 0.0
          Bssum(J) = 0.0
          Sda(J) = 0.0
          Sdb(J) = 0.0
  150  Continue
       Njack = Ntot - 1
       Do 160 I = 1, Ntot
          Do 170 K = 1, Ntot
             If(K.eq.i) Go To 170
             If(K.lt.i) Xsim(K) = X(K)
             If(K.lt.i) Ysim(K) = Y(K)
             If(K.gt.i) Xsim(K-1) = X(K)
             If(K.gt.i) Ysim(K-1) = Y(K)
  170     Continue
          Call Sixlin(Nmax,Njack,Xsim,Ysim,Asim,Sasim,Bsim,Sbsim)
          Do 175 J = 1, 6
             Asum(J) = Asum(J) + Asim(J)
             Bsum(J) = Bsum(J) + Bsim(J)
             Assum(J) = Assum(J) + Asim(J)*Asim(J)
             Bssum(J) = Bssum(J) + Bsim(J)*Bsim(J)
  175     Continue
C                 Optional Detailed Printout
C                 Write(10,99) I,Asim(1),Sasim(1),Bsim(1),Sbsim(1)
  160  Continue
       Do 190 J = 1,6
          Aavg(J) = Asum(J)/Rntot
          Sdtest = Assum(J) - Rntot*Aavg(J)*Aavg(J)
          If(Sdtest.gt.0.0) Sda(J) = Dsqrt((Rntot-1.0)*Sdtest/Rntot)
          Bavg(J) = Bsum(J)/Rntot
          Sdtest = Bssum(J) - Rntot*Bavg(J)*Bavg(J)
          If(Sdtest.gt.0.0) Sdb(J) = Dsqrt((Rntot-1.0)*Sdtest/Rntot)
  190  Continue
       Write(10,28)
       Write(10,28)
       Write(10,28)
       Write(10,28)
       Write(10,155)
       Write(10,28)
       Write(10,34)
       Write(10,28)
       Write(10,36) Aavg(1),Sda(1),Bavg(1),Sdb(1)
       Write(10,38) Aavg(2),Sda(2),Bavg(2),Sdb(2)
       Write(10,40) Aavg(3),Sda(3),Bavg(3),Sdb(3)
       Write(10,42) Aavg(4),Sda(4),Bavg(4),Sdb(4)
       Write(10,44) Aavg(5),Sda(5),Bavg(5),Sdb(5)
       Write(10,46) Aavg(6),Sda(6),Bavg(6),Sdb(6)
C
C     *     Make Bivariate Normal Simulated Datasets And Compute Averages
C     *     And Standard Deviations Of Regression Coefficients
C     *     (Comment Out Code From Here To `Call Bassav' If There Is No
C     *     Access To Tulsim Ms-Dos Routines)
C
C       Rho4 = Rho
C       Avg4(1) = Xavg
C       Avg4(2) = Yavg
C       Sd4(1) = Sigx
C       Sd4(2) = Sigy
C       Call Basget
C       Rsum = 0.0
C       Rssum = 0.0
C       Do 200 J = 1,6
C          Asum(J) = 0.0
C          Assum(J) = 0.0
C          Bsum(J) = 0.0
C          Bssum(J) = 0.0
C          Sda(J) = 0.0
C          Sdb(J) = 0.0
C          Rsd = 0.0
C  200  Continue
C       Do 210 I = 1, Nsim
C          Call Rbnorm(X4,Y4,Ntot,Rho4,Avg4,Sd4)
C          Rsum = Rsum + Dfloat(Rho4)
C          Rssum = Rssum + Dfloat(Rho4*Rho4)
C          Do 220 L = 1, Ntot
C             Xsim(L) = Dfloat(X4(L))
C             Ysim(L) = Dfloat(Y4(L))
C  220     Continue
C          Call Sixlin(Nmax,Ntot,Xsim,Ysim,Asim,Sasim,Bsim,Sbsim)
Cc                 Optional Detailed Printout
Cc                 Write(10,99) I,Asim(1),Sasim(1),Bsim(1),Sbsim(1)
C          Do 230 J = 1,6
C             Asum(J) = Asum(J) + Asim(J)
C             Assum(J) = Assum(J) + Asim(J)*Asim(J)
C             Bsum(J) = Bsum(J) + Bsim(J)
C             Bssum(J) = Bssum(J) + Bsim(J)*Bsim(J)
C  230     Continue
C  210  Continue
C       Do 240 J = 1,6
C          Aavg(J) = Asum(J)/Rnsim
C          Sdtest = Assum(J) - Rnsim*Aavg(J)*Aavg(J)
C          If(Sdtest.gt.0.0) Sda(J) = Dsqrt(Sdtest/(Rnsim-1.0))
C          Bavg(J) = Bsum(J)/Rnsim
C          Sdtest = Bssum(J) - Rnsim*Bavg(J)*Bavg(J)
C          If(Sdtest.gt.0.0) Sdb(J) = Dsqrt(Sdtest/(Rnsim-1.0))
C  240  Continue
C       Write(10,28)
C       Write(10,28)
C       Write(10,28)
C       Write(10,28)
C       Write(10,205) Nsim
C       Write(10,28)
C       Write(10,34)
C       Write(10,28)
C       Write(10,36) Aavg(1),Sda(1),Bavg(1),Sdb(1)
C       Write(10,38) Aavg(2),Sda(2),Bavg(2),Sdb(2)
C       Write(10,40) Aavg(3),Sda(3),Bavg(3),Sdb(3)
C       Write(10,42) Aavg(4),Sda(4),Bavg(4),Sdb(4)
C       Write(10,44) Aavg(5),Sda(5),Bavg(5),Sdb(5)
C       Write(10,46) Aavg(6),Sda(6),Bavg(6),Sdb(6)
C       Write(10,28)
C       Rsum = Rsum/Rnsim
C       Sdtest = Rssum - Rnsim*Rsum*Rsum
C       If(Sdtest.gt.0.0) Rsd = Dsqrt(Sdtest/(Rnsim-1.0))
C       Write(10,207) Rsum,Rsd
C       Call Bassav
C
C     *     Input Formats
C
   10  Format(' Data File : ',$)
  101  Format('  Number Of Simulations: ')
  102  Format(Bn,I6)
  103  Format('  Seed (Integer, I6): ')
   12  Format(A9)
   14  Format(2f10.3)
C
C     *     Output Formats
C
   16  Format(6x,' Slopes:  Analytical And Simulation Calculations Of')
   17  Format(12x,'  Linear Regressions And Uncertainties')
   28  Format(' ')
   19  Format(10x, ' Input Data File:  ',A9,'  # Points:  ',I4)
   21  Format(5x,'Xavg = ',F10.5,'  +/- ',F10.5,/,
     +        5x,'Yavg = ',F12.5,'  +/- ',F10.5,/,
     +        5x,'Sxx = ',F12.5,'  Syy = ',F12.5,'  Sxy = ',F12.5,/,
     +        5x,'Pearson Correlation Coefficient = ', F10.5)
   30  Format(13x,'Six Linear Regressions: Analytical Results')
   32  Format(7x,'A = Intercept,  B = Slope,  Sd = Standard Deviation')
   34  Format(29x,'A',7x,'Sd(A)',7x,'B',7x,'Sd(B)')
   36  Format(' Ols(Y/X)          : ',4f10.3)
   38  Format(' Ols(X/Y)          : ',4f10.3)
   40  Format(' Ols Bisector      : ',4f10.3)
   42  Format(' Orthogonal        : ',4f10.3)
   44  Format(' Reduced Maj Axis  : ',4f10.3)
   46  Format(' Mean Ols          : ',4f10.3)
  105  Format(5x,'Bootstrap Simulation Results: ',I5,' Simulations With
     + Seed:',I5,/,
     +   '   (Regression Coefficient Averages And Standard Deviations)')
  155  Format(17x,'Jackknife Simulation Results: ',/,
     +   '   (Regression Coefficient Averages And Standard Deviations)')
  205  Format(7x,' Normal Simulation Results: ',I5,' Simulations',/,
     +   '   (Regression Coefficient Averages And Standard Deviations)')
  207  Format(' Average Simulated Rho And Standard Dev: ',2f8.4)
   99  Format(10x,I5,4d10.3)
  999  Format(1x,2f10.3)
C
       Stop
       End
C*******************************************************************
C************************* Subroutine Datstt ***********************
C*******************************************************************
C
       Subroutine Datstt(Nmax,N,X,Y,Xavg,Varx,Yavg,Vary,Varxy,Rho)
C
C     *  Subroutine To Compute Simple Statistical Properties Of A  *
C     *  Bivariate Data Set.  It Gives The Variance In X, Variance *
C     *  In Y, And Pearson'S Linear Correlation Coefficient.       *
C
C
       Implicit Real*8(A-H,O-Z)
       Dimension X(Nmax),Y(Nmax)
C
C
C     *         Initializations
C
       S1 = 0.0
       S2 = 0.0
       Sxx = 0.0
       Syy = 0.0
       Sxy = 0.0
       Rn = Dfloat(N)
C
C     *         Compute Averages And Sums
C
       Do 100 I=1,N
         S1 = S1 + X(I)
         S2 = S2 + Y(I)
  100  Continue
       Xavg = S1/Rn
       Yavg = S2/Rn
       Do 200 I = 1, N
       Sxx  = Sxx  + (X(I) - Xavg)**2
       Syy  = Syy  + (Y(I) - Yavg)**2
       Sxy  = Sxy  + (X(I) - Xavg)*(Y(I) - Yavg)
 200   Continue
       If(Sxy.eq.0.0) Then
         Write(*,991)
         Stop
 991   Format(5x,'Sxy .eq. Zero.  Datstt Terminated.')
       Endif
C
C     *         Compute And Return Results
C
       Varx = Sxx/Rn
       Vary = Syy/Rn
       Varxy = Sxy/Rn
       Rho = Sxy/(Dsqrt(Sxx*Syy))
C
       Return
       End
