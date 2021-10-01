C************************************************************************
C************************* Subroutine Sixlin ****************************
C************************************************************************
C
      Subroutine Sixlin(Nmax,N,X,Y,A,Siga,B,Sigb)
C
C     *                     Six Linear Regressions
C     *     Written By T. Isobe, G. J. Babu And E. D. Feigelson
C     *               Center For Space Research, M.i.t.
C     *                             And
C     *              The Pennsylvania State University
C     *
C     *                   Rev. 1.0,   September 1990
C     *
C     *       This Subroutine Provides Linear Regression Coefficients
C     *    Computed By Six Different Methods Described In Isobe,
C     *    Feigelson, Akritas, And Babu 1990, Astrophysical Journal
C     *    And Babu And Feigelson 1990, Subm. To Technometrics.
C     *    The Methods Are Ols(Y/X), Ols(X/Y), Ols Bisector, Orthogonal,
C     *    Reduced Major Axis, And Mean-Ols Regressions.
C     *
C     *    Input
C     *         X(I) : Independent Variable
C     *         Y(I) : Dependent Variable
C     *            N : Number Of Data Points
C     *
C     *    Output
C     *         A(J) : Intercept Coefficients
C     *         B(J) : Slope Coefficients
C     *      Siga(J) : Standard Deviations Of Intercepts
C     *      Sigb(J) : Standard Deviations Of Slopes
C     *     Where J = 1, 6.
C     *
C     *    Error Returns
C     *         Calculation Is Stopped When Division By Zero Will
C     *         Occur (I.e. When Sxx, Sxy, Or Either Of The Ols
C     *         Slopes Is Zero).
C
C
      Implicit Real*8(A-H,O-Z)
      Dimension X(Nmax),Y(Nmax)
      Dimension A(6),B(6),Siga(6),Sigb(6)
C     
C     *         Initializations
C
      S1 = 0.0
      S2 = 0.0
      Sxx = 0.0
      Syy = 0.0
      Sxy = 0.0
      Sum1 = 0.0
      Sum2 = 0.0
      Sum3 = 0.0
      Rn = N
      Do 50 J = 1, 6
         A(J)    = 0.0
         Siga(J) = 0.0
         B(J)    = 0.0
         Sigb(J) = 0.0
 50   Continue
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
         X(I) = X(I) - Xavg
         Y(I) = Y(I) - Yavg
         Sxx  = Sxx  + X(I)**2
         Syy  = Syy  + Y(I)**2
         Sxy  = Sxy  + X(I)*Y(I)
 200  Continue
      If(Sxy.eq.0.0) Then
         Write(*,992) (X(I),Y(I),I=1,N)
 992     Format(2f10.3)
         Write(*,991)
         Stop
 991     Format(5x,'Sxy Is Zero.  Sixlin Terminated.')
      Endif
      Sign = 1.0
      If(Sxy .lt. 0.0) Sign = -1.0
C
C     *               Compute The Slope Coefficients
C
      B(1) = Sxy/Sxx
      B(2) = Syy/Sxy
      B(3) = (B(1)*B(2) - 1.0
     +     + Dsqrt((1.0 + B(1)**2)*(1.0 + B(2)**2)))/(B(1) + B(2))
      B(4) = 0.5*(B(2) - 1.0/B(1)
     +     + Sign*Dsqrt(4.0 + (B(2) - 1.0/B(1))**2))
      B(5) = Sign*Dsqrt(B(1)*B(2))
      B(6) = 0.5*(B(1) + B(2))
C     
C     *            Compute Intercept Coefficients
C     
      Do 300 J = 1, 6
         A(J) = Yavg - B(J)*Xavg
 300  Continue
C     
C     *     Prepare For Computation Of Variances
C     
      Gam1 = B(3)/((B(1) + B(2))
     +     *Dsqrt((1.0 + B(1)**2)*(1.0 + B(2)**2)))
      Gam2 = B(4)/(Dsqrt(4.0*B(1)**2 + (B(1)*B(2) - 1.0)**2))
      Do 400 I = 1, N
         Sum1 = Sum1 + (X(I)*(Y(I) - B(1)*X(I)))**2
         Sum2 = Sum2 + (Y(I)*(Y(I) - B(2)*X(I)))**2
         Sum3 = Sum3 + X(I)*Y(I)*(Y(I) - B(1)*X(I))*(Y(I) - B(2)*X(I))
 400  Continue
      Cov = Sum3/(B(1)*Sxx**2)
C     
C     *    Compute Variances Of The Slope Coefficients
C     
      Sigb(1) = Sum1/(Sxx**2)
      Sigb(2) = Sum2/(Sxy**2)
      Sigb(3) = (Gam1**2)*(((1.0 + B(2)**2)**2)*Sigb(1)
     +     + 2.0*(1.0 + B(1)**2)*(1.0 + B(2)**2)*Cov
     +     + ((1.0 +B(1)**2)**2)*Sigb(2))
      Sigb(4) = (Gam2**2)*(Sigb(1)/B(1)**2 + 2.0*Cov
     +     + B(1)**2*Sigb(2))
      Sigb(5) = 0.25*(B(2)*Sigb(1)/B(1)
     +     + 2.0*Cov + B(1)*Sigb(2)/B(2))
      Sigb(6) = 0.25*(Sigb(1) + 2.0*Cov + Sigb(2))
C     
C     *   Compute Variances Of The Intercept Coefficients
C     
      Do 500 I = 1, N
         Siga(1) = Siga(1) + ((Y(I) - B(1)*X(I))
     +        *(1.0 - Rn*Xavg*X(I)/Sxx))**2
         Siga(2) = Siga(2) + ((Y(I) - B(2)*X(I))
     +        *(1.0 - Rn*Xavg*Y(I)/Sxy))**2
         Siga(3) = Siga(3) + ((X(I)*(Y(I)
     +        - B(1)*X(I))*(1.0 + B(2)**2)/Sxx
     +        + Y(I)*(Y(I) - B(2)*X(I))*(1.0 + B(1)**2)/Sxy)
     +        *Gam1*Xavg*Rn - Y(I) + B(3)*X(I))**2
         Siga(4) = Siga(4) + ((X(I)*(Y(I) - B(1)*X(I))/Sxx
     +        + Y(I)*(Y(I) - B(2)*X(I))*(B(1)**2)/Sxy)*Gam2
     +        *Xavg*Rn/Dsqrt(B(1)**2) - Y(I) + B(4)*X(I))**2
         Siga(5) = Siga(5) + ((X(I)*(Y(I)
     +        - B(1)*X(I))*Dsqrt(B(2)/B(1))/Sxx
     +        + Y(I)*(Y(I) - B(2)*X(I))*Dsqrt(B(1)/B(2))/Sxy)
     +        *0.5*Rn*Xavg - Y(I) + B(5)*X(I))**2
         Siga(6) = Siga(6) + ((X(I)*(Y(I) - B(1)*X(I))/Sxx
     +        + Y(I)*(Y(I) - B(2)*X(I))/Sxy)
     +        *0.5*Rn*Xavg - Y(I) + B(6)*X(I))**2
 500  Continue
C     
C     *  Convert Variances To Standard Deviations
C     
      Do 600 J = 1, 6
         Sigb(J) = Dsqrt(Sigb(J))
         Siga(J) = Dsqrt(Siga(J))/Rn
 600  Continue
C     
C     *  Return Data Arrays To Their Original Form
C
      Do 900 I = 1, N
         X(I) = X(I) + Xavg
         Y(I) = Y(I) + Yavg
 900  Continue
C     
      Return
      End
