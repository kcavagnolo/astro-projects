pro ZREGR, $
         IERROR,N,M,T,TMIN,TMAX,F, $
         WK,A,AINV,S,C,RESID,COV
;+
; The modelled regession time series T(I) (I=1,2,...,N) is given by
; (see vector notation on the RIGHT, and note that in IDL we must
; subtract 1 from all array indices):
;                           M
;                   T(I) = SUM C(J)*F(I,J)        <== T=FC         (1)
;                          J=1
; where C(J)=regression coefficient for J-th input surrogate time
; series F(I,J).
;
; Method: Least squares analysis applied to (1) yields the following
; linear matrix equation:
;                         AC = S                  <==> (F'F)C=F'T  (2)
;                             
; where C = model coefficient vector to be solved and S = data source
; vector.  It follows that
;                           N
;                   S(J) = SUM T(I)*F(I,J)        <== F'T          (3)
;                          I=1
; and                         N
;                   A(J,K) = SUM F(I,J)*F(I,K)    <== F'F          (4)
;                            I=1
; NOTE(s): The solution is (in vector notation):
;                               -1        -1
;                       C = (F'F)  F'T = A  S                      (5)
;
; The covariance matrix of the C coefficients is:
;                                     2    -1
;                          COV = sigma (F'F)                       (6)
; where
;                            2    1   N             2
;                       sigma  = --- SUM Residual(I)               (7)
;                                N-M I=1
; with Residual(I) being the original time series T(I) MINUS the time
; series T(I) modelled by (1) above.  Under the null hypothesis,
; Var(T|F)=sigma**2 I, where I is the N X N unit matrix.
;
; Input Data (all MUST be declared prior to calling ZREGR.PRO):
;   IERROR = Error flag. If set to 1, standard error analysis is
;            performed for the coefficient vector C. Setting IERROR
;            to any other integer SKIPS this
;   N = Time series length
;   M = Number of surrogate series F applied in (1)
;   T(N) = measured time series being modelled
;   TMIN,TMAX = Values of input series T that are less than TMIN
;          or greater than TMAX will NOT be included in the
;          regression (these are the flagging values)
;   F(N,M) = J-th (J=1,2,..,M) surrogate series, each of length N
; Output Data:
;   C(M) = solved coefficient vector
;   RESID(N) = Residual error time series (Original series T(I)
;              MINUS the modelled T(I) from (1) above) 
;   COV(M,M) = Covariance matrix for coefficients C (SKIPPED if
;              IERROR is not equal to 1)
; EXAMPLE:
;      IERROR=1
;      N=20 & M=7
;      T=FLTARR(N) & F=FLTARR(N,M)
;      TMIN=-1.1 & TMAX=1.1
;      FOR I=1,N DO BEGIN
;        T(I-1)=COS(2.*I)
;        F(I-1,0)=1.0
;        F(I-1,1)=COS(2.05*I)
;        F(I-1,2)=SIN(0.4*I)
;        F(I-1,3)=COS(0.6*I)
;        F(I-1,4)=SIN(0.8*I)
;        F(I-1,5)=COS(0.3*I)
;        F(I-1,6)=SIN(0.9*I)
;      ENDFOR
;      ZREGR,IERROR,N,M,T,TMIN,TMAX,F,WK,A,AINV,S,C,RESID,COV
;      PRINT,'I, T(I), T_model(I)'
;      FOR I=0,N-1 DO BEGIN
;        PRINT, I, T(I), T(I)-RESID(I)
;      ENDFOR
;
;      END
;
; Address questions/comments to:
;   Dr. Jerry Ziemke
;   NASA/Goddard Space Flight Center            PH: (301) 614-6034
;   Code 916                                   FAX: (301) 614-5903
;   Greenbelt, MD  20771
;   (Affil: UMBC GEST, Baltimore, MD)
;   e-mail: ziemke@jwocky.gsfc.nasa.gov

      WK=FLTARR(N,2*N) & A=FLTARR(M,M) & AINV=FLTARR(M,M)
      S=FLTARR(M) & C=FLTARR(M) & RESID=FLTARR(N) & COV=FLTARR(M,M)

      NGOOD=0
      FOR I=1,N DO BEGIN
        IF (T(I-1) GT TMIN) AND (T(I-1) LT TMAX) THEN NGOOD=NGOOD+1
      ENDFOR
      FOR J=1,M DO BEGIN
        S(J-1)=0.
        FOR I=1,N DO BEGIN
          IF (T(I-1) GT TMIN) AND (T(I-1) LT TMAX) THEN BEGIN
            S(J-1)=S(J-1)+T(I-1)*F(I-1,J-1)         ; <== S=F'T
          ENDIF
        ENDFOR
      ENDFOR
      ;Construct matrix A:
      FOR K=1,M DO BEGIN
        FOR J=1,M DO BEGIN
          A(J-1,K-1)=0.
          FOR I=1,N DO BEGIN
            IF (T(I-1) GT TMIN) AND (T(I-1) LT TMAX) THEN BEGIN
              A(J-1,K-1)=A(J-1,K-1)+F(I-1,J-1)*F(I-1,K-1) ; <== A=F'F
            ENDIF
          ENDFOR
        ENDFOR
      ENDFOR
      ;Solve for coef vector C:
      IPRINT=0  ; ==> If 1, print out solution each CALL
      ICHECK=0  ; ==> If 1, print out error analysis each CALL
      FOR I=1,M DO BEGIN
        FOR J=1,M DO BEGIN
          WK(I-1,J-1)=A(I-1,J-1)
        ENDFOR
      ENDFOR
      FOR I=1,M DO BEGIN
        FOR J=M+1,2*M DO BEGIN
          IF M+I EQ J THEN BEGIN
            WK(I-1,J-1)=1.0
          ENDIF ELSE BEGIN
            WK(I-1,J-1)=0.0
          ENDELSE
        ENDFOR
      ENDFOR
      FOR K=1,M DO BEGIN
        FOR I=1,M DO BEGIN
          IF I NE K THEN BEGIN
            IF WK(K-1,K-1) EQ 0 THEN BEGIN
              L=1
JMP1:           IF WK(L-1,K-1) EQ 0 THEN BEGIN
                L=L+1
                GOTO, JMP1
              ENDIF
              FOR J=K,2*M DO BEGIN
                WK(K-1,J-1)=WK(K-1,J-1)+WK(L-1,J-1)
              ENDFOR
            ENDIF
            U=-WK(I-1,K-1)/WK(K-1,K-1)
            FOR J=K+1,2*M DO BEGIN
              WK(I-1,J-1)=WK(I-1,J-1)+U*WK(K-1,J-1)
            ENDFOR
          ENDIF
        ENDFOR
      ENDFOR
      FOR J=1,M DO BEGIN
        FOR I=1,M DO BEGIN
          AINV(I-1,J-1)=WK(I-1,J+M-1)/WK(I-1,I-1)
        ENDFOR
      ENDFOR
      IF  IPRINT EQ 1 THEN BEGIN
        FOR J=1,M DO BEGIN
          PRINT,'Column ',J,' of inverse matrix:',format='(1X,A7,I3,A19)'
          FOR I=1,M DO BEGIN
            print, A(I-1,J-1)
          ENDFOR
        ENDFOR
      ENDIF
      IF ICHECK EQ 1 THEN BEGIN
        print,'-----------------------------------------------'
        print,'Simple error check by multiplying A by AINV:'
        print,'(this should ideally yield the identity matrix)'
        FOR J=1,M DO BEGIN
          print,'Column ',J,' of product matrix A*AINV:', $
                format='(1X,A7,I3,A27)'
          FOR I=1,M DO BEGIN
            SUM=0.
            FOR K=1,M DO BEGIN
              SUM=SUM+A(I-1,K-1)*AINV(K-1,J-1)
            ENDFOR
            print, SUM
          ENDFOR
        ENDFOR
      ENDIF
      ;Coefficient vector solution:
      FOR I=1,M DO BEGIN
        C(I-1)=0.
        FOR J=1,M DO BEGIN
          C(I-1)=C(I-1)+AINV(I-1,J-1)*S(J-1)
        ENDFOR
      ENDFOR
      IF IERROR EQ 1 THEN BEGIN
        FOR I=1,N DO BEGIN
          IF (T(I-1) GT TMIN) AND (T(I-1) LT TMAX) THEN BEGIN
            SUM=0
            FOR J=1,M DO BEGIN
              SUM=SUM+C(J-1)*F(I-1,J-1)
            ENDFOR
            RESID(I-1)=T(I-1)-SUM
          ENDIF
        ENDFOR
        SUM=0
        FOR I=1,N DO BEGIN
          IF (T(I-1) GT TMIN) AND (T(I-1) LT TMAX) THEN SUM=SUM+RESID(I-1)^2
        ENDFOR
        SIGSQR=SUM/(NGOOD-M)
        FOR K=1,M DO BEGIN
          FOR J=1,M DO BEGIN
            COV(J-1,K-1)=SIGSQR*AINV(J-1,K-1)
          ENDFOR
        ENDFOR
      ENDIF

      RETURN
      END
