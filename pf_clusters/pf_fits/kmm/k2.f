        program KMM
c	this version writes p-value to screen


C     This is an updated version (October, 1987) of the program KMM
C     in the appendix of the book by McLachlan and Basford.
C     It writes out the log likelihood for NG=1 as well as testing
C     against the specified number of groups.
C     Program for fitting a mixture of normal distributions with either
C     equal or arbitrary covariance matrices to two-mode two-way data.
C     The first mode is taken to be the class of entities to be
C     clustered.  The second mode is a class of some characteristics
C     of the entities, hereafter referred to as the attributes.
C
C     The input file contains the following:
C     NIND NATT           (number of entities and attributes)
C     (X(I,J),J=1,NATT)
C     ------------------  (NIND rows of data each having
C     ------------------   NATT values per line)
C     ------------------
C     NG                  (number of groups)
C     NCOV                (1 = Equal covariance matrices;
C                          2 = Arbitrary covariance matrices)
C     NSWH                (1 = Initially specified partition
C                              of entities into NG groups;
C                          2 = Initially specified estimates of
C                              unknown parameters for the NG groups)
C                   If NSWH = 1
C     (IDT(I),I=1,NIND)   (initial grouping of the entities)
C        This allows free field input but for ease of reading use
C        ten numbers per line for I=1,NIND.
C     NOTE:  The initial partition of the entities requires at
C            least NATT+1 entities to be assigned to each group.
C                or if NSWH = 2
C     (XBAR(K,J),J=1,NATT)   (estimated mean vector for each group)
C     ------------------
C     (XVAR(K,I,J),J=1,NATT) (estimated covariance matrix for each
C     ------------------      group with NATT rows for each matrix)
C     ------------------     If NCOV = 1 there is only one matrix
C     ------------------
C     (T(K),K=1,NG)       (estimated mixing proportion for each group)
C
C     The output file contains the results of fitting the above model
C     starting from an initially specified partition of the data or
C     initially specified estimates of the unknown parameters.
C     It includes the final probabilistic clustering and the
C     consequent partition of the NIND entities into NG groups.
C
C     This section of the program calls the subroutines and then
C     writes out some summary results.
      character*40 infile,outfile
      DIMENSION X(2000,10),XVAR(10,10,10),T(10),DV(10),V(10,10,10),
     1XMU(10,10),XML(10)
c
c	open input and output files
c
	write(6,*) 'Enter input filename: '
	read(5,*) infile
	open(21,file=infile)
	rewind 21
	write(6,*) 'No output file -- this writes p-value to screen!'
	
      CALL GIN(NIND,NATT,NG,X,XMU,V,DV,T,NCOV,IER)
      IF (IER.NE.0) GO TO 605
      CALL LOOP1(NIND,NATT,1,X,NCOV,IER,XML)
      IF (IER.NE.0) GO TO 605
C     This subroutine calculates the log likelihood for NG=1 and stores
C     it in XML(1)
c     WRITE (22,611) XML(1)
611   FORMAT (//2X,'With one group the log likelihood is ',F15.3)
      CALL LOOP(NIND,NATT,NG,X,XMU,V,XVAR,DV,T,NCOV,IER,XML)
      IF (IER.NE.0) GO TO 605
c     WRITE (22,106)
106   FORMAT (//2X,'Estimated mean (as a row vector) for each group')
      DO 20 K=1,NG
20    CONTINUE
103   FORMAT (2X,10F16.6)
C     Test if a common covariance matrix is specified (NCOV = 1)
      IF (NCOV.EQ.1) GO TO 600
      DO 30 K=1,NG
c     WRITE (22,105) K
105   FORMAT (//2X,'Estimated covariance matrix for group ',I2)
      DO 30 J=1,NATT
30    CONTINUE
104   FORMAT (5X,10F16.6)
      GO TO 609
600   CONTINUE
107   FORMAT (//2X,'Estimated common covariance matrix ')
      DO 40 J=1,NATT
40    CONTINUE
      GO TO 609
605   CONTINUE
101   FORMAT (//2X,'Terminal error in MATINV as IERR = ',I6)
      GO TO 610
609   CONTINUE
      CALL STR(NG,XML,NATT,NCOV)
610   STOP
      END


      SUBROUTINE GIN(NIND,NATT,NG,X,XBAR,V,DV,T,NCOV,IER)
C     This subroutine reads the data and depending on the value of NSWH
C     either (i) calculates estimates of the mean, covariance matrix
C                and mixing proportion for each group, corresponding
C                to the initial partition of the NIND entities into
C                NG groups
C     or    (ii) reads initial estimates of the unknown parameters.
      DIMENSION X(2000,10),XBAR(10,10),XVAR(10,10,10),IDT(2000),N(10),
     1T(10),V(10,10,10),AL(10),DV(10),XV(10,10),XSUM(10,10)
      READ (21,*) NIND,NATT
c       write (9,*) nind,natt   ! output for hawke/hawku
      DO 10 I=1,NIND
      READ (21,*) (X(I,J),J=1,NATT)
c10      write (9,*) (x(i,j),j=1,natt)
10	continue
      READ (21,*) NG
c        write (9,*) ng
      READ (21,*) NCOV
      READ (21,*) NSWH
      IF (NSWH.EQ.2) GO TO 200
C     This section is appropriate for NSWH = 1
      READ (21,*) (IDT(I),I=1,NIND)
c     WRITE (22,109)
109   FORMAT (//2X,'Initial partition as specified by input')
c     WRITE (22,110) (IDT(I),I=1,NIND)
110   FORMAT (2X,10I4)
C     Compute estimates of mean, covariance matrix and mixing
C     proportion for each group
71    DO 15 K=1,NG
      N(K)=0
      DO 15 J=1,NATT
      XBAR(K,J)=0.0
      DO 15 I=1,NATT
15    XVAR(K,I,J)=0.0
c     WRITE (22,106)
106   FORMAT (//2X,'Estimated mean (as a row vector) for each group')
      DO 20 JJ=1,NIND
      K=IDT(JJ)
      N(K)=N(K)+1
      DO 20 J=1,NATT
      XBAR(K,J)=XBAR(K,J)+X(JJ,J)
      DO 20 I=1,J
20    XVAR(K,I,J)=XVAR(K,I,J)+X(JJ,J)*X(JJ,I)
      DO 35 K=1,NG
      DO 30 J=1,NATT
30    XBAR(K,J)=XBAR(K,J)/N(K)
      T(K)=N(K)
      T(K)=T(K)/FLOAT(NIND)
35    CONTINUE
103   FORMAT (2X,10F15.6)
C     Compute estimate of covariance matrix for each group
      DO 60 K=1,NG
      DO 50 J=1,NATT
      DO 50 I=1,J
      XVAR(K,I,J)=XVAR(K,I,J)-N(K)*XBAR(K,J)*XBAR(K,I)
      XVAR(K,I,J)=XVAR(K,I,J)/FLOAT(N(K)-1)
50    XVAR(K,J,I)=XVAR(K,I,J)
c     WRITE (22,105) K
105   FORMAT (//2X,'Estimated covariance matrix for group ',I4)
      DO 60 J=1,NATT
60    CONTINUE
C     Test if a common covariance matrix is specified (NCOV = 1)
      IF (NCOV.EQ.1) GO TO 600
C     Obtain inverse and determinant of each estimated covariance
C     matrix
      CALL GDET(NATT,NG,XVAR,V,DV,IER)
      IF (IER.NE.0) RETURN
      GO TO 412
600   CONTINUE
C     Compute pooled estimate of common covariance matrix
      DO 875 J=1,NATT
      DO 875 I=1,J
      XV(I,J)=0.0
      DO 874 K=1,NG
874   XV(I,J)=XV(I,J)+(N(K)-1)*XVAR(K,I,J)
      XVAR(1,I,J)=XV(I,J)/FLOAT(NIND-NG)
875   XVAR(1,J,I)=XVAR(1,I,J)
c     WRITE (22,115)
115   FORMAT (//2X,'Estimated common covariance matrix ')
      DO 878 J=1,NATT
878   CONTINUE
C     Obtain inverse of this matrix
      CALL GDET(NATT,1,XVAR,V,DV,IER)
      IF (IER.NE.0) RETURN
      DO 411 K=2,NG
      DV(K)=DV(1)
      DO 411 J=1,NATT
      DO 411 I=1,NATT
      XVAR(K,I,J)=XVAR(1,I,J)
411   V(K,I,J)=V(1,I,J)
412   CONTINUE
c     WRITE (22,112)
112   FORMAT (//2X,'Proportion from each group as specified by input')
c     WRITE (22,111) (T(K),K=1,NG)
111   FORMAT (5X,10F7.3)
      RETURN
200   CONTINUE
C     This section is appropriate for NSWH = 2
c     WRITE (22,106)
206   FORMAT (//2X,'Estimated mean (as a row vector) for each group')
      DO 80 K=1,NG
      READ (21,*) (XBAR(K,J),J=1,NATT)
80    CONTINUE
C     Test if a common covariance matrix is specified (NCOV = 1)
      IF (NCOV.EQ.1) GO TO 700
      DO 90 K=1,NG
c     WRITE (22,107) K
107   FORMAT (//2X,'Estimated covariance matrix for group ',I2)
      DO 90 J=1,NATT
      READ (21,*) (XVAR(K,I,J),I=1,J)
      DO 85 I=1,J
85    XVAR(K,J,I)=XVAR(K,I,J)
90    continue
      CALL GDET(NATT,NG,XVAR,V,DV,IER)
      IF (IER.NE.0) RETURN
      GO TO 812
700   CONTINUE
C     Read estimated common covariance matrix
c     WRITE (22,113)
113   FORMAT (//2X,'Estimated common covariance matrix ')
      DO 95 J=1,NATT
      READ (21,*) (XVAR(1,I,J),I=1,J)
      DO 93 I=1,J
93    XVAR(1,J,I)=XVAR(1,I,J)
95     CONTINUE
      CALL GDET(NATT,1,XVAR,V,DV,IER)
      IF (IER.NE.0) RETURN
      DO 96 K=2,NG
      DV(K)=DV(1)
      DO 96 J=1,NATT
      DO 96 I=1,NATT
      XVAR(K,I,J)=XVAR(1,I,J)
96    V(K,I,J)=V(1,I,J)
812   CONTINUE
C     Read estimated mixing proportion for each group
      READ (21,*) (T(K),K=1,NG)
c     WRITE (22,108)
108   FORMAT (//2X,'Estimated mixing proportion for each group')
c     WRITE (22,111) (T(K),K=1,NG)
      RETURN
      END


      SUBROUTINE GDET(NATT,NG,XVAR,V,DV,IER)
C     This subroutine reads all covariance matrices, then calls
C     MATINV, which inverts a matrix and calculates its determinant,
C     for each covariance matrix in turn.
      DIMENSION XVAR(10,10,10),V(10,10,10),DV(10),E(10,10),IP(10)
      DO 40 K=1,NG
      TOL=0.0
      DO 20 J=1,NATT
      TOL=TOL+SQRT(XVAR(K,J,J))
      DO 20 I=1,J
      E(I,J)=XVAR(K,I,J)
20    E(J,I)=E(I,J)
      TOL=(TOL/NATT)*0.0000001
      CALL MATINV(E,NATT,NATT,10,DET,IP,TOL,IER)
      IF (IER.NE.0) RETURN
      DO 35 J=1,NATT
      DO 35 I=1,J
      V(K,I,J)=E(I,J)
      V(K,J,I)=V(K,I,J)
35    CONTINUE
      DV(K)=DET
40    CONTINUE
      END


      SUBROUTINE LOOP1(NIND,NATT,NG,X,NCOV,IER,XML)
C     This subroutine uses the EM algorithm from a specified starting
C     value to find a soltuion of the likelihood equation.
C     It is called from the main program with NG = 1
      DIMENSION X(2000,10),V(10,10,10),AL(10),DV(10),T(10),N(10),
     1W(2000,10),WSUM(10),SUM(10,10),V1(10,10,10),XVAR(10,10,10),
     2IDT(2000),XLOGL(250),XCC(10),XMU(10,10),XV(10,10),XML(10)
      PI=3.141592653
      CALL GIN1(NIND,NATT,NG,X,IDT,XMU,V,DV,T,NCOV,IER)
      IOUNT=1
1111  CONTINUE
      XLOGL(IOUNT)=0.0
      DO 205 JJ=1,NIND
      GUM=0.0
      DO 800 K=1,NG
      AL(K)=0.0
      DO 805 I=1,NATT
      DO 805 J=1,NATT
      AL(K)=AL(K)+(X(JJ,I)-XMU(K,I))*V(K,I,J)*(X(JJ,J)-XMU(K,J))
805   CONTINUE
      IF (AL(K).GT.75.0) GO TO 8055
      AL(K)=-0.5*AL(K)
      AL(K)=EXP(AL(K))/(SQRT(DV(K))*(2.*PI)**(NATT/2.0))
      GO TO 898
8055  AL(K)=0.0
898   CONTINUE
      C=1.0E-30
      IF (T(K).LT.C.OR.AL(K).LT.C) GO TO 800
      GUM=GUM+T(K)*AL(K)
800   CONTINUE
C     Compute new estimates of posterior probabilities of group
C     membership (W) and then log likelihood
      IF (GUM.EQ.0.0) GO TO 8110
      DO 810 K=1,NG
      IF (T(K).LT.C.OR.AL(K).LT.C) GO TO 901
      W(JJ,K)=T(K)*AL(K)/GUM
      GO TO 810
901   W(JJ,K)=0.0
810   CONTINUE
      XLOGL(IOUNT)=XLOGL(IOUNT)+ALOG(GUM)
      GO TO 205
8110  DO 8101 K=1,NG
8101  W(JJ,K)=0.0
205   CONTINUE
C     Test for exit from loop
      IF (IOUNT.LE.10) GO TO 230
      LAST=IOUNT-10
      ALIM=0.00001*XLOGL(LAST)
      DIFF=XLOGL(IOUNT)-XLOGL(LAST)
      IF (ABS(DIFF).LE.ABS(ALIM)) GO TO 990
      IF (IOUNT.GT.249) GO TO 985
230   CONTINUE
C     Compute new estimate for the mixing proportion (T) for each group
      DO 666 K=1,NG
      WSUM(K)=0.0
      DO 667 JJ=1,NIND
      WSUM(K)=WSUM(K)+W(JJ,K)
667   CONTINUE
      T(K)=WSUM(K)/NIND
666   CONTINUE
C     Compute new estimate of group means (XMU)
      DO 671 K=1,NG
      DO 671 J=1,NATT
      SUM(K,J)=0.0
      DO 670 JJ=1,NIND
      SUM(K,J)=SUM(K,J)+X(JJ,J)*W(JJ,K)
670   CONTINUE
      XMU(K,J)=SUM(K,J)/WSUM(K)
671   CONTINUE
C     Compute new estimate of covariance matrix for each group
      DO 870 K=1,NG
      DO 871 J=1,NATT
      DO 871 I=1,J
      V(K,I,J)=0.0
871   CONTINUE
      DO 872 JJ=1,NIND
      DO 873 J=1,NATT
      DO 873 I=1,J
      V(K,I,J)=V(K,I,J)+(X(JJ,I)-XMU(K,I))*(X(JJ,J)-XMU(K,J))*W(JJ,K)
873   CONTINUE
872   CONTINUE
      DO 673 J=1,NATT
      DO 673 I=1,J
      V(K,I,J)=V(K,I,J)/WSUM(K)
      V(K,J,I)=V(K,I,J)
673   CONTINUE
870   CONTINUE
C     If NCOV = 1 compute estimate of common covariance matrix
      IF (NCOV.EQ.1) GO TO 600
C     Obtain inverse and determinant of each covariance matrix
      CALL GDET(NATT,NG,V,V1,DV,IER)
      IF (IER.NE.0) RETURN
      DO 410 K=1,NG
      DO 410 J=1,NATT
      DO 410 I=1,NATT
      XVAR(K,J,I)=V(K,J,I)
410   V(K,J,I)=V1(K,J,I)
      GO TO 412
600   CONTINUE
C     Compute estimate of common covariance matrix
      DO 875 J=1,NATT
      DO 875 I=1,J
      XV(I,J)=0.0
      DO 874 K=1,NG
874   XV(I,J)=XV(I,J)+V(K,I,J)*WSUM(K)
      V(1,I,J)=XV(I,J)/NIND
875   V(1,J,I)=V(1,I,J)
C     Obtain inverse of this matrix
      CALL GDET(NATT,1,V,V1,DV,IER)
      IF (IER.NE.0) RETURN
      DO 411 K=1,NG
      DV(K)=DV(1)
      DO 411 J=1,NATT
      DO 411 I=1,NATT
411   XVAR(K,I,J)=V(1,I,J)
      DO 413 K=1,NG
      DO 413 J=1,NATT
      DO 413 I=1,NATT
413   V(K,I,J)=V1(1,I,J)
412   CONTINUE
      IOUNT=IOUNT+1
      GO TO 1111
985   CONTINUE
      WRITE (6,115)
115   FORMAT (//2X,'Note: This sample did not converge in 250 ',
     1'iterations.',/8X,'However the program continues using the ',
     2'last cycle estimates.')
990   CONTINUE
      XML(NG)=XLOGL(IOUNT)
      RETURN
      END


      SUBROUTINE GIN1(NIND,NATT,NG,X,IDT,XBAR,V,DV,T,NCOV,IER)
C     This subroutine calculates estimates of group means, covariance
C     matrices and proportions for the special case of NG = 1 as this
C     is specified in the calling sequence from LOOP1
      DIMENSION X(2000,10),XBAR(10,10),XVAR(10,10,10),IDT(2000),N(10),
     1T(10),V(10,10,10),AL(10),DV(10),XV(10,10),XSUM(10,10)
      DO 11 I=1,NIND
11    IDT(I)=1
12    CONTINUE
C     Compute estimates of means, covariances and proportions
C     for each group
71    DO 15 K=1,NG
      N(K)=0
      DO 15 J=1,NATT
      XBAR(K,J)=0.0
      DO 15 I=1,NATT
15    XVAR(K,I,J)=0.0
      DO 20 JJ=1,NIND
      K=IDT(JJ)
      N(K)=N(K)+1
      DO 20 J=1,NATT
      XBAR(K,J)=XBAR(K,J)+X(JJ,J)
      DO 20 I=1,J
20    XVAR(K,I,J)=XVAR(K,I,J)+X(JJ,J)*X(JJ,I)
      DO 35 K=1,NG
      DO 30 J=1,NATT
30    XBAR(K,J)=XBAR(K,J)/N(K)
      T(K)=N(K)
      T(K)=T(K)/FLOAT(NIND)
35    CONTINUE
C     Compute estimate of covariance matrix for each group
      DO 60 K=1,NG
      DO 50 J=1,NATT
      DO 50 I=1,J
      XVAR(K,I,J)=XVAR(K,I,J)-N(K)*XBAR(K,J)*XBAR(K,I)
      XVAR(K,I,J)=XVAR(K,I,J)/FLOAT(N(K)-1)
50    XVAR(K,J,I)=XVAR(K,I,J)
60    CONTINUE
C     Test if a common covariance matrix is specified (NCOV = 1)
      IF (NCOV.EQ.1) GO TO 600
C     Obtain inverse and determinant of each estimated covariance
C     matrix
      CALL GDET(NATT,NG,XVAR,V,DV,IER)
      IF (IER.NE.0) RETURN
      GO TO 412
600   CONTINUE
C     Compute pooled estimate of common covariance matrix
      DO 875 J=1,NATT
      DO 875 I=1,J
      XV(I,J)=0.0
      DO 874 K=1,NG
874   XV(I,J)=XV(I,J)+(N(K)-1)*XVAR(K,I,J)
      XVAR(1,I,J)=XV(I,J)/FLOAT(NIND-NG)
875   XVAR(1,J,I)=XVAR(1,I,J)
C     Obtain inverse of this matrix
      CALL GDET(NATT,1,XVAR,V,DV,IER)
      IF (IER.NE.0) RETURN
      DO 411 K=2,NG
      DV(K)=DV(1)
      DO 411 J=1,NATT
      DO 411 I=1,NATT
      XVAR(K,I,J)=XVAR(1,I,J)
411   V(K,I,J)=V(1,I,J)
412   CONTINUE
      RETURN
      END


      SUBROUTINE LOOP(NIND,NATT,NG,X,XMU,V,XVAR,DV,T,NCOV,IER,XML)
C     This subroutine uses the EM algorithm from a specified starting
C     value to find a solution of the likelihood equation.
      DIMENSION X(2000,10),V(10,10,10),AL(10),DV(10),T(10),N(10),
     1W(2000,10),WSUM(10),SUM(10,10),V1(10,10,10),XVAR(10,10,10),
     2IDT(2000),XLOGL(250),XCC(10),XMU(10,10),XV(10,10),XML(10)
      PI=3.141592653
      IOUNT=1
1111  CONTINUE
C     Compute the log likelihood
      XLOGL(IOUNT)=0.0
      DO 205 JJ=1,NIND
      GUM=0.0
      DO 800 K=1,NG
      AL(K)=0.0
      DO 805 I=1,NATT
      DO 805 J=1,NATT
      AL(K)=AL(K)+(X(JJ,I)-XMU(K,I))*V(K,I,J)*(X(JJ,J)-XMU(K,J))
805   CONTINUE
      IF (AL(K).GT.75.0) GO TO 8055
      AL(K)=-0.5*AL(K)
      AL(K)=EXP(AL(K))/(SQRT(DV(K))*(2.*PI)**(NATT/2.0))
      GO TO 898
8055  AL(K)=0.0
898   CONTINUE
      C=1.0E-30
      IF (T(K).LT.C.OR.AL(K).LT.C) GO TO 800
      GUM=GUM+T(K)*AL(K)
800   CONTINUE
C     Compute current estimates of posterior probabilities of group
C     membership (W)
      IF (GUM.EQ.0.0) GO TO 8110
      DO 810 K=1,NG
      IF (T(K).LT.C.OR.AL(K).LT.C) GO TO 901
      W(JJ,K)=T(K)*AL(K)/GUM
      GO TO 810
901   W(JJ,K)=0.0
810   CONTINUE
      XLOGL(IOUNT)=XLOGL(IOUNT)+ALOG(GUM)
      GO TO 205
8110  DO 8101 K=1,NG
8101  W(JJ,K)=0.0
c      WRITE(22,116) IOUNT,JJ
c116   FORMAT (/2X,'In loop ',I3,' the estimated mixture density is ',
c     1'zero ',/4X,'for the observation on entity ',I6)
205   CONTINUE
      WRITE (*,9001) IOUNT,XLOGL(IOUNT)
9001  FORMAT (2X,'Value of IOUNT is',I4,' with loglikelihood ',F15.3)
C     Test for exit from loop
      IF (IOUNT.LE.10) GO TO 230
      LAST=IOUNT-10
      ALIM=0.00001*XLOGL(LAST)
      DIFF=XLOGL(IOUNT)-XLOGL(LAST)
      IF (ABS(DIFF).LE.ABS(ALIM)) GO TO 990
      IF (IOUNT.GT.249) GO TO 985
230   CONTINUE
C     Compute new estimate of mixing proportion (T) for each group
      DO 666 K=1,NG
      WSUM(K)=0.0
      DO 667 JJ=1,NIND
      WSUM(K)=WSUM(K)+W(JJ,K)
667   CONTINUE
      T(K)=WSUM(K)/NIND
666   CONTINUE
C     Compute new estimates of group means (XMU)
      DO 671 K=1,NG
      DO 671 J=1,NATT
      SUM(K,J)=0.0
      DO 670 JJ=1,NIND
      SUM(K,J)=SUM(K,J)+X(JJ,J)*W(JJ,K)
670   CONTINUE
      XMU(K,J)=SUM(K,J)/WSUM(K)
671   CONTINUE
C     Compute new estimate of covariance matrix for each group
      DO 870 K=1,NG
      DO 871 J=1,NATT
      DO 871 I=1,J
      V(K,I,J)=0.0
871   CONTINUE
      DO 872 JJ=1,NIND
      DO 873 J=1,NATT
      DO 873 I=1,J
      V(K,I,J)=V(K,I,J)+(X(JJ,I)-XMU(K,I))*(X(JJ,J)-XMU(K,J))*W(JJ,K)
873   CONTINUE
872   CONTINUE
      DO 673 J=1,NATT
      DO 673 I=1,J
      V(K,I,J)=V(K,I,J)/WSUM(K)
      V(K,J,I)=V(K,I,J)
673   CONTINUE
870   CONTINUE
C     Test if a common covariance matrix is specified (NCOV = 1)
      IF (NCOV.EQ.1) GO TO 600
C     Obtain inverse and determinant of each estimated covariance
C     matrix
      CALL GDET(NATT,NG,V,V1,DV,IER)
      IF (IER.NE.0) RETURN
      DO 410 K=1,NG
      DO 410 J=1,NATT
      DO 410 I=1,NATT
      XVAR(K,J,I)=V(K,J,I)
410   V(K,J,I)=V1(K,J,I)
      GO TO 412
600   CONTINUE
C     Compute new estimate of common covariance matrix
      DO 875 J=1,NATT
      DO 875 I=1,J
      XV(I,J)=0.0
      DO 874 K=1,NG
874   XV(I,J)=XV(I,J)+V(K,I,J)*WSUM(K)
      V(1,I,J)=XV(I,J)/NIND
875   V(1,J,I)=V(1,I,J)
C     Obtain inverse of this matrix
      CALL GDET(NATT,1,V,V1,DV,IER)
      IF (IER.NE.0) RETURN
      DO 411 K=1,NG
      DV(K)=DV(1)
      DO 411 J=1,NATT
      DO 411 I=1,NATT
411   XVAR(K,I,J)=V(1,I,J)
      DO 413 K=1,NG
      DO 413 J=1,NATT
      DO 413 I=1,NATT
413   V(K,I,J)=V1(1,I,J)
412   CONTINUE
      IOUNT=IOUNT+1
      GO TO 1111
985   CONTINUE
c     WRITE (22,115)
115   FORMAT (//2X,'Note: This sample did not converge in 250 ',
     1'iterations.',/8X,'However the program will continue to ',
     2'print results ',/8X,'obtained from the last cycle estimates.')
990   CONTINUE
      XML(NG)=XLOGL(IOUNT)
c     WRITE (22,611) IOUNT,XLOGL(IOUNT)
611   FORMAT (//2X,'In loop ',I3,' log likelihood is ',F15.3)
c     WRITE (22,612)
612   FORMAT (/4X,'Estimate of mixing proportion for each group')
c     WRITE (22,102) (T(K),K=1,NG)
102   FORMAT (2X,10F7.3)
C     Determine partition of entities into NG groups
      DO 30 K=1,NG
30    XCC(K)=0.0
      DO 50 I=1,NIND
      MAX=1
      DO 40 K=2,NG
40    IF (W(I,K).GT.W(I,MAX)) MAX=K
      XCC(MAX)=XCC(MAX)+W(I,MAX)
      IDT(I)=MAX
50    CONTINUE
c     WRITE (22,101)
101   FORMAT (//2X,'Entity: Final estimates of posterior ',
     1'probabilities of group membership'/)
      DO 60 I=1,NIND
60    CONTINUE
103   FORMAT (2X,I6,2X,10F7.3)
c     WRITE (22,107)
107   FORMAT (//2X,'Resulting partition of the entities into NG groups')
c     WRITE (22,108) (IDT(I),I=1,NIND)
c       write (9,108) (idt(i),i=1,nind)
108   FORMAT (2X,10I4)
      DO 310 K=1,NG
310   N(K)=0
      DO 320 I=1,NIND
      K=IDT(I)
      IF (K.EQ.0) GO TO 320
      N(K)=N(K)+1
320   CONTINUE
c     WRITE (22,113)
113   FORMAT (//2X,'Number assigned to each group')
c     WRITE (22,114) (N(K),K=1,NG)
114   FORMAT (2X,10I6)
C     Compute estimates of correct allocation rates
      CC=0.0
      DO 70 K=1,NG
      XCC(K)=XCC(K)/(NIND*T(K))
70    CC=CC+T(K)*XCC(K)
c     WRITE (22,105)
105   FORMAT (//2X,'Estimates of correct allocation rates for ',
     1'each group')
c     WRITE (22,102) (XCC(K),K=1,NG)
c     WRITE (22,104) CC
104   FORMAT (//2X,'Estimate of overall correct allocation rate ',
     1F7.3)
      RETURN
      END


      SUBROUTINE STR(NG,XML,NATT,NCOV)
C     This subroutine calculates the log likelihood criterion value.
      DIMENSION XML(10)
      CRIT=-2.0*(XML(1)-XML(NG))
      IF (NCOV.EQ.1) THEN
      IDF=2*NATT*(NG-1)
      ELSE
      IDF=2*(NATT+NATT*(NATT+1)/2)*(NG-1)
      ENDIF
      WRITE (6,111) CRIT,IDF
111   FORMAT (//2X,'The likelihood ratio test statistic is ',F9.3,
     1' with ',I4,' degrees of freedom')
      P = PROB (CRIT,IDF)
      WRITE (6,112) P
112   FORMAT (4X,'and the p value for this statistic is ',F8.3)
      RETURN
      END


      FUNCTION PROB (CRIT,IDF)
C     For accuracy see Golden, Weiss and Davis (1968)
C     Educ. Psycol. Measurement Vol 28, 163-165.
C     Reference R.G. Davies. Computer Programming in Quantitative
C     Biology.
C
      X = CRIT
      AN1 = IDF
      F=X/AN1
      AN2=1.0E10
      FF=F
      PROB=1.0
      IF (AN1*AN2*F.EQ.0.0) RETURN
C
C     Take reciprocal if F les than 1.0
C
      IF (F.GE.1.0) GO TO 6
      FF=1.0/F
      TEMP=AN1
      AN1=AN2
      AN2=TEMP
C
C     Normalise Variance Ratio
C
6     A1=2.0/AN1/9.0
      A2=2.0/AN2/9.0
      Z=ABS(((1.0-A2)*FF**0.3333333-1.0+A1)/SQRT(A2*FF**0.6666666+A1))
      IF (AN2.LE.3.0) Z=Z*(1.0+0.08*Z**4/AN2**3)
C
C     Compute probability
C
7     FZ = EXP(-Z*Z/2.0)*0.3989423
      W=1.0/(1.0+Z*0.2316419)
      PROB = FZ*W*((((1.332074*W-1.821256)*W+1.781478)*W-0.3565638)*W
     1+0.3193815)
      IF (F.LT.1.0) PROB=1.0-PROB
      RETURN
      END


      SUBROUTINE MATINV(A,NR,NC,NMAX,DET,IP,TOL,IERR)
      DIMENSION A(10,11),IP(10)
C     This is a matrix inversion subroutine originally written by
C     I. Oliver of the University of Queensland Computer Centre in
C     September 1966 in FORTRAN IV.  It was revised by J. Williams in
C     April 1967; then revised and updated by I. Oliver in November
C     1969.  Ref. UQ D4.505
C
C     Note:   NMAX on input must equal the row dimension of A
C             In this program it is set at 10.
C
C     Initialization
      IERR=0
      IF (NR.LE.NMAX.AND.NR.GT.0.AND.NC.GT.0) GO TO 5
      IERR=2
      RETURN
5     DET=1.0
      DO 10 I=1,NR
10    IP(I)=I
      DO 170 K=1,NR
C     Search for pivot
      IR=K
      AMAX=0.0
      DO 20 I=K,NR
      IF (ABS(A(I,K)).LE.ABS(AMAX)) GO TO 20
      AMAX=A(I,K)
      IR=I
20    CONTINUE
      DET=DET*AMAX
C     Record pivot row
      I=IP(K)
      IP(K)=IP(IR)
      IP(IR)=I
C     Test for zero pivot
      IF (ABS(AMAX).GE.TOL*TOL) GO TO 70
      IERR=1
      RETURN
C     Move selected pivot row to pivot row
70    IF (IR.EQ.K) GO TO 100
      DO 90 J=1,NC
      TEMP=A(IR,J)
      A(IR,J)=A(K,J)
90    A(K,J)=TEMP
      DET=-DET
C     Transform matrix
100   DO 110 J=1,NC
110   A(K,J)=A(K,J)/AMAX
      DO 150 I=1,NR
      IF (I.EQ.K) GO TO 150
      DO 140 J=1,NC
      IF (J.EQ.K) GO TO 140
      T1=A(I,J)
      T2=A(I,K)*A(K,J)
      T3=T1-T2
      IF (ABS(T3).LT.(ABS(T1)+ABS(T2))*TOL)T3=0.0
      A(I,J)=T3
140   CONTINUE
150   CONTINUE
      DO 160 I=1,NR
160   A(I,K)=-A(I,K)/AMAX
      A(K,K)=1.0/AMAX
170   CONTINUE
C     Realign inverse
      DO 220 K=1,NR
      DO 180 J=1,NR
      IF (K.EQ.IP(J)) GO TO 190
180   CONTINUE
190   IF (J.EQ.K) GO TO 220
      DO 210 I=1,NR
      TEMP=A(I,J)
      A(I,J)=A(I,K)
210   A(I,K)=TEMP
      I=IP(K)
      IP(K)=IP(J)
      IP(J)=I
220   CONTINUE
      IF (IERR.EQ.1) IERR=3
      IF (IERR.EQ.2) IERR=0
      RETURN
      END

