c This is a program to fit spectral data to theoretical synchrotron
c spectra generated under various assumptions.  The theoretical 
c curves are actually very accurate polynomial fits to the calculated
c curves for high frequency steepening in spectra due to synchrotron
c losses.  The fit is done in two steps. 
c
c needs to link to  setspc.f  + PACHOL.INCL,  GSCONSTS.INCL
c                   g77 sfnrl.f setspc.f -o sfnrl2.exe
c step 1: Good initial guess by Bisection. 
c         A Myers and Spangler test (1985, Ap.J., 291, 52)
c         to get a first guess at the turnover
c         frequency. Calculate the two point spectral index from the 
c         high and low points of the data (or two points 
c         with small errors, and well separated in frequency). 
c         Then calculate the turnover frequency which allows for a
c         good fit to this value, given the errors. If only two
c         frequencies are available, this is all you get.
c         A good initial guess should speed up step 2, and
c         should put routine  near global minimum.
c         The MS test cannot differentiate between models.
c         The MS test is independent of intensity scaling.
c
c         This section also does screening for bad input model
c         i.e. a model which cannot physically match data.
c
c
c step 2: Marquardt Non-linear Chi Square Fitting Routine.
c         from `Numerical Recipes', Press, Flannery, Teukolsky, 
c         and Vetterling, Cambridge Univ. Press 1987, page 523.
c         Use all data to fit for intensity scaling and break freq.
c
c C. Carilli, 3/24/90
c  final edit 4/15/90
c  revised 2/99 -- for log input file
c  fixed for linux 2/06
c
c Notes: i. The theoretical curves in terms of freq/(turnover freq)
c       ii. We assume a low frequency spectral index, and then derive
c           the corresponding spectral shape for fits.
c      iii. fitting is done in log plane
c         
c Potential Problems:
c        a. KP, CI models: MS solution is dicey for break freq. < lowest
c           freq. in data.  basically get first guess = lowest freq. in data.
c           NR routine should recover proper solution
c        b. JP model bombs if highest freq. in data > 80x(break freq)
c           (sampling spectrum at e**-80). probably won't be
c           important in practice since source will be gone.
c
       real*4 nud(100), id(100),   nut,  nutt
       real*4 eidp(100), eidi(100), lenut, enut
       real*4 lnut,lincr, incr
       real*4 int, lnu, fo, lfo
       real*4 lk, lfth, fth
       real*8 x
       real*4 gamma
c
       DIMENSION COVAR(100,100),  LISTA(100), A(100), ALPHA(100, 100)
       dimension xlnud(100),xlid(100),xleid(100)
c
c              First get low frequency spectral index, alph,
c              and calculate the theoretical spectral fit 
c              polynomial coefficients.
       print *,'Input low freq. spectral index (I = nu**SI):'
       read(5,*) alph
       print *,'Input option (integer:2=JP,3=KP,5=CI):'
       read(5,*) iopt
       print *,'Input convergence test (0 = 0.005):'
       read(5,*) ct
       if(ct.eq.0.0) ct = 0.005
       print *,'Input nb for intensity scaling first guess(0=1):'
       read(5,*) nb
       if(nb.eq.0) nb = 1
c              to be consistent with PL, redefine malpha = -alpha
       xalpha = -1.0*(alph)
       gamma = 2.0*(xalpha) + 1.0
       call setspc(gamma,iopt)
c              
c              This section added for testing
c              generate real spectrum for testing
c
c       print *,'input low,mid1,mid2,mid3,high, break freq(MHz):'
c        read(5,*) nud(1),nud(2),nud(3),nud(4),nud(5),nutt
c        nud(1) = nud(1)*1000000.0
c        nud(2) = nud(2)*1000000.0
c        nud(3) = nud(3)*1000000.0
c        nud(4) = nud(4)*1000000.0
c        nud(5) = nud(5)*1000000.0
c        nutt = nutt*1000000.0
c       nfreq = 5
c       x = nud(1)/nutt
c        id(1) = spval(x)
c        id(1) = id(1) + 0.06*id(1)
c        eidp(1) = 0.05
c        eidi(1) = 0.0
c       x = nud(2)/nutt
c        id(2) = spval(x)
c        id(2) = id(2) - 0.09*id(2)
c        eidp(2) = 0.05
c        eidi(2) = 0.0
c       x = nud(3)/nutt
c        id(3) = spval(x)
c        id(3) = id(3) + 0.03*id(3)
c        eidp(3) = 0.05
c        eidi(3) = 0.0
c       x = nud(4)/nutt
c        id(4) = spval(x)
c        id(4) = id(4) + 0.04*id(4)
c        eidp(4) = 0.05
c        eidi(4) = 0.0
c       x = nud(5)/nutt
c        id(5) = spval(x)
c        id(5) = id(5) - 0.03*id(5)
c        eidp(5) = 0.05
c        eidi(5) = 0.0
c
c              Then get data: freq, flux and errors (percentage
c              and 'noise', ie. multiplicative and additive)
c              file should be in order from low to high freq.
c              THIS USES LOG INPUT FILE
c
       open(3, file='spectra.in')
       do 10 i= 1, 100
        read(3,*,end=20) xlnud(i), xlid(i), xleid(i)
 10    continue
 20    nfreq = i - 1
c
c              ask for two Myers and Spangler fit frequencies, or
c              else use hi and low.
c              
       print *,' input integers for low, hi MS freqs (0, 0=> min, max):'
      read(5,*) nl, nh
      if (nl.eq.0) then
       nl = 1
       nh = nfreq
      endif
c
c         debug 
c         print *,'input first guess nub (MHZ):'
c         read(5,*) nut
c         nut = nut*1000000.0
c          
c          MS test for first guess (if only two freqs, thats
c          all you get) and also test for unphysical model/data match
c
       ierr = 0
C         
      call MS(nfreq,xlnud,xlid,xleid,alph,iopt,nh,nl,xlnut,ierr)
c
        if(ierr.eq.1) then
         print *,' alpha too steep, raise value or fit power law'
         go to 811
        endif
        if(ierr.eq.2) then
         print *,' alpha too flat, lower value or fit power law'
         go to 811
        endif
c
        nut = 10**xlnut
c
        print *,' after MS, nut (MHz)= ', nut/1000000.0
c
        if(nfreq.eq.2) go to 701
c
c                   
c                   first guesses: nut = from MS
c                   int scale = scale for nu(nub)
c                   IMPORTANT elements of A are: lnut and int. scale
c
        a(1) = log10(nut)
c
c 
c              STAGE 2
c              use rest of data and do true least squares fit
c              in two variables: lnut and lk (break freq. and int. scale). 
c              Note: this can be generalized for other functions
c              and more valiables.
c
       nit = 0
       chis = 0.0
c                   calculate intesity scaling guess, a(2)
c                   
        x = 10.0**(xlnud(nb)-a(1))
        fth = spval(x) 
        lfth = log10(fth)       
        a(2) = xlid(nb) - lfth
C         
CC   DEBUG
c        PRINT *,' IS1=',A(2)
c
c              first initialize arrays
C                                          
      DO 7 JJ= 1,100
       DO 8 KK = 1, 100
        COVAR(JJ,KK) = 0.0
        alpha(jj,kk) = 0.0
  8    CONTINUE
  7   CONTINUE
c
c
C
c             number of variables = 2, change for generalization
       MA = 2
C
C SET UP DUMMY MATRIX FOR SORTING
       DO 22 J=1, MA
        LISTA(J) = J
 22    CONTINUE
c                      fit for all variables
       MFIT = MA
       NCVM = MA
c                      initialize alamda
       alamda = -1.0
C
C FIT
c                      iterate to find lowest chisquare    
c                      convergence set at ct = fractional  change in
c                      chis (I recommend .005)
c                      note: alpha in mrqmin is not related to spectral index
c
700    nit = nit + 1
       chiso = chis	
       CALL mrqmin(xlnud,xlid,xleid,nfreq,a,ma,LISTA,MFIT,COVAR,alpha,
     m              NCVM,CHIS,alamda)
C
c              first iteration => loop
       if (nit.eq.1) go to 700
c              compare to chi square old for convergence
       delchi = (chis - chiso)/chis
       if(delchi.gt.0.0) go to 700
       if(abs(delchi).gt.ct) go to 700
c              all done - final call to set alpha and covar = curvature
c              and covariance matricies, for rough error analysis
       alamda = 0.0
       CALL mrqmin(xlnud,xlid,xleid,nfreq,a,ma,LISTA,MFIT,COVAR,alpha,
     m              NCVM,CHIS,alamda)
c
c               formal error in nut: enut = root(COVAR(1,1)) = 1 sigma
c               or delta(chisquare) = 1 or
c               68%  confidence level that nut from fit
c               lies within nut +/- enut 
c               1. Note: fitting in log plane => enut = percentage error.
c               2. note that  the error as defined 
c                  may not be strictly valid for multiparameter
c                  fit. Further, this simple analysis
c                  holds strictly for gaussian random 
c                  (normal) errors on data, which is probably not the
c                  case for most astronomical data, in which systematics
c                  dominate. Hence, true error in nut is probably much
c                  larger than formal error (e.g. still have not considered
c                  injection index = third parameter!).
c
       nut = 10**(a(1))
c
       lenut = (abs(covar(1,1)))**0.5
       enut = 10**lenut - 1.0
c
701     print *,' '
        print *,'END END END'
c                            normalize chisquare and break freq. (MHz)
       chis = chis/nfreq
       nut = nut/1000000.0
c       
       print *,' nfreq, normalized chisquare=',nfreq,chis
       print *,' Break freq. (MHZ)',nut
       print *,' fractional error, log(int. scale)',enut,a(2)
c             write spectrum to file (in log)
c             first get range in freq.
       open(1,file='theory.out')
       open(2,file='data.out')
       print *,' input range for spectrum, xlo, xhi (MHz)'
       read(5,*) xlo, xhi
        xhi = log10(xhi*1000000.0)
        xlo = log10(xlo*1000000.0)
        int = (xhi - xlo)/200.0
c       int = (xlnud(nfreq) - xlnud(1))/200.0
       do 800 i= 0, 200
        lnu = xlo + i*int
        x = 10.0**(lnu-a(1))
        fo = spval(x) 
        lfo = log10(fo) + a(2)
        write(1,*) lnu, lfo
800    continue
c       do 800 i= 0, 200
c        lnu = xlnud(1) + i*int
c        x = 10.0**(lnu-a(1))
c        fo = spval(x) 
c        lfo = log10(fo) + a(2)
c        write(1,*) lnu, lfo
c800    continue
c             write log data to file
       do 810 i=1, nfreq
        write(2,*) xlnud(i),xlid(i),xleid(i)
810    continue
811    continue
       close(1)
       close(2)
       close(3)
       stop
       end
c
C    routine to do non-linear least square fitting.
C    using normal matrix manipulation and interation.
C     X, Y are data arrays, sig is error array
C     Ndata = number of data points
C     A = output array of polynomial coefficients
C     MA = number of coefficients
C     lista = dummy matrix for sorting
C     mfit = ma     (fit for all variables)
C     covar = covariance matrix
c     alpha = work space for interations
C     ncvm = ma  (dimension of covar and alpha)
C     chisq = output chisq
c     funcs = function returning values given parameters a()
c     alamda = marquardt fudge factor
C         
	SUBROUTINE MRQMIN(X,Y,SIG,NDATA,A,MA,LISTA,MFIT,
     *		COVAR,ALPHA,NCA,CHISQ,ALAMDA)
	PARAMETER (MMAX=20)
	DIMENSION X(NDATA),Y(NDATA),SIG(NDATA),A(MA),LISTA(MA),
     *	COVAR(NCA,NCA),ALPHA(NCA,NCA),ATRY(MMAX),BETA(MMAX),DA(MMAX)
	IF(ALAMDA.LT.0.)THEN
		KK=MFIT+1
		DO J=1,MA
			IHIT=0
			DO K=1,MFIT
				IF(LISTA(K).EQ.J)IHIT=IHIT+1
			ENDDO
			IF (IHIT.EQ.0) THEN
				LISTA(KK)=J
				KK=KK+1
			ELSE IF (IHIT.GT.1) THEN
				PAUSE 'Improper permutation in LISTA'
			ENDIF
		ENDDO
		IF (KK.NE.(MA+1)) PAUSE 'Improper permutation in LISTA'
		ALAMDA=0.001
		CALL MRQCOF(X,Y,SIG,NDATA,A,MA,LISTA,MFIT,ALPHA,BETA,NCA,
     c                      CHISQ)
		OCHISQ=CHISQ
		DO J=1,MA
			ATRY(J)=A(J)
		ENDDO
	ENDIF
	DO J=1,MFIT
		DO K=1,MFIT
			COVAR(J,K)=ALPHA(J,K)
		ENDDO
		COVAR(J,J)=ALPHA(J,J)*(1.+ALAMDA)
		DA(J)=BETA(J)
	ENDDO
	CALL GAUSSJ(COVAR,MFIT,NCA,DA,1,1)
	IF(ALAMDA.EQ.0.)THEN
		CALL COVSRT(COVAR,NCA,MA,LISTA,MFIT)
		RETURN
	ENDIF
	DO J=1,MFIT
		ATRY(LISTA(J))=A(LISTA(J))+DA(J)
	ENDDO
	CALL MRQCOF(X,Y,SIG,NDATA,ATRY,MA,LISTA,MFIT,COVAR,
     m              DA,NCA,CHISQ)
	IF(CHISQ.LT.OCHISQ)THEN
		ALAMDA=0.1*ALAMDA
		OCHISQ=CHISQ
		DO J=1,MFIT
			DO K=1,MFIT
				ALPHA(J,K)=COVAR(J,K)
			ENDDO
			BETA(J)=DA(J)
			A(LISTA(J))=ATRY(LISTA(J))
		ENDDO
	ELSE
		ALAMDA=10.*ALAMDA
		CHISQ=OCHISQ
	ENDIF
	RETURN
	END
c 
c               evaluate fitting matrix
c         
	SUBROUTINE MRQCOF(X,Y,SIG,NDATA,A,MA,LISTA,MFIT,ALPHA,
     m                    BETA,NALP,CHISQ)
	PARAMETER (MMAX=20)
	DIMENSION X(NDATA),Y(NDATA),SIG(NDATA),ALPHA(NALP,NALP),BETA(MA),
     *		DYDA(MMAX),LISTA(MFIT),A(MA)
	DO J=1,MFIT
		DO K=1,J
			ALPHA(J,K)=0.
		ENDDO
		BETA(J)=0.
	ENDDO
	CHISQ=0.
	DO I=1,NDATA
c               
		CALL FUNCS(X(I),A,YMOD,DYDA,MA)
c
		SIG2I=1./(SIG(I)*SIG(I))
		DY=Y(I)-YMOD
		DO J=1,MFIT
			WT=DYDA(LISTA(J))*SIG2I
			DO K=1,J
				ALPHA(J,K)=ALPHA(J,K)+WT*DYDA(LISTA(K))
			ENDDO
			BETA(J)=BETA(J)+DY*WT
		ENDDO
		CHISQ=CHISQ+DY*DY*SIG2I
	ENDDO
	DO J=2,MFIT
		DO K=1,J-1
			ALPHA(K,J)=ALPHA(J,K)
		ENDDO
	ENDDO
	RETURN
	END
c
c             re-sort covariance matrix
c
	SUBROUTINE COVSRT(COVAR,NCVM,MA,LISTA,MFIT)
	DIMENSION COVAR(NCVM,NCVM),LISTA(MFIT)
	DO J=1,MA-1
		DO I=J+1,MA
			COVAR(I,J)=0.
		ENDDO
	ENDDO
	DO I=1,MFIT-1
		DO J=I+1,MFIT
			IF(LISTA(J).GT.LISTA(I)) THEN
				COVAR(LISTA(J),LISTA(I))=COVAR(I,J)
			ELSE
				COVAR(LISTA(I),LISTA(J))=COVAR(I,J)
			ENDIF
		ENDDO
	ENDDO
	SWAP=COVAR(1,1)
	DO J=1,MA
		COVAR(1,J)=COVAR(J,J)
		COVAR(J,J)=0.
	ENDDO
	COVAR(LISTA(1),LISTA(1))=SWAP
	DO J=2,MFIT
		COVAR(LISTA(J),LISTA(J))=COVAR(1,J)
	ENDDO
	DO J=2,MA
		DO I=1,J-1
			COVAR(I,J)=COVAR(J,I)
		ENDDO
	ENDDO
	RETURN
	END
C    matrix manipulation using Gauss-Jordan elimination
C
	SUBROUTINE GAUSSJ(A,N,NP,B,M,MP)
	PARAMETER (NMAX=50)
	DIMENSION A(NP,NP),B(NP,MP),IPIV(NMAX),INDXR(NMAX),INDXC(NMAX)
	DO J=1,N
		IPIV(J)=0
	ENDDO
	DO I=1,N
		BIG=0.
		DO J=1,N
		  IF(IPIV(J).NE.1)THEN
		    DO K=1,N
		     IF (IPIV(K).EQ.0) THEN
			IF (ABS(A(J,K)).GE.BIG)THEN
				BIG=ABS(A(J,K))
				 IROW=J
				 ICOL=K
			ENDIF
		        ELSE IF (IPIV(K).GT.1) THEN
		        PAUSE 'Singular matrix1'
		      ENDIF
		     ENDDO
	           ENDIF
		ENDDO
		IPIV(ICOL)=IPIV(ICOL)+1
		IF (IROW.NE.ICOL) THEN
			DO L=1,N
				DUM=A(IROW,L)
				A(IROW,L)=A(ICOL,L)
				A(ICOL,L)=DUM
			ENDDO
			DO L=1,M
				DUM=B(IROW,L)
				B(IROW,L)=B(ICOL,L)
				B(ICOL,L)=DUM
			ENDDO
		ENDIF
		INDXR(I)=IROW
		INDXC(I)=ICOL
		IF (A(ICOL,ICOL).EQ.0.) PAUSE 'Singular matrix 2.'
		PIVINV=1./A(ICOL,ICOL)
		A(ICOL,ICOL)=1.
		DO L=1,N
			A(ICOL,L)=A(ICOL,L)*PIVINV
		ENDDO
		DO L=1,M
			B(ICOL,L)=B(ICOL,L)*PIVINV
		ENDDO
		DO LL=1,N
			IF(LL.NE.ICOL)THEN
				DUM=A(LL,ICOL)
				A(LL,ICOL)=0.
				DO L=1,N
				 A(LL,L)=A(LL,L)-A(ICOL,L)*DUM
				ENDDO
				DO L=1,M
				 B(LL,L)=B(LL,L)-B(ICOL,L)*DUM
				ENDDO
			ENDIF
		ENDDO
	ENDDO
	DO L=N,1,-1
		IF(INDXR(L).NE.INDXC(L))THEN
			DO K=1,N
				DUM=A(K,INDXR(L))
				A(K,INDXR(L))=A(K,INDXC(L))
				A(K,INDXC(L))=DUM
			ENDDO
		ENDIF
	ENDDO
	RETURN
	END
c
c           subroutine to calculate function and numerical derivative (in log)
c
       SUBROUTINE FUNCS(x,a,yfl,dly,ma)
	DIMENSION A(ma),  dly(ma)
        double precision x1, ad11, ad12, ad21, ad22, dl1,dl2,dl3,dl4
        double precision yl11, yl12, yl21, yl22
c                   
        x1 = 10.0**(x-a(1))
        fth = spval(x1) 
        yfl = log10(fth) + a(2)
c           
c                    derivatives wrt a(1) and a(2)
c                    increment in log plane is 0.00043
c                    which equals fractional change of 0.001
c                    in normal space log(1.001) = 0.00043
c                    derivative is mean above/below x
c
        ad11 = a(1) + 0.00043
        x1 = 10.0**(x-ad11)
        fth = spval(x1) 
        yl11 = log10(fth) + a(2)  
        dl1 = (yl11 - yfl)/(ad11 - a(1))
c
        ad12 = a(1) - 0.00043
        x1 = 10.0**(x-ad12)
        fth = spval(x1) 
        yl12 = log10(fth) + a(2)  
        dl2 = (yl12 - yfl)/(ad12 - a(1))
c
        dly(1) = (dl1 + dl2)/2.0
c
c
        ad21 = a(2) + 0.0043
        x1 = 10.0**(x-a(1))
        fth = spval(x1) 
        yl21 = log10(fth) + ad21
        dl3 = (yl21 - yfl)/(ad21 - a(2))
c
c
        ad22 = a(2) - 0.0043
        x1 = 10.0**(x-a(1))
        fth = spval(x1) 
        yl22 = log10(fth) + ad22
        dl4 = (yl22 - yfl)/(ad22 - a(2))
c
        dly(2) = (dl4 + dl3)/2.0
c
C        print *,'yfl, dly=', yfl, dly
       return
      end
C
      SUBROUTINE MS(nfreq,xlnud,xlid,xleid,alph,iopt,nh,nl,xlnut,ierr)
c 
c         A Myers and Spangler test to get a first guess at the turnover
c         frequency. Calculate the two point spectral index from the 
c         high and low points of the data (or two points 
c         with small errors, and well separated in frequency
C         given by user). 
c         Then calculate the turnover frequency which allows for a
c         good fit to this value, given the errors. If only two
c         frequencies are available, this is all you get.
c         This section speeds up slope calculation, and hopefully
c         puts one near the global min.
c         my experience has shown local minima exist.
c                                                       3/23/90
c
c              calculate the max. and min. spectral index from data
c              amaxd= flattest, amind = steepest, using errors
c
       dimension xlnud(100),xlid(100),xleid(100)
       double precision x
c
c               
       ierr = 0
 30    xeh = xleid(nh)
       xel = xleid(nl)
       xldl = xlnud(nh) - xlnud(nl)
       amaxd = ((xlid(nh)+xeh)-(xlid(nl)-xel))/xldl
       amind = ((xlid(nh)-xeh)-(xlid(nl)+xel))/xldl
       print *,' max spec index from data:', amaxd
c
c              rejection tests
c
c              check if amind > alpha(in), 
c              if so, then data spectrum is flatter than injection
c              spectrum, and since steepening can only occur, you should 
c              use a flatter injection index.  reset high to highest
c              in data, and try again. if already highest, then error.
c              program would try to push nut to infinity
c
       if(amind.gt.alph) then
        if(nh.eq.nfreq) then
         ierr = 1
         go to 500
        endif
        nh = nfreq 
        go to 30
       endif
c
       if(iopt.eq.2) go to 35
       if(iopt.eq.1) go to 35
c             
c              check if amaxd < alpha(in), 
c              if so, then data spectrum is steeper than maximum allowed
c              by model, even at high frequency. you should 
c              use a steeper injection index.
c              program would try to push nut to zero
c              extra amount of 0.1 subtracted to allow for
c              some slop in data/model such that we do not
c              reject potentially useful data/models
c              0.1 is arbitrary but reasonable
c 
       if(iopt.eq.3) alphi = (4.0/3.0)*alph - 1.0 - 0.1
       if(iopt.eq.4) alphi = alph - 0.5 - 0.1
       if(iopt.eq.5) alphi = alph - 0.5 - 0.1
       if(iopt.eq.6) alphi = alph - 0.5 - 0.1
       if(amaxd.lt.alphi) then
        if(nl.eq.1) then
         ierr = 2
         go to 500
        endif
        nl = 1 
        go to 30
       endif
c              first guess at turnover freq. = mean (in log) between 
c              high and low in fit. Then calculate ratios 
c              implied for data points.
 35     niter = 0
        itest = 0
        xlincr = 0.5*(xlnud(nh) - xlnud(nl))
        xlnut = 0.5*(xlnud(nl) + xlnud(nh))
 40    niter = niter + 1
        xlrnudh = xlnud(nh) - xlnut
        xlrnudl = xlnud(nl) - xlnut
c              calculate implied two freq spectral index from theory
       x = (10.0**(xlnud(nl)))/(10.0**(xlnut))
        fthl = spval(x)
        xlfthl = log10(fthl)
       x = (10.0**(xlnud(nh)))/(10.0**(xlnut))
        fthh = spval(x) 
        xlfthh = log10(fthh)
       ath = (xlfthh - xlfthl)/(xlrnudh - xlrnudl)
c              check with data, if between max and min, then we
c              have our first guess, if not, adjust and iterate.
c              initial iterations step in constant 
c              increments = difference
c              between max. and min (log) in fit, until we find a range
c              within which the correct value lies. then we change the
c              search to one which divides by two the defined region.
c
       if(ath.gt.amaxd) then 
c               theory is too flat, adjust nut down by log incr
c               but first test if have max-min defined, and start
c               second search
        if(itest.eq.1) go to 200
        itest = -1
        xlnutold = xlnut
        xlnut = xlnut - xlincr
        go to 40
       endif
c
       if(ath.lt.amind) then
c              theory is too steep, adjust nut etc...
        if(itest.eq.-1) go to 200
        itest = 1
        xlnutold = xlnut
        xlnut = xlnut + xlincr
        go to 40
       endif
c              if fails both these test, we have our initial guess
       go to 500
c              start second fitting by bisection search
200    continue 
       if(itest.eq.1) then
        xlnuth = xlnutold
        xlnutl = xlnut
       else
        xlnuth = xlnut
        xlnutl = xlnutold
       endif
 50    niter = niter + 1
       xlnut = 0.5*(xlnuth + xlnutl)
c       print *,' Bisection: niter, lnut =',niter, lnut
       xlrnudh = xlnud(nh) - xlnut
       xlrnudl = xlnud(nl) - xlnut
c              calculate implied two freq spectral index from theory
       x = (10.0**(xlnud(nl)))/(10.0**(xlnut))
        fthl = spval(x)
        xlfthl = log10(fthl)
       x = (10.0**(xlnud(nh)))/(10.0**(xlnut))
        fthh = spval(x) 
        xlfthh = log10(fthh)
       ath = (xlfthh - xlfthl)/(xlrnudh - xlrnudl)
       if(ath.gt.amaxd) then 
c               theory is too flat, adjust nut  
        xlnutl = xlnut
        go to 50
       endif
c
       if(ath.lt.amind) then
c              theory is too steep, adjust nut 
        xlnuth = xlnut
        go to 50
       endif
c              if both these tests fail, we have our initial guess
500    continue
       return
       end

