PRO HISTOGAUSS,SAMPLE,A,XX,YY,GX,GY,NOPLOT=WHATEVER,NOFIT=SIMPL,CHARSIZE=CSIZE
;
;+
;NAME:
;	HISTOGAUSS
;
; PURPOSE:
;	Histograms data and overlays it with a Gaussian. Draws the mean, sigma,
;	and number of points on the plot.
;
; CALLING SEQUENCE:
;	HISTOGAUSS, Sample, A, XX, YY, GX, GY, [/NOPLOT, /NOFIT, CHARSIZE = ]
;
; INPUT:
;	SAMPLE = Vector to be histogrammed
;
; OUTPUT ARGUMENTS:
;	A = coefficients of the Gaussian fit: Height, mean, sigma
;		A(0)= the height of the Gaussian
;		A(1)= the mean
;		A(2)= the standard deviation
;		A(3)= the half-width of the 95% conf. interval 
;		A(4)= 1/(N-1)*total( (y-mean)/sigma)^2 ) = a measure of 
;			normality
;
;	Below: superceded. The formula is not entirely reliable.
;	A(4)= measure of the normality of the distribution. =1.0, perfectly
;       normal. If no more than a few hundred points are input, there are
;       formulae for the 90 and 95% confidence intervals of this quantity:
;       M=ALOG10(N-1) ; N = number of points
;       T90=ABS(.6376-1.1535*M+.1266*M^2)  ; = 90% confidence interval
;       IF N LT 50 THEN T95=ABS(-1.9065-2.5465*M+.5652*M^2) $
;                  ELSE T95=ABS( 0.7824-1.1021*M+.1021*M^2)   ;95% conf.
;       (From Martinez, J. and Iglewicz, I., 1981, Biometrika, 68, 331-333.)
;
;	XX = the x coordinates of the histogram bins (CENTER)
;	YY = the y coordinates of the histogram bins
;
; OPTIONAL INPUT KEYWORDS:
;	NOPLOT - If set, nothing is drawn
;	FITIT   If set, a Gaussian is actually fitted to the distribution.
;		By default, a Gaussian with the same mean and sigma is drawn; 
;		the height is the only free parameter.
;	CHARSIZE Size of the characters in the annotation. Default = 0.82.
;
; SUBROUTINE CALLS:
;	BIWEIGHT_MEAN, which determines the mean and std. dev.
;	AUTOHIST, which draws the histogram
;	FITAGAUSS, which does just that
;	MED, which calculates a median (called by AUTOHIST)
;
; REVISION HISTORY:
;	Written, H. Freudenreich, STX, 12/89
;	Modified for IDL Version 2, 1/91, HF
;	More quantities returned in A, 2/94, HF
;	Added NOPLOT keyword and print if Gaussian, 3/94
;	Stopped printing confidence limits on normality 3/31/94 HF
;	Added CHARSIZE keyword, changed annotation format, 8/94 HF
;	Simplified calculation of Gaussian height, 5/95 HF
;-

on_error,2

DATA=SAMPLE
N=N_ELEMENTS(DATA)

; First make sure that not everything is in the same bin. If most
; data = 0, reject zeroes. If they = some other value, complain and
; give up.
A=0.
DATA=DATA(SORT(DATA))  
N3=.75*N & N1=.25*N
IF DATA(N3) EQ DATA(N1) THEN BEGIN
   IF DATA(N/2) EQ 0. THEN BEGIN
      Q=WHERE(DATA NE 0.,NON0)
      IF (N-NON0) GT 15 THEN BEGIN
         PRINT,'AUTOHIST: Suppressing Zeroes!'
         DATA=DATA(Q)
         N=NON0
      ENDIF ELSE BEGIN 
         PRINT,'AUTOHIST: Too Few Non-0 Values!'
         RETURN
      ENDELSE
      Q=0
   ENDIF ELSE BEGIN
      PRINT,'AUTOHIST:  Too Many Identical Values: ',DATA(N/2)
      RETURN
   ENDELSE
ENDIF

A=FLTARR(5) 

; The "mean":
A(1)=BIWEIGHT_MEAN(DATA,S)
; The "standard deviation":
A(2)=S  
; The 95% confidence interval:
M=.7*(N-1)  ;appropriate for a biweighted mean
A(3)=ABS( STUDENT_T(.95,M) )*S/sqrt(n)

; A measure of the Gausianness:
A(4)=TOTAL((DATA-A(1))^2)/((N-1)*A(2)^2)
;Q=WHERE( ABS(DATA-A(1)) LT (5.*S), COUNT )   ; "robust I" unreliable
;ROB_I=TOTAL((DATA(Q)-A(1))^2)/((COUNT-1)*A(2)^2)
;PRINT,A(4),ROB_I

; Set bounds on the data:
U1=A(1)-5.*A(2)
U2=A(1)+5.*A(2)
Q=WHERE(DATA LT U1)
IF Q(0) GE 0 THEN DATA(Q) = U1
Q=WHERE(DATA GT U2)
IF Q(0) GE 0 THEN DATA(Q) = U2

; Draw the histogram
IF KEYWORD_SET(WHATEVER) THEN AUTOHIST,DATA,X,Y,XX,YY,/NOPLOT $
                         ELSE AUTOHIST,DATA,X,Y,XX,YY
; Check for error in AUTOHIST:
M =N_ELEMENTS(X)
MM=N_ELEMENTS(XX)
IF M LT 2 THEN BEGIN
   XX=0. & YY=0. & A=0.
   RETURN ; (AUTOHIST has already screamed)
ENDIF

; Calculate the height of the Gaussian:
Z = EXP(-.5*(X-A(1))^2/A(2)^2 )
XQ1=A(1)-1.3*A(2)
XQ2=A(1)+1.3*A(2)
QQ=WHERE((X GT XQ1) AND (X LT XQ2),COUNT)
IF COUNT GT 0 THEN HYTE=MED(Y(QQ)/Z(QQ)) ELSE BEGIN
   print,'HISTOGAUSS: Distribution too Weird!'
   HYTE=MAX(SMOOTH(Y,5))
ENDELSE
A(0)=HYTE

; Fit a Gaussian, unless the /NOFIT qualifier is present
IF NOT KEYWORD_SET(SIMPL) THEN BEGIN
   PARM=A(0:2)
   YFIT=FITAGAUSS(XX,YY,PARM)
   A(0:2)=PARM
ENDIF

; It the /NOPLOT qualifier is present, we're done.
IF KEYWORD_SET(WHATEVER) THEN RETURN

; Overplot the Gaussian, 
GX=FLTARR(200)
DU=(U2-U1)/199.
GX(0)=U1  &  FOR I=1,199 DO GX(I)=GX(I-1)+DU
Z=(GX-A(1))/A(2)
GY=A(0)*EXP(-Z^2/2. )
OPLOT,GX,GY

; Annotate. 
MEANST=STRING(A(1),'(F9.4)')
SIGST =STRING(A(2),'(F9.4)')
NUM=N_ELEMENTS(DATA)
NUMST =STRING(N,'(I6)')

IF KEYWORD_SET(CSIZE) THEN ANNOT=CSIZE ELSE ANNOT=.82
LABL='#, !7l!6, !7r!3='+numst+','+meanst+','+sigst 
X1=!x.crange(0)+(!x.crange(1)-!x.crange(0))/20. 
y1=!y.crange(1)-(!y.crange(1)-!y.crange(0))/23. 
XYOUTS,X1,Y1,LABL,CHARSIZE=ANNOT

;---commented out test on normality. not entirely reliable.
;; How about the normality of the distribution?
;IF (N LE 500) AND (A(4) LT 3.) THEN BEGIN
;   M=ALOG10(N-1) ; N = number of points
;   IF N LT 50 THEN T95=ABS(-1.9065-2.5465*M+.5652*M^2) $
;              ELSE T95=ABS( 0.7824-1.1021*M+.1021*M^2)   ;=95% conf.
;;   IF A(4) LT T95 THEN BEGIN
;;      Y4=.92*Y3
;;      XYOUTS,X1,Y4,'>95% Gauss',CHARSIZE=ANNOT
;;   ENDIF ELSE BEGIN
;;      T90=ABS(.6376-1.1535*M+.1266*M^2)  ; = 90% confidence interval
;;      IF A(4) LT T90 THEN XYOUTS,X1,.92*Y3,'>90% Gauss',CHARSIZE=ANNOT
;;   ENDELSE
;ENDIF

RETURN
END

