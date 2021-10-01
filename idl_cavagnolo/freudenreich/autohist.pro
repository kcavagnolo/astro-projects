PRO AUTOHIST,V, zx,zy,xx,yy, NOPLOT=whatever
;
;+
; NAME:
;       AUTOHIST
;
; PURPOSE:
;       Draws a histogram using automatic bin-sizing.
; 
; EXPLANATION:
;       AUTOHIST chooses a number of bins (initially, SQRT(2*N). If this leads 
;       to a histogram in which > 1/5 of the central 50% of the bins are empty,
;       it decreases the number of bins and tries again. The minimum # bins 
;       is 5. The max=199.   Called by HISTOGAUSS and HALFAGAUSS.    Note that 
;       the intrinsic HISTOGRAM function is *not* used.
;
; CALLING SEQUENCE:
;       AUTOHIST, Sample, XLines, Ylines, XCenters, YCenters, [/NOPLOT]
;
; INPUT:
;       Sample = the vector to be histogrammed
;
; OUTPUT:
;       XLINES = the x coordinates of the points that trace the rectangular 
;               histogram bins
;       YLINES = the y coordinates. To draw the histogram plot YLINES vs XLINES.
;       XCENTERS = the x values of the bin centers
;       YCENTERS = the corresponding y values
;
; OPTIONAL INPUT KEYWORD:
;       /NOPLOT  If set, nothing is drawn
;
; SUBROUTINE CALLS:
;       MED, which calculates a median
; REVISION HISTORY:
;       Written,   H. Freudenreich, STX, 1/91
;       1998 March 17 - Changed shading of histogram.  RSH, RSTX
;-

 ON_ERROR,2
 minbin = 5

 N = N_ELEMENTS(v)
 nb = FIX(SQRT(2.*N)) < 199
 nb = nb > minbin

 x1 = MIN(V)  &  x2 = MAX(V)

TRYAGAIN:

 yy = FLTARR(nb)
 dx = (x2-x1)/nb
 xx = FindGEN(nb)*dx + dx/2. + x1

 ind = (V-x1)/dx
 Q = WHERE( ind GT (nb-1),Count )   &  IF Count GT 0 THEN ind(Q) = nb-1
 Q = WHERE( ind LT 0,Count )        &  IF Count GT 0 THEN ind(Q) = 0

 FOR I = LONG(0),N-1 DO yy(ind(I)) = yy(ind(I))+1.

; Count the fraction of empty bins in the middle half of the histogram:
 x14 = (xx(nb-1)-xx(0))/4. + x1
 X34 = xx(nb-1) - (xx(nb-1)-xx(0))/4.
 Q = WHERE( (yy EQ 0.) AND (xx GT x14) AND (xx LT X34), Count )
 IF (Count GT nb/10) AND (nb GT MInbIN) THEN BEGIN  ; 20% EMPTY
   nb = 3*nb/4
   IF nb LT (2*N) THEN GOTO, TRYAGAIN
 ENDIF

; Fill in zx,zy:
 mb=2*nb+2
 zx=FLTARR(mb)  &  zy=FLTARR(mb)
 zx(0)=x1       &  zy(0)=0.
 K=0 
 FOR J=0,nb-1 DO BEGIN
  K=K+1  &  zx(K) = xx(J)-dx/2.  &  zy(K)=yy(J)
  K=K+1  &  zx(K) = xx(J)+dx/2.  &  zy(K)=yy(J)
 ENDFOR
 K=K+1    &  zx(K)=x2             &  zy(K)=0.

 IF KEYWORD_SET(WHATEVER) THEN RETURN

; Plot, then fill, the bins:
 ytop = MAX(yy(1:nb-2))
 IF yy(0) GT ytop THEN yy(0) = ytop
 IF yy(nb-1) GT ytop THEN yy(nb-1) = ytop

 PLOT,xx,yy,XRAN=[x1-dx, x2+dx],YRAN=[0.,1.1*ytop],PSYM=10,LINE=0

 FOR J = 0,nb-1 DO BEGIN
 IF yy(J) GT 0 THEN BEGIN
     A = [xx(j)-dx/2.,xx(j)+dx/2.,xx(j)+dx/2.,xx(j)-dx/2.] 
     B = [0.,0.,yy(j),yy(j)]
     POLYFILL,A,B,orientation=45
 ENDIF
 ENDFOR

 RETURN
 END

