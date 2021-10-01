FUNCTION INTEG,X,Y,IMIN,IMAX
;+
; NAME:
;       INTEG
; PURPOSE:
;       Trapezoidal integration of the area under a curve
; CALLING SEQUENCE:
;       Result=INTEG(y)
;       Result=INTEG(x,y)
;       Result=INTEG(x,y,imin,imax)
; INPUTS:
;       x = array containing independent variable.  If omitted, then
;           x is assumed to contain the index of the y variable.
;           x=indgen(n_elements(y)).
;       y = array containing dependent variable y=f(x)
;       imin = index of x array at which to begin the integration.  If
;              omitted, then integration starts at x(0).
;       imax = index of x value at which to end the integration.  If 
;              omitted then the integration ends at x(npts).
; OUTPUTS:
;       result = area under the curve y=f(x) between x(imin) and x(imax).
; PROCEDURE:
;       The area is determined of indivdual trapezoids defined by x(i),
;       x(i+1), y(i) and y(i+1).
; MODIFICATION HISTORY:
;       Written, W.B. Landsman, STI Corp. May 1986
;       Modified so X is not altered in a one parameter call Jan 1990
;-
; Set default parameters
NPAR = N_PARAMS(0)
IF NPAR EQ 1 THEN BEGIN
    NPTS = N_ELEMENTS(X)
    YY = X
    XX = INDGEN(NPTS)
ENDIF ELSE BEGIN
   IF NPAR LT 3 THEN IMIN = 0
   NPTS = MIN( [N_ELEMENTS(X),N_ELEMENTS(Y)] )
   IF NPAR LT 4 THEN IMAX = NPTS-1
   XX = X(IMIN:IMAX)
   YY = Y(IMIN:IMAX)
   NPTS = IMAX - IMIN + 1
ENDELSE
;
; Compute areas of trapezoids and sum result
XDIF = SHIFT(XX,-1) - XX
XDIF = XDIF(0:NPTS-2)
YAVG = (YY+SHIFT(YY,-1))/2.
YAVG = YAVG(0:NPTS-2)
RETURN,TOTAL(XDIF*YAVG)
END
