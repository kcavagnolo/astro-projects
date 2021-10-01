FUNCTION MEDSMOOTH,ARRAY,WINDOW
;+
; NAME:
;	MEDSMOOTH
;
; PURPOSE:
;	Median smoothing of a vector, including pointe near its ends.
;
; CALLING SEQUENCE:
;	SMOOTHED = MEDSMOOTH( VECTOR, WINDOW_WIDTH )
;
; INPUTS:
;	VECTOR  = The vector to be smoothed
;	WINDOW = The full width of the window over which the median is 
;		determined for each point.
;
; OUTPUT:
;	Function returns the smoothed vector
;
; SUBROUTINES CALLED: 
;	MEDIAN, to find the median
;
; PROCEDURE:
;	Each point is replaced by the median of the nearest WINDOW of points.
;	The width of the window shrinks towards the ends of the vector, so that
;	only the first and last points are not filtered. These points are 
;	replaced by forecasting from smoothed interior points.
;
; REVISION HISTORY:
; 	Written, H. Freudenreich, STX, 12/89
;	H.Freudenreich, 8/90: took care of end-points by shrinking window.
;-

LEND = N_ELEMENTS(ARRAY)-1
IF (LEND+1) LT WINDOW THEN BEGIN
   PRINT,'MEDSMOOTH:  String too short'
   RETURN,ARRAY
ENDIF

SMOOTHED = FLTARR(LEND+1)
OFFSET = FIX(WINDOW/2)
LOCAL  = FLTARR(WINDOW)

FOR I = long(OFFSET), long(LEND-OFFSET) DO $
  SMOOTHED(I) = MEDIAN( ARRAY(I-OFFSET:I+OFFSET) )

; Fix the ends:
NUMLOOP = (WINDOW-1)/2 - 1
IF NUMLOOP GT 0 THEN BEGIN
   FOR J=1,NUMLOOP DO BEGIN 

     LEN = 2*J+1
     OFFSET = LEN/2

     LOCAL = ARRAY(0:LEN-1)
     LOCAL = LOCAL(SORT(LOCAL))
     SMOOTHED(J) = LOCAL(OFFSET) 

     LOCAL = ARRAY(LEND-LEN+1:LEND)
     LOCAL = LOCAL(SORT(LOCAL))
     SMOOTHED(LEND-J) = LOCAL(OFFSET)

   ENDFOR
ENDIF
; Now replace the very last and first points:
Y0 = 3.*ARRAY(0)-2.*ARRAY(1)         ; Predicted value of point -1
SMOOTHED(0) = MEDIAN([Y0,ARRAY(0),ARRAY(1)])
Y0 = 3.*ARRAY(LEND)-2.*ARRAY(LEND-1) ; Predicted value of point LEND+1
SMOOTHED(LEND) = MEDIAN([Y0,ARRAY(LEND),ARRAY(LEND-1)])
               
RETURN,SMOOTHED
END
