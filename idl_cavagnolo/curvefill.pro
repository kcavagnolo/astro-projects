;+ 
; NAME:
; curvefill
;
; PURPOSE:
;    Shade the region between two curves
;
; CALLING SEQUENCE:
;      curvefill, x, y1, y2, COLOR=, OUTLINECOLOR=, OUTHICK=
;   
; INPUTS:
;  x -- x-values of the curve
;  y1 -- lower set of y-values
;  y2 -- upper set of y-values
;
; RETURNS:
;
; OUTPUTS:
;
; OPTIONAL KEYWORDS:
; color=  -- Color for shading
;
; OPTIONAL OUTPUTS:
;
; COMMENTS:
;
; EXAMPLES:
;
; PROCEDURES CALLED:
;
; REVISION HISTORY:
;   18-May-2006 Written by Joe H.
;-
;------------------------------------------------------------------------------
pro curvefill, x, y1, y2, color = color, outlinecolor = outlinecolor $
               , _EXTRA = EXTRA, OUTHICK = OUTHICK

if  N_params() LT 3  then begin 
    print,'Syntax - ' + $
          'curvefill, x, y1, y2, COLOR= [v1.1]'
    return
endif 

n1 = n_elements(x)
IF n_elements(y1) NE n1 OR n_elements(y2) NE n1 or $
  N_elements(y1) NE n_elements(y2) THEN message, 'problem with array sizes'

;; Create a polygon to fill.
xpoly = [x[0], x, reverse(x)]
ypoly = [y1[0], y2, reverse(y1)]

PolyFill, xpoly, ypoly, Color = color, NOCLIP = 0, _EXTRA = EXTRA
  
IF n_elements(OUTLINECOLOR) NE 0 THEN $
  plots, xpoly, ypoly, Color = outlineColor, Thick = OUTHICK, NOCLIP = 0
return

END
