FUNCTION SQRTSCL, Image, MINVAL=Minval, MAXVAL=Maxval, TOP=top
;+
; NAME:
;       SQRTSCL
;
; PURPOSE:
;       This scales an image using a square root function table.
;
; CATEGORY:
;       Image processing
;
; CALLING SEQUENCE:
;       Result = SQRTSCL( Image )
;
; INPUT POSITIONAL PARAMETERS:
;       Image:    Image to be scaled; can actually be any array.
;
; INPUT KEYWORD PARAMETERS:
;       MINVAL:   Minimum value for square root stretch.  Default=min(Image)
;       MAXVAL:   Maximum value for square root stretch.  Default=max(Image)
;       TOP:      Top value for output.  Default=!D.table_size-1
;
; FUNCTION RESULT:
;       Byte array with sqrt stretch.  Negative and zero values are
;       included nicely in the stretch range instead of being clipped.
;
; MODIFICATION HISTORY:
;       Written by:     RSH, RITSS, 29 August 2000
;-

IF n_params(0) LT 1 THEN BEGIN
    print, 'CALLING SEQUENCE:  Result=SQRTSCL(Image)'
    print, 'KEYWORD PARAMETERS:  MINVAL, MAXVAL, TOP'
    RETURN,-1
ENDIF

IF n_elements(minval) LT 1 THEN minval=min(image)
IF n_elements(maxval) LT 1 THEN maxval=max(image)
IF n_elements(top) LT 1 THEN top=!D.table_size-1

sqrttab = byte(round(sqrt(indgen(top+1))/sqrt(top)*top))
RETURN,sqrttab[bytscl(image,minval,maxval,top=top)]
END
