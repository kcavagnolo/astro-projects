FUNCTION LOGSCL, Image, MINVAL=Minval, MAXVAL=Maxval, TOP=top
;+
; NAME:
;       LOGSCL
;
; PURPOSE:
;       This scales an image using a log function table.
;
; CATEGORY:
;       Image processing
;
; CALLING SEQUENCE:
;       Result = LOGSCL( Image )
;
; INPUT POSITIONAL PARAMETERS:
;       Image:    Image to be scaled; can actually be any array.
;
; INPUT KEYWORD PARAMETERS:
;       MINVAL:   Minimum value for log stretch.  Default=min(Image)
;       MAXVAL:   Maximum value for log stretch.  Default=max(Image)
;       TOP:      Top value for output.  Default=!D.table_size-1
;
; FUNCTION RESULT:
;       Byte array with log stretch.  Negative and zero values are
;       included nicely in the stretch range instead of being clipped.
;
; MODIFICATION HISTORY:
;       Written by:     RSH, RITSS, 5 Jan 99
;       16 Feb 99 - TOP keyword added.  RSH
;        3 Jul 00 - RETALL changed to RETURN in doc section;
;                   square brackets.  RSH
;       25 Jul 00 - RETURN in doc section returns -1.  RSH
;-

IF n_params(0) LT 1 THEN BEGIN
    print, 'CALLING SEQUENCE:  Result=LOGSCL(Image)'
    print, 'KEYWORD PARAMETERS:  MINVAL, MAXVAL, TOP'
    RETURN,-1
ENDIF

IF n_elements(minval) LT 1 THEN minval=min(image)
IF n_elements(maxval) LT 1 THEN maxval=max(image)
IF n_elements(top) LT 1 THEN top=!D.table_size-1

logtab = byte(round(alog10(indgen(top+1)+1)/alog10(top+1)*top))
RETURN,logtab[bytscl(image,minval,maxval,top=top)]
END
