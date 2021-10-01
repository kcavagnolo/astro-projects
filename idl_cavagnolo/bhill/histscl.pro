FUNCTION HISTSCL, Image, MINVAL=Minval, MAXVAL=Maxval, TOP=top
;+
; NAME:
;       HISTSCL
;
; PURPOSE:
;       Wrapper for IDL histogram equalization.
;
; CATEGORY:
;       Image processing
;
; CALLING SEQUENCE:
;       Result = HISTSCL( Image )
;
; INPUT POSITIONAL PARAMETERS:
;       Image:    Image to be scaled; can actually be any array.
;
; INPUT KEYWORD PARAMETERS:
;       MINVAL:   Minimum value for stretch.  Default=min(Image)
;       MAXVAL:   Maximum value for stretch.  Default=max(Image)
;       TOP:      Top value for output.  Default=!D.table_size-1
;
; FUNCTION RESULT:
;       Byte array with sqrt stretch.  Negative and zero values are
;       included nicely in the stretch range instead of being clipped.
;
; MODIFICATION HISTORY:
;       Written by:     RSH, RITSS, 29 August 2000
;       Kluge because HIST_EQUAL didn't handle unsigned datatypes until V5.4
;       No need to compute offset prior to HIST_EQUAL at least since V5.1   
;         W. Landsman
;-

IF n_params(0) LT 1 THEN BEGIN
    print, 'CALLING SEQUENCE:  Result=HISTSCL(Image)'
    print, 'KEYWORD PARAMETERS:  MINVAL, MAXVAL, TOP'
    RETURN,-1
ENDIF

 IF n_elements(top) LT 1 THEN top=!D.table_size-1

; Need a kluge because HIST_EQUAL didn't handle unsigned datatypes until IDL
; V5.4

if (size(image,/TYPE) GE 12) and (!VERSION.RELEASE LT 'V5.4') then $
        RETURN,hist_equal(long(image),minv=minval,maxv=maxval,top=top) $
   else RETURN, hist_equal(image,minv=minval,maxv=maxval,top=top)
END
