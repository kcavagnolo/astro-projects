pro plotdate, PROG, SIZE = size, DEVICE = device, DATA = data, $
	NORMAL = normal, X = x, Y = y
;+
; 	PROCEDURE: 
;		PLOTDATE
; 	PURPOSE:
;		This procedure puts the user name, calling program and system 
;		time and date in the corner of a plot.  
; 	CALLING SEQUENCE:
;		plotdate, Prog, [Charsize=, Charthick=,/device,/data, /normal]
; 	OPTIONAL INPUTS:
;		PROG -- Character string containing the name of the calling
;			program (Default = 'INTERACTIVE')
;	KEYWORD INPUTS:
;		SIZE -- Specify the character size.  (Default = 0.35)
;		X, Y -- Location to put the string.  (Default is lower right 
;			corner, depedning on the current device size).  
;		DEVICE -- If present and non-zero, X and Y are in DEVICE 
;			  coordinates.  Ignored if X, Y are not both specified.
;			  (Default = 1)
;		DATA -- If present and non-zero, X and Y are in DATA 
;			coordinates.  Ignored if X, Y are not both specified.
;			(Default = 0)
;		NORMAL -- If present and non-zero, X and Y are NORMALIZED
;			  coordinates.  Ignored if X, Y are not both specified.
;	NOTE: Only one of [DEVICE, DATA, NORMAL] should be specified.  For 
;	      more information regarding these parameters, check documentation
;	      of XYOUTS.
; 	REVISION HISTORY
; 		Modified from RCB::[BOHLIN.IDL]PLOTDATE.PRO 
; 		90DEC22-CHANGE XYOUTS FOR V2 AND SIMPLIFY-RCB
;		Converted for general use in MOUSSE.  Keywords added, user name
;		obtained from OS rather than hard-coded.   
;		Joel D. Offenberg, HSTX, 26-Mar-93
;-

IF (n_elements(PROG) eq 0) THEN PROG = "INTERACTIVE"

USER = strupcase(getenv('USER'))

IF (keyword_set(X) AND keyword_set(Y)) THEN BEGIN
	keyck = keyword_set(NORMAL) + keyword_set(DEVICE) + keyword_set(DATA)
	IF (keych eq 0) THEN DEVICE = 1 
	IF (keych gt 1) THEN BEGIN
	   message,/inf,'ERROR - Only one of [DEVICE, DATA, NORMAL] can be set'
	   return
	endIF
endIF ELSE BEGIN
	X = 0.75
	Y = 0.01
	NORMAL = 1
	DEVICE = 0
	DATA = 0
endELSE

IF not(keyword_set(SIZE)) then SIZE = 0.35

xyouts,X,Y,USER+': '+PROG+'  '+STRMID(!stime,0,17),SIZE=size, $
	DEVICE=device, NORMAL=normal, DATA=data

return
end
