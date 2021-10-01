FUNCTION PERMUTE,M, RSEED
;+
; NAME:
;	PERMUTE
; PURPOSE:
;	Randomly permute the elements of a vector
; USAGE:
;	NewIndex = PERMUTE( M, [ Seed] )
; INPUT:
;	M = length of vector
; OPTIONAL
;	SEED = random number seed to be passed to RANDOMU (optional)
; OUTPUT:
;	PERMUTE returns M randomly shuffled integers between 0 and M-1
; EXAMPLE:
;	To shuffle the elements of a vector V,
;
;	V = V( PERMUTE(N_ELEMENTS(V))	
;
; WARNING: 
;	This can be pretty slow. 
;
; REVISION HISTORY:
;	Written,  H.T. Freudenreich, HSTX, 2/95
;-

; Select M numbers at random, repeating none.
 ORDER=LINDGEN(M)
 INDICES=ORDER

; Get the random number seed:      
 IF N_PARAMS() EQ 2 THEN BEGIN
	IF RSEED GT 0. THEN SEED=RSEED ELSE SEED=LONG(SYSTIME(1))
 ENDIF ELSE SEED=LONG(SYSTIME(1))

; The first number is easy:
NLEFT = M
J=LONG(RANDOMU(SEED)*NLEFT)
INDICES(0) = J
FOR II = 1L, M-1L DO BEGIN
; Squeeze out the element that was just chosen--
  NLEFT = NLEFT - 1L
  CASE J OF
     0L    : ORDER = ORDER(1:NLEFT)
     NLEFT : ORDER = ORDER(0:NLEFT-1)
     ELSE  : ORDER = [ORDER(0:J-1),ORDER(J+1:NLEFT)]
  ENDCASE
; --and choose another:
  J=LONG(RANDOMU(SEED)*NLEFT)
  INDX=ORDER(J)
  INDICES(II) = LONG(INDX)            ; store it 
ENDFOR

RETURN,INDICES
END
