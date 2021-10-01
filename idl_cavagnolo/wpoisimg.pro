FUNCTION WPOISIMG, IM, SEED, OUTPUT_KIND=OUTPUT_KIND

;+
; NAME:
;	WPOISIMG
;
; PURPOSE:
;	Add Poisson noise to an array.
;
; CATEGORY:
;	Image simulation.
;
; CALLING SEQUENCE:
;	RESULT = WPOISIMG ( IM[, SEED] )
;
; INPUT:
;	IM: A numeric array (byte, integer, long, or float) of arbitrary
;	dimensionality.  This is the array of values around which values in the
;	result will be Poisson-distributed.
;
; OPTIONAL INPUTS:
;	SEED: A longword seed for the random number generator.  If this is not
;	supplied, the value -123456789L is used for generating the first random
;	value.
;	
; KEYWORD PARAMETER:
;	OUTPUT_KIND: The data type of the output array, that is byte, integer,
;	longword, or float.  The words "byte", "int", "integer","long",
;	"longword", and "float", in upper or lower case, are accepted, as are
;	the numeric IDL values 1,2,3,4 for byte, integer, longword, and float.
;
; OUTPUT:
;	WPOISIMG returns a copy of the input array, Poisson noise added.
;
;
; RESTRICTIONS:
;	Negative input values are mapped to a result of 0.  
;
; PROCEDURE:
;	Create an image with values Poisson-distributed around the mean values
;	IM, using Knuth's "Algorithm Q".  (Donald E. Knuth, The Art of
;	Computer Prograwmming, Volume 2, "Seminumerical Algorithms", Addison-
;	Wesley (1969), page 117.  This routine IS NOT VECTORIZED, AND SHOULD RUN
;	SLOWLY.  A deft IDL'er could probably vectorize the algorithm, and
;	anyone who does so is entitled to a gold star.  Where the gaussian and
;	Poisson distributions are essentially identical (mean value > 50) a
;	normal, that is gaussian, distribution is used.
;
; EXAMPLE:
;	Here is how you can create a 100x100 array of values Poisson-distributed
;	around the mean value 5.0, and check the empirical probability against
;	the Poisson distribution:
;
;		n = 100
;		a = REPLICATE(5.0,n,n)
;		b = WPOISIMG ( a, out="byte" )
;		tvscl, b
;		print, STDEV(b,mean), mean, SQRT(5.0)
;		h = histogram ( b )
;		prob = FLOAT(h)/N_ELEMENTS(b)
;		probi = EXP(-5.0)
;		FOR i=0,10 do begin PRINT, i, prob(i), probi & $
;			probi=probi*5.0/(i+1)
;
; MODIFICATION HISTORY:
; 	Written by:	James Hamill
;			Siemens Medical Systems
;			2501 N. Barrington Rd.
;			Hoffman Estates, IL  60195-7372
;			(708)304-7760
;			hamill@sgi.siemens.com
;			February, 1992
;Renamed from poisson_image.pro by A. Graps to go with Wavelet Workbench, March 1996.
;-

ON_ERROR, 1

default_seed = -123456789L
BIG_ENOUGH = 50	; bigger than this, use normal approximation

undefined_type = 0
byte_type = 1
int_type = 2
long_type = 3
float_type = 4
string_type = 7


;-------------------------------------------------------------------------------
;  First part of the code: decide what type output to make.
;-------------------------------------------------------------------------------


sizi = SIZE(im)
dims = sizi(1:sizi(0))
old_type = sizi(sizi(0)+1)

IF N_ELEMENTS(output_type) EQ 0 tHEN BEGIN
			; if keyword doesn't specify the type ...

  CASE old_type OF
    byte_type:	output_type = int_type
    int_type:	output_type = long_type
    else:	output_type = float_type
  ENDCASE

ENDIF ELSE BEGIN	; if keyword is set then follow the instructions


  sizok = SIZE(output_kind)	; text or numeric parameter?
  ok_type = sizok(sizok(0)+1)


  CASE ok_type OF

    int_type: output_type = output_kind

    long_type: output_type = output_kind

    string_type: BEGIN

      CASE STRUPCASE(output_kind) OF
	"BYTE":		output_type = byte_type
	"INT":		output_type = int_type
	"INTEGER":	output_type = int_type
	"LONG":		output_type = long_type
	"LONGWORD":	output_type = long_type
	ELSE: MESSAGE,"Invalid output type"
      ENDCASE

    END

    ELSE: MESSAGE, "Invalid output_kind keyword."

  ENDCASE

  IF (output_type LT byte_type) OR (output_type GT float_type) THEN $
	message, "Invalid output_kind keyword"
		; thus we trap nonsense like string, complex, or structure


ENDELSE

result = MAKE_ARRAY(dimension=dims,type=output_type)		; (zeroed)


;-------------------------------------------------------------------------------
;  Check the random number generator seed, set it if necessary.
;-------------------------------------------------------------------------------


IF n_params(dummy) GE 2 THEN BEGIN
  sizs = SIZE(seed)
  seed_type = sizs(sizs(0)+1)
  IF seed_type EQ undefined_type THEN BEGIN
    seed = default_seed			; undefined parameter: set it
  ENDIF ELSE BEGIN			; check that seed is a long scalar
    if (seed_type ne long_type) or (sizs(0) ne 0) then $
	MESSAGE,"Invalid seed."
  ENDELSE
ENDIF ELSE seed=default_seed


;-------------------------------------------------------------------------------
;  Consider the small ones: use the Poisson distribution for these.  This is
;  Knuth's algorithm Q.
;-------------------------------------------------------------------------------


ROI = WHERE( (im gt 0) AND (im lt BIG_ENOUGH), count)
IF count gt 0 THEN BEGIN

  FOR i=0L,n_elements(ROI)-1 DO BEGIN

    j = ROI(i)
    p = EXP(-DOUBLE(im(j)))
    q = 1D0
    n = -1

    WHILE q GE p DO BEGIN
      n = n + 1
      u = RANDOMU(seed)
      q = q*u
    ENDWHILE

    result(j) = n > 0
    ;if i mod 10000L eq 0 then wstbusy,$
	;"Poisson distribution ... working on element #"+strtrim(i)+"."

  ENDFOR

ENDIF


;-------------------------------------------------------------------------------
;  Consider the region in which the normal distribution can be used.  Round to
;  the nearest whole number.
;-------------------------------------------------------------------------------


ROI = WHERE ( im GE BIG_ENOUGH, count )

IF count NE 0 THEN BEGIN

  n_values = N_ELEMENTS(ROI)
  result(ROI) = FIX(0.5 + im(ROI) + SQRT(im(ROI))*RANDOMN(seed,n_values))

ENDIF



RETURN, result


END
