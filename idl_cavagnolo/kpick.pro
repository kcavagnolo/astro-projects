;+
; NAME:
;       Kpick
; PURPOSE:
;       Randomly select n elements of a vector.
; USAGE:
;       NewIndex = Kpick( M, n, [SEED = ] )
; INPUT:
;       M = length of vector
;       n = number of elements to select
;
; OPTIONAL INPUT-OUTPUT:
;       SEED = random number seed to be passed to RANDOMU
;              Upon return, it will updated as a vector
;              containing a new seed
; OUTPUT:
;       Kpick returns n randomly shuffled integers between 0 and M-1
; EXAMPLE:
;       To select N elements of a vector V,
;
;       V = V[ Kpick(N_ELEMENTS(V), N) ]
;
; METHOD:
;       This routine is a vectorized form of the following literal
;       interpretation of Knuth's algorithm R.
;
;       I = LINDGEN(n)
;       FOR t=n, Set-1 DO BEGIN
;           M = RANDOMU(Seed, /long) MOD t
;           IF M LT n THEN BEGIN
;               I[M] = t
;           ENDIF
;       ENDFOR
;       RETURN, I
;
; REVISION HISTORY:
;       Written,  Richard Younger, MIT/LL 5/2002
;               Based on Algorithm R, from Knuth, D., The Art of
;               Computer Programming, Vol 2, Ch. 3.4.2
;       RY 5/2002 Replaced random index generating line based on
;               MOD with slower but more statistically correct
;               statement based on floating point #s and FLOOR().
;               Feel free to use the faster MOD if you're nowhere near
;               2^31 ~ 2E9 elements.
;-

FUNCTION Kpick, Set, n, SEED=seed

  COMPILE_OPT idl2
  I = LINDGEN(n)
  
  ; generate a vector of uniform random #s between 0 and
  ; [n,...,Set]
  ; M = RANDOMU(Seed, Set-n, /long) MOD (lindgen(Set-n)+n)
  M = FLOOR(RANDOMU(Seed, Set-n, /double)*(lindgen(Set-n)+n))

  t_pr = WHERE(M LT n)          ; t_pr + n = Knuth's t
  I[M[t_pr]] = t_pr+n

  RETURN, I

END
