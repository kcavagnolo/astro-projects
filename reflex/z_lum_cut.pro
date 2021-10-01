pro z_lum_cut, input, z0, lx0

;
; NAME:
;
; PURPOSE:
;
; EXPLANATION:
;
; CALLING SEQUENCE:
;
; INPUTS:
;
; OUTPUTS:
;
; MODIFICATION HISTORY:
;

fitfile = mrdfits(input, 1, hdr)
z = fitfile.redshift
lx = fitfile.lx
name = fitfile.name

goodname = strarr(n_elements(name)))
goodlx = fltarr(n_elements(name)))
FOR i = 0, n_elements(z)-1 DO BEGIN
    IF (z[i] LT z0) THEN goodname[i] = name[i] AND goodlx[i] = lx[i]
    ELSEIF

RETURN
END
