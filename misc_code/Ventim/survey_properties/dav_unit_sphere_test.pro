;+
; NAME:
;   unit_sphere_test
;
; PURPOSE:
;
; CALLING SEQUENCE:
;   unit_sphere_test, n
;
; INPUTS:
;   n
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;
; OPTIONAL OUTPUTS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;   @Revision@
;-
function unit_sphere_test, n
  vol = sphere_volume(n, 1)
  return, abs(vol - unit_sphere_vol(n))/vol
end
