;+
; NAME:
;   sphere_volume
;
; PURPOSE:
;
; CALLING SEQUENCE:
;   sphere_volume, n, r
;
; INPUTS:
;   n, r
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
function sphere_volume, n, r
  return, !pi^((n+1)/2.)*r^(n+1)/gamma((n+1)/2. + 1)
end
