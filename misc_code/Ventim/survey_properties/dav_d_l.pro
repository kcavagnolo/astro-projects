;+
; NAME:
;   dav_d_l
;
; PURPOSE:
;; Calculate the luminosity distance D_L(z)
;
; CALLING SEQUENCE:
;   dav_d_l, z, theta
;
; INPUTS:
;   z, theta
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
;; Calculate the luminosity distance D_L(z)
function dav_d_l, z, theta
  common constants
  common global
  D_L = (1.+z)*dav_d(z, theta)
  return, D_L
end

