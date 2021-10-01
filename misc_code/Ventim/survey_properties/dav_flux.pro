;+
; NAME:
;   dav_flux
;
; PURPOSE:
;; Calculate the X-ray energy flux of a cluster at z with L, T
;
; CALLING SEQUENCE:
;   dav_flux, L, z, T, theta, params
;
; INPUTS:
;   L, z, T, theta, params
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
;; Calculate the X-ray energy flux of a cluster at z with L, T
function dav_flux, L, z, T, theta, params
  common constants
  common global
  E1 = params.E1
  E2 = params.E2
  D_L = dav_d_l(z, theta)*cm_per_Mpc
  x1 = -1*E1/T
  x2 = -1*E2/T
  flux = L/(4.*!pi*D_L^2)*(exp(x2*(1.+z)) - exp(x1*(1.+z)))/(exp(x2) - exp(x1))
  return, flux
end

