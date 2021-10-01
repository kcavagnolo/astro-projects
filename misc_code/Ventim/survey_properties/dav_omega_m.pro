;+
; NAME:
;   dav_omega_m
;
; PURPOSE:
;; Calculate Omega_M(z)
;
; CALLING SEQUENCE:
;   dav_omega_m, z, theta
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
;; Calculate Omega_M(z)
function dav_omega_m, z, theta
  common constants
  common global
  theta_dummy = theta
  return, theta.Omega_M_0*(1.+z)^3/dav_e(z, theta)^2
end
