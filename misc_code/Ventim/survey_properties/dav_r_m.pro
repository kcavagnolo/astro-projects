;+
; NAME:
;   dav_r_m
;
; PURPOSE:
;; Calculate the radius of a sphere enclosing M*rho_c
;
; CALLING SEQUENCE:
;   dav_r_m, M, theta
;
; INPUTS:
;   M, theta
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
;; Calculate the radius of a sphere enclosing M*rho_c
function dav_r_m, M, theta
  common constants
  common global
  return, (3.*M/(4.*!pi*theta.Omega_M_0*dav_rho_c(theta)))^(1./3.)
end
