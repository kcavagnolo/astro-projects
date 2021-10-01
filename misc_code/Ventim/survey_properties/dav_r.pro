;+
; NAME:
;   dav_r_integrand
;
; PURPOSE:
;; Integrand for calculating the comoving distance coord
;
; CALLING SEQUENCE:
;   dav_r_integrand, z
;
; INPUTS:
;   z
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
;; Integrand for calculating the comoving distance coord
function dav_r_integrand, z
  common constants
  common global
  common r_dummy, theta_dummy
  theta = theta_dummy
  return, 1./dav_e(z, theta)
end

;; Calculate the comoving distance coord for redshift z
function dav_r, z, theta
  common constants
  common global
  common r_dummy, theta_dummy
  theta_dummy = theta
  r = c/theta.h/H_0*qromb('dav_r_integrand', 0, z, eps=1E-3)/cm_per_Mpc
  return, r
end

