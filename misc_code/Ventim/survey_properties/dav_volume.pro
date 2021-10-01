;+
; NAME:
;   dav_v_integrand
;
; PURPOSE:
;; Integrand for calculating the comoving volume element dV
;
; CALLING SEQUENCE:
;   dav_v_integrand, z
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
;; Integrand for calculating the comoving volume element dV
function dav_v_integrand, z
  common constants
  common global
;;   common r_dummy
  common v_dummy, theta_dummy_v, params_dummy_v
  theta = theta_dummy_v
  params = params_dummy_v
  theta_dummy = theta
  params_dummy = params
  D = dav_d(z, theta)
  E = dav_e(z, theta)
  vol = D^2/E
  return, vol
end

;; Calculate the comoving volume V between z1 and z2
function dav_volume, z1, z2, theta, params
  common constants
  common global
  common v_dummy
  theta_dummy_v = theta
  params_dummy_v = params
  return, (c/cm_per_Mpc)/(theta.h*H_0)*qromb('dav_v_integrand', z1, z2)*params.dOmega
end

