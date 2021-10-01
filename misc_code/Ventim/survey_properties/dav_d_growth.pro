;+
; NAME:
;   dav_d_growth
;
; PURPOSE:
;; Calculate the cluster growth function
;
; CALLING SEQUENCE:
;   dav_d_growth, z, theta
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
;; Calculate the cluster growth function
function dav_d_growth_basic, z, theta
  Omega_M = dav_Omega_M(z, theta)
  Omega_l = dav_Omega_lambda(z, theta)
  return, 5./2.*Omega_M/(1.+z)/(Omega_M^(4./7.) - Omega_l + (1. + Omega_M/2.)*(1. + Omega_l/70.))
end

function dav_d_growth, z, theta, params=params
  common constants
  common global
  if n_elements(d_norm) eq 0 then begin
     d_norm = 1./call_function(params.growth_fn, 0, theta)
  endif
  return, d_norm*call_function(params.growth_fn, z, theta)
end
