;+
; NAME:
;   dav_omega_lambda
;
; PURPOSE:
;; Calculate Omega_lambda(z)
;
; CALLING SEQUENCE:
;   dav_omega_lambda, z, theta
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
;; Calculate Omega_lambda(z)
function dav_omega_lambda, z, theta
  common constants
  common global
  theta_dummy = theta
  return, theta.Omega_lambda_0/dav_e(z, theta)^2
end
