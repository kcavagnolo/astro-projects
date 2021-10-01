;+
; NAME:
;   dav_e
;
; PURPOSE:
;; Calculate E(z)
;
; CALLING SEQUENCE:
;   dav_e, z, theta
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
;   $Revision: 1.1.1.1 $
;-
;; Calculate E(z)
function dav_e, z, theta
  common constants
  common global
  Omega_K_0 = 1 - (theta.Omega_M_0 + theta.Omega_lambda_0)
  return, sqrt(theta.Omega_M_0*(1.+z)^3 + Omega_K_0*(1.+z)^2 + theta.Omega_lambda_0*(1.+z)^(3*(1+theta.w)))
end
