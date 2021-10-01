;+
; NAME:
;   dav_r_c
;
; PURPOSE:
;; Calculate the current-epoch radius of curvature
;
; CALLING SEQUENCE:
;   dav_r_c, theta
;
; INPUTS:
;   theta
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
;; Calculate the current-epoch radius of curvature
function dav_r_c, theta
  common constants
  common global
  Omega_K_0 = 1. - (theta.Omega_M_0 + theta.Omega_lambda_0)
  R_c = c/(H_0*theta.h)/sqrt(abs(Omega_K_0))/cm_per_Mpc
  return, R_c
end

