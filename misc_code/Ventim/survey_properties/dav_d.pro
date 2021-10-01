;+
; NAME:
;   dav_d   dav_d
; PURPOSE:
;; Calculate the distance measure D(z)
;
; CALLING SEQUENCE:
;   dav_d, z, theta   dav_d, z, theta
; INPUTS:
;   z, theta   z, theta
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:
;
; OPTIONAL OUTPUTS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;   none
;
; EXAMPLE:
;   dav_d, z, theta
;
; MODIFICATION HISTORY:
;   @Revision@
;-
;; Calculate the distance measure D(z)
function dav_d, z, theta
  common constants
  common global
  Omega_K_0 = 1. - (theta.Omega_M_0 + theta.Omega_lambda_0)
  r = dav_r(z, theta)
  if Omega_K_0 lt -1E-3 then begin
     R_c = dav_R_c(theta)
     D = R_c*sinh(r/R_c)
  endif
  if Omega_K_0 gt 1E3 then begin
     R_c = dav_R_c(theta)
     D = R_c*sin(r/R_c)
  endif
  if Omega_K_0 ge -1E-3 and Omega_K_0 le 1E3 then begin
     D = r
  endif
  return, D
end

