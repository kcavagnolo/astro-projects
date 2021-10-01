;+
; NAME:
;   dav_rho_c
;
; PURPOSE:
;; Calculate the critical density
;
; CALLING SEQUENCE:
;   dav_rho_c, theta
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
;; Calculate the critical density
function dav_rho_c, theta
  common constants
  common global
  return, 3*(H_0*theta.h)^2./(8.*!pi*G)
end
