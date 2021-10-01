;+
; NAME:
;   dav_q
;
; PURPOSE:
;; Used by the transfer function
;
; CALLING SEQUENCE:
;   dav_q, k, theta
;
; INPUTS:
;   k, theta
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
;; Used by the transfer function
function dav_q, k, theta
  common constants
  common global
  Gamma = theta.Omega_M_0*theta.h*exp(-1*theta.Omega_B_0/theta.h^2*(1.+sqrt(2.*theta.h)/theta.Omega_M_0))
  return, k/(Gamma*theta.h)
end
