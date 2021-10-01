;+
; NAME:
;   dav_t(k)
;
; PURPOSE:
;; Calculate the transfer function dav_t(k)
;
; CALLING SEQUENCE:
;   dav_t(k)
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
;; Calculate the transfer function dav_t(k)
function dav_t, k, theta
  common constants
  common global
  q_val = dav_q(k, theta)
  return, alog(1 + 2.34*q_val)/(2.34*q_val)/(1. + 3.89*q_val + (16.1*q_val)^2. + (5.46*q_val)^3. + (6.71*q_val)^4.)^(1/4.)
end
