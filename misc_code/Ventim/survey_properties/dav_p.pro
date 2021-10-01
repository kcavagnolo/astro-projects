;+
; NAME:
;   dav_p
;
; PURPOSE:
;; Calculate the transformed initial power spectrum P(k)
;
; CALLING SEQUENCE:
;   dav_p, k, theta
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
;; Calculate the transformed initial power spectrum P(k)
function dav_p, k, theta
  common constants
  common global
  return, k^np * dav_T(k, theta)^2
end
