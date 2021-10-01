;+
; NAME:
;   dav_w_r(m)
;
; PURPOSE:
;; Window function dav_w_r(m), as a function in wavenumber k
;
; CALLING SEQUENCE:
;   dav_w_r(m), as a function in wavenumber k
;
; INPUTS:
;   as a function in wavenumber k
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
;; Window function dav_w_r(m), as a function in wavenumber k
function dav_W, k, r_m
  common constants
  common global
  return, double(3)*(sin(k*r_m) - k*r_m*cos(k*r_m))/(k*r_m)^3.
end

