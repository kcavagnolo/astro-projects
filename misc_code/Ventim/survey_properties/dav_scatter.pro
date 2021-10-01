;+
; NAME:
;   dav_scatter
;
; PURPOSE:
;; Return a covariance matrix for Prob(m,l|t,z)
;
; CALLING SEQUENCE:
;   dav_scatter, t, z
;
; INPUTS:
;   t, z
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
;; Return a covariance matrix for Prob(m,l|t,z)
function dav_scatter, t, z
  sig_m = 0.002
  sig_l = 0.002
  rho = 0.0
  cov = [[sig_m*sig_m, rho*sig_m*sig_l], [rho*sig_l*sig_m, sig_l*sig_l]]
  return, cov
end

