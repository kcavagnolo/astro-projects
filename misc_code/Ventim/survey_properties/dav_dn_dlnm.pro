;+
; NAME:
;  dav_dn_dlnM
;
; PURPOSE:
;; Caculate the mass function dav_for some number of bins
;
; CALLING SEQUENCE:
;   dav_for some number of bins
;
; INPUTS:
;   M_min, M_max, z, bins, theta
;
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
;; Caculate the mass function dav_for some number of bins
function dav_dn_dlnM, M_min, M_max, z, bins, theta, params=params
  n = bins
  M_norm = findgen(n)/(n-1)
  m = M_min*(M_max/M_min)^M_norm
  sigmam = dav_sigma(theta, params, m, z)
  nm = n_elements(m)
  dsig=sigmam[1:nm-1]-sigmam[0:nm-2]
  dmass=m[1:nm-1]-m[0:nm-2]
  midsig=(sigmam[0:nm-2]+sigmam[1:nm-1])/2.0
  midmass=(m[0:nm-2]+m[1:nm-1])/2.0
  dlnsig_dlnm = -dsig/dmass*midmass/midsig
  dnmj_dlnsig = dav_dn_M_d_ln_inv_sigma(midmass, z, theta, params=params)
  dndlnm = dnmj_dlnsig * dlnsig_dlnm
  return, [[midmass], [dndlnm]]
end

