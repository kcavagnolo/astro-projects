;+
; NAME:
;   dav_constants
;
; PURPOSE:
;; Establish physical constants, and cosmological and cluster parameters
;
; CALLING SEQUENCE:
;
; INPUTS:
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
; RESTRICTIONS:
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;   @Revision@
;-
pro dav_constants
  common constants, G, c, H_0, gm_per_Msun, cm_per_Mpc, delta_c, np, M_naught
  common global, sigma_tabulated, sigma_norm, d_norm
  ;; constants
  gm_per_Msun = 2.D30*1000.
  cm_per_Mpc = 3.08D18*1D6
  G = 6.67259D-8*gm_per_Msun/cm_per_Mpc^3
  c = 2.99792e+10
  H_0 = 100.*1000.*100./cm_per_Mpc
  np = 1.0
  delta_c = 1.686D
  M_naught = 1D15
end

