;+
; NAME:
;   dav_ocdm
;
; PURPOSE:
;; Return a struct of cosmological and cluster parameters
;
; CALLING SEQUENCE:
;   dav_ocdm
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
; EXAMPLE:
;
; MODIFICATION HISTORY:
;   @Revision@
;-
;; Return a struct of cosmological and cluster parameters
function dav_ocdm
  return, {h:0.7, $
           w:0.0, $
           Omega_M_0:0.3, $
           Omega_lambda_0:0.0, $
           Omega_B_0:0.02, $
           sigma_8:0.90, $
           np:1.0, $
           Temp_naught:8.2, $
           L_naught:1D45, $
           alpha:1.5D, $
           beta:1.8D}
end

