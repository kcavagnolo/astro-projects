;+
; NAME:
;   dav_lcdm
;
; PURPOSE:
;; Return a struct of cosmological and cluster parameters
;
; CALLING SEQUENCE:
;   dav_lcdm
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
;   $Revision: 1.1.1.1 $
;-
;; Return a struct of cosmological and cluster parameters
function dav_lcdm
  return, {h:0.7, $              ;0
           w:-1.0, $             ;1
           Omega_M_0:0.3, $      ;2
           Omega_lambda_0:0.7, $ ;3
           Omega_B_0:0.02, $     ;4
           sigma_8:0.90, $       ;5
           Temp_naught:8.2, $    ;6
           L_naught:1D45, $      ;7
           alpha:1.5D, $         ;8
           beta:1.8D}            ;9
end

