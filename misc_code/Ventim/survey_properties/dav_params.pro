;+
; NAME:
;   dav_params
;
; PURPOSE:
;; Return a struct of the params properties
;
; CALLING SEQUENCE:
;   dav_params
;
; INPUTS:
;   {Temp_min:3.21, $
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
;; Return a struct of the params properties
function dav_params
  return, {Temp_min:4.66, $
           Temp_max:15.0, $
           tbins:10, $
           z1:0.01, $
           z2:2.00, $
           zbins:10, $
           E1:0.5, $
           E2:2.0, $
           dOmega:2.*!pi, $
           flux_limit:3.3E-14, $
           growth_fn:'dav_d_growth_basic'}
end

