;+
; NAME:
;   mvnormal
;
; PURPOSE:
;
; CALLING SEQUENCE:
;   mvnormal, r
;
; INPUTS:
;   r
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
function mvnormal, r
  common mvparams
  n = n_elements(r)
  return, exp(-0.5*total(r^2))/(sqrt(2*!pi))^n
end
