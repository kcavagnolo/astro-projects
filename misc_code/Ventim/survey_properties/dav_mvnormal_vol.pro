;+
; NAME:
;   mvnormal_vol
;
; PURPOSE:
;
; CALLING SEQUENCE:
;   mvnormal_vol, n, sigma
;
; INPUTS:
;   n, sigma
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
function mvnormal_vol, n, sigma
  common mvparams, h
  h = sigma
  D = [[replicate(-4, n)], [replicate(4, n)]]
  return, recursigrate('mvnormal', D, eps=1E-2)
end
