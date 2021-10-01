;+
; NAME:
;   mvnormal_vol2
;
; PURPOSE:
;
; CALLING SEQUENCE:
;   mvnormal_vol2, n, sigma
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
function mvnormal_vol2, n, sigma
  common mvparams, h
  h = sigma
  D1 = [[replicate(-1./0, n)], [replicate(-4*sigma, n)]]
  D2 = [[replicate(-4*sigma, n)], [replicate(4*sigma, n)]]
  D3 = [[replicate(-4*sigma, n)], [replicate(1./0, n)]]
  v1 = recursigrate('mvnormal', D1, eps=1E-2)
  v2 = recursigrate('mvnormal', D2, eps=1E-2)
  v3 = recursigrate('mvnormal', D3, eps=1E-2)
  return, v1 + v2 + v3
end
