;+
; NAME:
;   mvnormal_test
;
; PURPOSE:
;
; CALLING SEQUENCE:
;   mvnormal_test, n
;
; INPUTS:
;   n
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
function mvnormal_test, n
  sigma = 0.1
  vol1 = mvnormal_vol(n, sigma)
  vol2 = 1.
  return, abs(vol1 - vol2)/vol2
end
