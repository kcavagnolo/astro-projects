;+
; NAME:
;   mvnormal_test2
;
; PURPOSE:
;
; CALLING SEQUENCE:
;   mvnormal_test2, n
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
function mvnormal_test2, n
  sigma = 1.
  vol1 = mvnormal_vol2(n, sigma)
  vol2 = (sqrt(2*!pi)*sigma)^n
  return, abs(vol1 - vol2)/vol2
end
