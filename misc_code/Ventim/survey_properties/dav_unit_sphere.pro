;+
; NAME:
;   unit_sphere
;
; PURPOSE:
;
; CALLING SEQUENCE:
;   unit_sphere, r
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
function unit_sphere, r
  radix = 1 - total(r^2)
  return, radix gt 0 ? sqrt(radix) : 0
end
