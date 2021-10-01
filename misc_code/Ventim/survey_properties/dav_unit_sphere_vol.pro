;+
; NAME:
;   unit_sphere_vol
;
; PURPOSE:
;
; CALLING SEQUENCE:
;   unit_sphere_vol, n
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
function unit_sphere_vol, n
  D = [[replicate(-1.5, n)], [replicate(1.5, n)]]
  return, 2*recursigrate('unit_sphere', D, eps=1E-2)
end
