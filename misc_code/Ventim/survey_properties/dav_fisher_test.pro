;+
; NAME:
;   dav_map
;
; PURPOSE:
;
; CALLING SEQUENCE:
;   dav_map, obj_func, x, y, args
;
; INPUTS:
;   obj_func, x, y, args
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
function dav_map, obj_func, x, y, args
  m = n_elements(x)
  n = n_elements(y)
  map = dblarr(m, n)
  for i = 0, m - 1 do begin
     for j = 0, n - 1 do begin
        map[i, j] = call_function(obj_func, x[i], y[j], args)
     endfor
  endfor
  return, map
end
