;+
; NAME:
;   dav_dn_wrapper
;
; PURPOSE:
;
; CALLING SEQUENCE:
;   dav_dn_wrapper, x, y, args
;
; INPUTS:
;   x, y, args
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
function dav_dn_wrapper, x, y, args
  params = args.params
  theta = args.theta
  m = args.m
  i = args.i
  j = args.j
  theta.(i) = x
  theta.(j) = y
  n = dav_dn(theta, params)
  return, total((m-n)^2/n)
end
