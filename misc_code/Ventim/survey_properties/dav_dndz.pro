;+
; NAME:
;   dav_dndz
;
; PURPOSE:
;
; CALLING SEQUENCE:
;   dav_dndz, theta, params
;
; INPUTS:
;   theta, params
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
function dav_dndz, theta, params
  common constants
  common global
  theta_dummy = theta
  params_dummy = params
  zmin = params.z1
  zmax = params.z2
  n = params.zbins + 1
  z = findgen(n)/(n-1)*(zmax - zmin) + zmin
  dz = (zmax - zmin)/params.zbins
  midz = (z[0:n-2] + z[1:n-1])/2.
  dndv = dblarr(params.zbins)
  dvdz = dblarr(params.zbins)
  for i = 0, params.zbins - 1 do begin
     redshift = midz[i]
     dndm = dav_dn_dlnm(3.0D14*.7, 5D15, redshift, 100, theta, params=params)
     dndv[i] = int_tabulated(alog10(dndm[*,0]), dndm[*,1])
     dv = dav_volume(z[i], z[i+1], theta, params)
     dvdz[i] = dv/dz
  endfor
 return, [[midz], [dndv], [dvdz]]
end

