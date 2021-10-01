;+
; NAME:
;   dav_dn_integrand
;
; PURPOSE:
;
; CALLING SEQUENCE:
;   dav_dn_integrand, r
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
function dav_dn_integrand, r
  common constants
  common global
  common dn_integrand_dummy, redshift, theta_dummy, params_dummy
  theta = theta_dummy
  params = params_dummy
  z = redshift
  t = r[0]
  x_m = r[1]
  x_l = r[2]
  Cov = dav_scatter(t, z)
  Cov_inv = invert(Cov, /double)
  sig_m = sqrt(Cov[0,0])
  sig_l = sqrt(Cov[1,1])
  mean_m = t*theta.alpha
  mean_l = t*theta.beta
  L = theta.L_naught/(theta.h^2)*10^(x_l*sig_l + mean_l)
  M1 = M_naught/theta.h*10^(x_m*sig_m + mean_m)*0.99
  M2 = M_naught/theta.h*10^(x_m*sig_m + mean_m)/0.99
  dndm = mean((dav_dn_dlnm(M1, M2, z, 2, theta, params=params))[*, 1])
  f = dav_flux(L, z, theta.Temp_naught*10^t, theta, params)
  x = [x_m*sig_m, x_l*sig_l]
  cutoff = erf(alog10(f/params.flux_limit))
  cutoff = cutoff lt 0 ? 0 : cutoff
  return, 1./(2.*!pi)*exp(-0.5*(transpose(x) # Cov_inv # x))*dndm*theta.alpha * cutoff
end

function dav_dn, theta, params
  common constants
  common global
  common dn_integrand_dummy
  theta_dummy = theta
  params_dummy = params
  zmin = params.z1
  zmax = params.z2
  tmin = alog10(params.Temp_min/theta.Temp_naught*theta.h^2)
  tmax = alog10(params.Temp_max/theta.Temp_naught)
  n = params.zbins + 1
  z = findgen(n)/(n-1)*(zmax - zmin) + zmin
  dz = (zmax - zmin)/params.zbins
  midz = (z[0:n-2] + z[1:n-1])/2.
  dn = dblarr(params.zbins)
  dndz = dblarr(params.zbins)
  dndv = dblarr(params.zbins)
  dv = dblarr(params.zbins)
  for i = 0, params.zbins - 1 do begin
     redshift = midz[i]
     dndv[i] = dav_recursigrate('dav_dn_integrand', [[tmin, -5., -5.], [tmax, 5., 5.]], eps=1E-2)
     dv[i] = dav_volume(z[i], z[i+1], theta, params)
     dn[i] = dndv[i]*dv[i]
     dndz[i] = dndv[i]*dv[i]/dz
  endfor
;;   return, [[midz], [dndz], [dn], [dndv], [dv]]
  return, dn
end


