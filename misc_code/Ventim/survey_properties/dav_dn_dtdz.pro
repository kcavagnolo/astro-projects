;+
; NAME:
;   dav_dn_dtdz
;
; PURPOSE:
;
; CALLING SEQUENCE:
;   dav_dn_dtdz
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
function dav_dn_dtdz_integrand, r
  common constants
  common global
  common dav_dn_dtdz_integrand_dummy, redshift, theta_dummy, params_dummy
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

function dav_dn_dtdz, theta, params
  common constants
  common global
  common dav_dn_dtdz_integrand_dummy

  restore, 'tftest.dat'
  plot,tf[*,0],tf[*,1],/xlog,/ylog,xrange=[Tmin, Tmax],xstyle=1,yrange=[5E-11,7E-5],ystyle=1, $
       xtitle=textoidl('T_X [keV]'),ytitle=textoidl('dn_M/dlnT_X (h^3_{70} Mpc^{-3})'),yminor=10

  theta_dummy = theta
  params_dummy = params
  z1 = params.z1
  z2 = params.z2
  t1 = alog10(params.Temp_min/theta.Temp_naught)
  t2 = alog10(params.Temp_max/theta.Temp_naught)
  nz = params.zbins + 1
  nt = params.tbins + 1
  z = findgen(nz)/(nz-1)*(z2 - z1) + z1
  t = findgen(nt)/(nt-1)*(t2 - t1) + t1
  dz = (z2 - z1)/params.zbins
  dt = (t2 - t1)/params.tbins
  midz = (z[0:nz-2] + z[1:nz-1])/2.
  midt = (t[0:nt-2] + t[1:nt-1])/2.
  midTemp = theta.Temp_naught*10^midt
  dn_dtdz = dblarr(params.zbins, params.tbins)
  dv = dblarr(params.zbins)
  dn_dtdv = dblarr(params.tbins)
  for i = 0, params.zbins - 1 do begin
     redshift = midz[i]
     dv[i] = dav_volume(z[i], z[i+1], theta, params)
     for j = 0, params.tbins - 1 do begin
        dn_dtdv[j] = dav_recursigrate('dav_dn_dtdz_integrand', [[t[j], -5., -5.], [t[j+1], 5., 5.]], eps=1E-2)/(t[i+1] - t[i])
        dn_dtdz[i, j] = dn_dtdv[j]*dv[i]/dz
     endfor
     oplot, midTemp, dn_dtdv
  endfor
  return, dn_dtdz
end


