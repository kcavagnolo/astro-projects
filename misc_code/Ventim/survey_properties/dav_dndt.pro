;+
; NAME:
;   dav_dndt_integrand
;
; PURPOSE:
;; dndt integrand is dndm times Prob(m,l|t,z)
;
; CALLING SEQUENCE:
;   dav_dndt_integrand, r
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
;; dndt integrand is dndm times Prob(m,l|t,z)
function dav_dndt_integrand, r
  common constants
  common global
  common dndt_integrand_dummy, redshift, theta_dummy, params_dummy
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

;; Integrate over all masses, and OVER the temperature bin, and over luminosity
function dav_dndt, Temp_min, Temp_max, z, nbins, theta, params
  common constants
  common global
  common dndt_integrand_dummy
  theta_dummy = theta
  params_dummy = params
  redshift = z
  t1 = alog10(Temp_min/theta.Temp_naught)
  t2 = alog10(Temp_max/theta.Temp_naught)
  t = findgen(nbins+1)/(nbins)*(t2 - t1) + t1
  Temp = theta.Temp_naught*10^t
  n = nbins + 1
  midt = (t[0:n-2] + t[1:n-1])/2.
  midTemp = theta.Temp_naught*10^midt
  val = dblarr(nbins)
  for i = 0, nbins - 1 do begin
     val[i] = dav_recursigrate('dav_dndt_integrand', [[t[i], -5., -5.], [t[i+1], 5., 5.]], eps=1E-2)/(t[i+1] - t[i])
  endfor
  return, [[midTemp], [val]]
end

