;+
; NAME:
;   dav_sigma_integrand
;
; PURPOSE:
;; sigma(M) is integrated over all wavenumbers k
;
; CALLING SEQUENCE:
;   dav_sigma_integrand, k
;
; INPUTS:
;   k
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
;; sigma(M) is integrated over all wavenumbers k
function dav_sigma_integrand, k
  common constants
  common global
  common sigma_integrand_dummy, r_m_dummy, theta_dummy
  r_m = r_m_dummy
  theta = theta_dummy
  return, dav_P(k, theta) * dav_W(k,r_m)^2 * k^2
end

;; Calculate sigma(M), within a normalization, and cache it
function dav_sigma, theta, params, M, z
  common constants
  common global
  common sigma_integrand_dummy
  theta_dummy = theta
  n = 100
  M_min = 1D12
  M_max = 1D16
  M_index = (alog10(M) - alog10(M_min))/(alog10(M_max) - alog10(M_min))*(n-1)
  if n_elements(sigma_tabulated) eq 0 then begin
     M_grid = M_min*(M_max/M_min)^(findgen(n)/(n-1))
     r_grid = dav_r_M(M_grid, theta)
     intval = dblarr(n)
     for i = 0, n - 1 do begin
        kmax = 100./r_grid[i]
        kmin = 1./100./r_grid[i]
        r_m_dummy = r_grid[i]
        intval[i] = qromb('dav_sigma_integrand', kmin, kmax)
     endfor
     sigma_tabulated = sqrt(intval)
     M_8 = 6.D14*theta.Omega_M_0/theta.h
     M_8_index = (alog10(M_8) - alog10(M_min))/(alog10(M_max) - alog10(M_min))*n
     sigma_tabulated = sigma_tabulated/interpolate(sigma_tabulated, M_8_index)
  endif
  return, theta.sigma_8*dav_d_growth(z, theta, params=params)*interpolate(sigma_tabulated, M_index)
end
