;+
; NAME:
;   dav_dn_m_d_ln_inv_sigma
;
; PURPOSE:
;; Jenkins mass function
;
; CALLING SEQUENCE:
;   dav_dn_m_d_ln_inv_sigma, M, z, theta
;
; INPUTS:
;   M, z, theta
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
;; Jenkins mass function
function dav_dn_m_d_ln_inv_sigma, M, z, theta, params=params
  common constants
  common global
  A_J = 0.27 - 0.07*(1. - theta.Omega_M_0)
  B_J = 0.65 + 0.11*(1. - theta.Omega_M_0)
  eps_J = 3.80
  sigma_val = dav_sigma(theta, params, M, z)
  return, A_J*theta.Omega_M_0*dav_rho_c(theta)/M*exp(-(abs(alog(1./sigma_val) + B_J))^eps_J)
end

