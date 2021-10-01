;+
; NAME:
;   dav_log_likelihood1
;
; PURPOSE:
;   Calculate the log likelihood for:
;     1. gaussian likelihood
;     2. our cosmological model
;     3. using dav_dn (cluster number evolution with redshift)
;
; CALLING SEQUENCE:
;   dav_log_likelihood1, theta, params
;
; INPUTS:
;   params = parameters that don't change (here, they're the params properties)
;   theta  = the parameters that are varied in calculating partial
;            derivatives (note that these values themselves are the ML values)
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
;   dav_log_likelihood1 - this serves to cache the "observed data",
;                         which is calculated from the model using the
;                         ML values of the parameters theta, passed in
;
; SIDE EFFECTS:
;   none
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;   $Revision: 1.1.1.1 $
;-
function dav_log_likelihood1, theta, params, m
  params = params
  n = dav_dn(theta, params)
  s = sqrt(n)
  chi2 = total(((m-n)/s)^2)
  return, chi2
end
