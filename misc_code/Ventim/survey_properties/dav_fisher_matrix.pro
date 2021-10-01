;+
; NAME:
;   dav_fisher_matrix
;
; PURPOSE:
;; Calculate the fisher matrix at the ML point, for a params and observations
;
; CALLING SEQUENCE:
;   dav_fisher_matrix, log_likelihood, theta, params
;
; INPUTS:
;   log_likelihood = name of a log likelihood function 
;   params         = params that don't vary
;   theta          = params that do vary.  ML values
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
;; Calculate the fisher matrix at the ML point, for a params and observations
function dav_fisher_element, log_likelihood, theta, params, m, i, j
  h_i = abs(theta.(i)) gt 0 ? sqrt(sqrt(1D-16))*theta.(i) : sqrt(sqrt(1D-16))
  h_j = abs(theta.(j)) gt 0 ? sqrt(sqrt(1D-16))*theta.(j) : sqrt(sqrt(1D-16))
  temp = theta.(i) + h_i
  h_i = temp - theta.(i)
  temp = theta.(j) + h_j
  h_j = temp - theta.(j)
  h_i = double(h_i)
  h_j = double(h_j)
  theta1 = theta
  theta2 = theta
  theta3 = theta
  theta4 = theta
  theta1.(i) = theta1.(i) + h_i 
  theta1.(j) = theta1.(j) + h_j
  theta2.(i) = theta2.(i) + h_i 
  theta2.(j) = theta2.(j) - h_j 
  theta3.(i) = theta3.(i) - h_i 
  theta3.(j) = theta3.(j) + h_j 
  theta4.(i) = theta4.(i) - h_i 
  theta4.(j) = theta4.(j) - h_j 
  ll1 = call_function(log_likelihood, theta1, params, m)
  ll2 = call_function(log_likelihood, theta2, params, m)
  ll3 = call_function(log_likelihood, theta3, params, m)
  ll4 = call_function(log_likelihood, theta4, params, m)
  F_ij = 0.5*((ll1 - ll2) - (ll3 - ll4))/(4*h_i*h_j)
  if i eq j and F_ij lt 0 then begin
     F_ij = F_ij
  end
  return, F_ij
end

function dav_fisher_matrix, log_likelihood, theta, params, data=data, model=model, thawed=thawed
  date = systime()
  thawed_names=(tag_names(theta))[thawed]
  a=strsplit(strcompress(systime()),' ',/extract)
  b=strsplit(a[3],':',/extract)
  c=[a[0],a[1],a[2],b[0],b[1],b[2],a[4]]
  filename='F'
  for i = 0, n_elements(thawed_names) - 1 do begin
     filename = filename + '-' + string(thawed_names[i])
  endfor
  filename = strcompress(filename + '.dat', /remove_all)
  m = n_elements(data) eq 0 ? call_function(model, theta, params) : data
  N = n_elements(thawed) eq 0 ? n_tags(theta) : n_elements(thawed)
  FM = dblarr(N, N)
  index = n_elements(thawed) eq 0 ? indgen(N) : thawed
  thawed = n_elements(thawed) eq 0 ? index : thawed
  for i = 0, N - 1 do begin
     for j = 0, i do begin
        idx_i = index[i]
        idx_j = index[j]
        FM[i, j] = dav_fisher_element(log_likelihood, theta, params, m, idx_i, idx_j)
        FM[j, i] = FM[i, j]
     endfor
  endfor
  F = {user:getenv('USER'),$
       date:date, $
       ll:log_likelihood, $
       theta:theta, $
       params:params, $
       data:data, $
       thawed:thawed, $
       thawed_names:thawed_names, $
       FM:FM}
  save, F, filename=filename
  return, F
end
