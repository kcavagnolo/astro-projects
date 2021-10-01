function mg, gamma
  dav_constants
  theta = mg_lcdm()
  params = mg_params()
  F = fltarr(2,2)
  N = n_elements(gamma)
  for i = 0, N-1 do begin
     theta.gamma = gamma[i]
     m = dav_dn(theta, params)
     F = [[[F]],[[dav_fisher_matrix('dav_log_likelihood1', theta, params, data=m, thawed=[1,2])]]]
  endfor
  return, F[*,*,1:*]
end
