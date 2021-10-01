pro dav_ellipse_test, x=x, y=y, F=F, theta_ML=theta_ML, idx_i=idx_i, idx_j=idx_j
  if n_elements(F) gt 0 then begin
     F_inv = invert(F, /double)
     F_inv_minor = [[F_inv[2,2],F_inv[2,1]],[F_inv[1,2],F_inv[1,1]]]
     Q = invert(F_inv_minor, /double)
  end
  x = n_elements(x) eq 0 ? findgen(100)/99*(.5 - (-.5)) + (-.5) : x
  y = n_elements(y) eq 0 ? findgen(100)/99*(.5 - (-.5)) + (-.5) : y
  n = n_elements(y)
  m = n_elements(x)
  x_ML = n_elements(theta_ML) eq 0 ? 0 : theta_ML.(idx_i)
  y_ML = n_elements(theta_ML) eq 0 ? 0 : theta_ML.(idx_j)
  sig_x = 0.1
  sig_y = 0.1
  rho = 0.5
  C = [[sig_x*sig_x, rho*sig_x*sig_y], [rho*sig_y*sig_x, sig_y*sig_y]]
  C_inv = invert(C,/double)
  Q = n_elements(F) eq 0 ? C_inv : Q
  map = make_array(n,m,/double)
  for i = 0, m - 1 do begin
     for j = 0, n - 1 do begin
        r = transpose([x[i] - x_ML, y[j] - y_ML])
        map[i,j] = transpose(r) ## Q ## r
     endfor
  endfor
  contour,map,x,y,levels=[2.30,6.17,11.8],nlevels=10,xrange=[min(x),max(x)],xstyle=1,yrange=[min(y),max(y)],ystyle=1
end
