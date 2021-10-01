;+
; NAME:
;   dav_error_ellipse
;
; PURPOSE:
;   plot confidence ellipses based on a Fisher matrix
;
; CALLING SEQUENCE:
;   dav_error_ellipse, x=x, y=y, F=F, theta_ML=theta_ML, idx_i=idx_i, idx_j=idx_j
;
; INPUTS:
;   x=x, y=y, F=F, theta_ML=theta_ML, idx_i=idx_i, idx_j=idx_j
;
; OPTIONAL INPUTS:
;   x        vector of x values (eg., Omega_M_0)
;   y        vector of y values (eg., w)
;   F        Fisher information matrix
;   theta_ML ML parameter values
;   idx_i    index in theta_ML of the x parameter
;   idx_j    index in theta_ML of the y parameter
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
;    dchi2 = dav_ellipse_test,x=Omega_M_0,y=w,F=F,theta_ML=lcdm,idx_i=2,idx_j=1
;
; MODIFICATION HISTORY:
;   @Revision@
;-
function dav_error_ellipse, x=x, y=y, F=F, theta_ML=theta_ML, idx_i=idx_i, idx_j=idx_j, theta_idx_i=theta_idx_i, theta_idx_j=theta_idx_j
  if n_elements(F) gt 0 then begin
     F_inv = invert(F, /double)
     F_inv_minor = [[F_inv[idx_i,idx_i],F_inv[idx_i,idx_j]],[F_inv[idx_j,idx_i],F_inv[idx_j,idx_j]]]
     Q = invert(F_inv_minor, /double)
  end
  x = n_elements(x) eq 0 ? findgen(100)/99*(.5 - (-.5)) + (-.5) : x
  y = n_elements(y) eq 0 ? findgen(100)/99*(.5 - (-.5)) + (-.5) : y
  n = n_elements(y)
  m = n_elements(x)
  x_ML = n_elements(theta_ML) eq 0 ? 0 : theta_ML.(theta_idx_i)
  y_ML = n_elements(theta_ML) eq 0 ? 0 : theta_ML.(theta_idx_j)
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
  return,map
end

pro dav_error_ellipse_test
  restore
  dav_constants
  lcdm=dav_lcdm()
  params=dav_params()
  w=findgen(30)/29*((-0.75) - (-1.02)) + (-1.02)
  Omega_M_0=findgen(30)/29*(0.34 - 0.18) + 0.18
;;   chi2=dav_error_ellipse(f, lcdm, Omega_M_0, w, 2, 1)
  dchi2=dav_error_ellipse(x=Omega_M_0,y=w,F=F,theta_ML=lcdm,idx_i=2,idx_j=1)
  contour,map,x,y,levels=[2.30,6.17,11.8],nlevels=10,xrange=[min(x),max(x)],xstyle=1,yrange=[min(y),max(y)],ystyle=1,xtitle=strlowcase(strtrim((tag_names(lcdm))[2])),ytitle=strlowcase(strtrim((tag_names(lcdm))[1]))
end

