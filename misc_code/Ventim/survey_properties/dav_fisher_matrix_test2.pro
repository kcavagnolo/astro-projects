function dav_log_likelihood_test, theta, params, m
  n = (dav_dn(theta, params))[*,2]
  oplot,n,linestyle=1
  s = sqrt(m)
  chi2 = total(((m-n)/s)^2)
  return, chi2
end

function dav_log_likelihood_wrapper_test, x, y, args
  params=args.params
  theta=args.theta
  m=args.m
  i=args.i
  j=args.j
  theta.(i) = x
  theta.(j) = y
  return,dav_log_likelihood_test(theta, params, m)
end

function dav_fisher_matrix_test2, F
;;   restore
  erase
  dav_make_window
  dav_constants
  params = dav_params()
  theta_ML = dav_lcdm()
;;   m = (dav_dn(theta_ML, params))[*,2]
;;   Omega_M_0=findgen(30)/29*(0.34 - 0.18) + 0.18
  Omega_M_0=findgen(5)/4*(0.6 - 0.1) + 0.18
  w=findgen(5)/4*(-0.6 - (-1.0)) + (-1.0)
  b=w
  a=Omega_M_0
  idx_i = 2
  idx_j = 1

  ;; make ellipses using Fisher matrix
  dchi2=dav_error_ellipse(x=a,y=b,F=F,theta_ML=theta_ML,idx_i=0,idx_j=1,theta_idx_i=1,theta_idx_j=2)
  contour,dchi2,a,b,levels=[2.30,6.17,11.8],xrange=[min(a),max(a)],xstyle=1,yrange=[min(b),max(b)],ystyle=1,xtitle=strlowcase(strtrim((tag_names(theta_ML))[idx_i])),ytitle=strlowcase(strtrim((tag_names(theta_ML))[idx_j])),c_annotation=[textoidl('1-\sigma'),textoidl('2-\sigma'),textoidl('3-\sigma')],c_charsize=1,title='Fisher Matrix'

  ;; make likelihood contours by computing on a parameter grid
;;   chi2=dav_map("dav_log_likelihood_wrapper_test",a,b,{params:params,theta:theta_ML,m:m,i:idx_i,j:idx_j})
;;   contour,chi2,a,b,levels=min(chi2)+[2.30,6.17,11.8],xminor=10,yminor=10,xticks=6,xtitle=strlowcase(strtrim((tag_names(theta_ML))[idx_i])),ytitle=strlowcase(strtrim((tag_names(theta_ML))[idx_j])),xrange=[min(a),max(a)],xstyle=1,yrange=[min(b),max(b)],ystyle=1,c_annotation=[textoidl('1-\sigma'),textoidl('2-\sigma'),textoidl('3-\sigma')],c_charsize=1,title='Sparse Grid Sampling'
  return, chi2
end
