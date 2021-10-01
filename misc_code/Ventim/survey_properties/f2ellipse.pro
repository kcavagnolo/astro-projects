function f2ellipse, F, i, j, x, y
  erase
  dav_make_window
  thawed = F.thawed
  theta = F.theta
  x_ml = theta.(thawed[i])
  y_ml = theta.(thawed[j])
  xtitle = (tag_names(theta))[thawed[i]]
  ytitle = (tag_names(theta))[thawed[j]]
  title = F.user + ' ' + F.date + ' ' + F.LL
  FM = F.FM
  FM_inv = invert(FM, /double)
  FM_inv_minor = [[FM_inv[i,i], FM_inv[i,j]], [FM_inv[j,i], FM_inv[j,j]]]
  Q = invert(FM_inv_minor, /double)
  n = n_elements(y)
  m = n_elements(x)
  dchi2 = make_array(n,m, /double)
  for i = 0, m - 1 do begin
     for j = 0, m - 1 do begin
        r = transpose([x[i] - x_ml, y[j] - y_ml])
        dchi2[i,j] = transpose(r) ## Q ## r
     endfor
  endfor
  contour,dchi2,x,y,levels=[2.30,6.17,11.8],xrange=[min(x),max(x)],xstyle=1,yrange=[min(y),max(y)],ystyle=1,c_annotation=[textoidl('1-\sigma'),textoidl('2-\sigma'),textoidl('3-\sigma')],c_charsize=1,title=title,xtitle=xtitle,ytitle=ytitle,charsize=1.5
  return,dchi2
end
