pro dav_make_window
  if !D.name eq "X" then begin
     window, 0, xsize=500*62./51., ysize=500, title='Plot Window'
  endif
end
