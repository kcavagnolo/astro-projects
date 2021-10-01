FUNCTION moment_power, array, xcen, ycen, R_ap, m
  m = n_elements(m) ? m : 2.0
  aperture = aperture_grid(array, xcen, ycen, R_ap)   ;grid of 1s w/in R_ap, 0s elsewhere
  coords = coord_grids(array, xcen, ycen)             ;struct of 2 arrays, x and y, with coords. rel. to origin
  radius_grid = dist_grid(array, xcen, ycen)          ;grid of radial distances from origin
  angle_grid = atan(coords.y, coords.x)               ;grid of angles w.r.t. origin (in radians)
  cos_grid = cos(angle_grid*m)                        ;grid of cosines
  sin_grid = sin(angle_grid*m)                        ;grid of sines

  a_m = total(array*aperture*(radius_grid^m)*cos_grid)
  b_m = total(array*aperture*(radius_grid^m)*sin_grid)

  P_m = m EQ 0 ? (a_m*alog(R_ap))^2. : 1./2./m^2/R_ap^(2.*m)*(a_m^2. + b_m^2.)
  
  return, p_m
END

