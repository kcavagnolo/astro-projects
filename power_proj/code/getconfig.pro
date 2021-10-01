;+
; NAME:
;   getconfig
;
; PURPOSE:
;   A generator of random changes in the configuration of the system
;   whereby "configuration" in this instance means a new set of coordinates
;   in a map of a simulated cluster projection, to use as the origin for
;   calculating a measure of substructure.
;
; CATEGORY:
;   Self Annealing, minimax, Numerical Recipes
;
; CALLING SEQUENCE:
;   getconfig(map, xcen, ycen, orig_x, orig_y, aperture, scale)
;
; INPUTS:
;   map       2 dimensional array
;   xcen      abscissa of old configuration
;   ycen      ordinate of old configuration
;   orig_x    abscissa of orig configuration
;   orig_x    ordinate of orig configuration
;   aperture  radius of allowable zone, in percentage of smaller dimension
;   scale     new config is displaced a normal variate distance away, given this scaling
;
; OUTPUTS:
;   returns a 2 element array of the new x coord and new y coord, [x, y]
;-
FUNCTION getconfig, map, xcen, ycen, orig_x, orig_y, aperture, scale
  ;get map dimensions and ascertain smaller dimension
  ncols = (size(map, /dimensions))[0]
  nrows = (size(map, /dimensions))[1]
  smaller_dim = ncols LE nrows ? ncols : nrows

  new_point = round(randomn(system_seed, 2, 1)*smaller_dim*aperture*scale)
  new_x = xcen + new_point[0, 0]
  new_y = ycen + new_point[1, 0]

  dist_from_orig = sqrt((new_x - orig_x)^2 + (new_y - orig_y)^2)
  IF $
    dist_from_orig GE aperture*smaller_dim/2.0 $ 
    OR new_x LT 0 $
    OR new_y LT 0 $
    OR new_x GE ncols $
    OR new_y GE nrows THEN BEGIN
    new_x = -1
    new_y = -1
  END

  RETURN, [new_x, new_y]
END

;Simple test harness whose side-effect is to produce a plot of random
;configuration steps
PRO test_getconfig, filename, size, scale, aperture
  filename = n_elements(filename) ? filename : 'config_dist.ps'
  size = n_elements(size) ? size : 51
  scale = n_elements(scale) ? scale : 0.14
  aperture = n_elements(aperture) ? aperture : 0.25
  map = fltarr(size, size)
  origin = [size/2, size/2]
  max_attempts = 100000L
  max_loops = 1000000L
  i = 0L
  j = 0L
  WHILE i LT max_attempts AND j LT max_loops DO BEGIN 
    new_origin = getconfig(map, origin[0], origin[1], origin[0], origin[1], aperture, scale)
    new_x = new_origin[0]
    new_y = new_origin[1]
    IF new_x GE 0 AND new_y GE 0 THEN BEGIN
      map[new_x, new_y] = map[new_x, new_y] + 1
      i++
    END
    j++
  END 

  ;Generate the plot
  page_width = 8.5
  page_height = 11.0
  row_gap = 0.5
  xsize = 6.5
  ysize = 10.0
  xoffset = (page_width - xsize)*0.5 + 0.5
  yoffset = (page_height - ysize)*0.5 + 1.5
  set_plot, 'ps'
  device, $
    file = filename, $
    /portrait, $
    xsize = xsize, $
    ysize = ysize, $
    xoffset = xoffset, $
    yoffset = yoffset, $
    /inches, $
    /color
  plot_width = 0.75
  plot_height = 0.75
  x0 = xoffset/page_width
  x1 = x0 + plot_width
  y0 = yoffset/page_height
  y1 = y0 + plot_height*float(!d.x_vsize)/float(!d.y_vsize)

  dims = size(map, /dimensions)
  ncol = dims[0]
  index = where(map GT 0)
  x = index MOD ncol
  y = index / ncol
  surface, map, /horizontal, position = [x0, y0, x1, y1], /normal
  title = string(scale, format = '("Scale:  ", f5.2)') + ", " + string(aperture, format = '("Aperture:  ", f5.2)')
  xyouts, .25, .65, title, /normal, font = 1, charsize = 2
  device, /close_file
END
