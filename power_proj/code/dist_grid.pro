; Return array with distance
; from the supplied origin
; Uses:  coord_grids
FUNCTION dist_grid, array, xcen, ycen
  coords = coord_grids(array, xcen, ycen)
  x = coords.x
  y = coords.y
  r = sqrt(x*x + y*y)
  return, r
END

PRO dist_grid_test
  a = make_array(5, 5, /float)
  print, dist_grid(a, 1, 1)
END
