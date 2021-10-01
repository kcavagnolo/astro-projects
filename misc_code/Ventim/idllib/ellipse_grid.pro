FUNCTION ellipse_grid, array, xcen, ycen
  coords = coord_grids(array, xcen, ycen)
  x = coords.x
  y = coords.y
  r = sqrt(x*x + y*y)
  return, r
END
