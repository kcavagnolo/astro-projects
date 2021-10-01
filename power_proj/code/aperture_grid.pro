; Return grid mask with 1's for the mask
; and 0's everywhere else
FUNCTION aperture_grid, array, xcen, ycen, radius
  return, dist_grid(array, xcen, ycen) LE radius
END

PRO aperture_grid_test
  print, aperture_grid(make_array(15, 15, /float), 7, 7, 6)
END
