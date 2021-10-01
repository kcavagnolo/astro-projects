; Return struct with two arrays x y
; each with the coords from the origin
FUNCTION coord_grids, array, xcen, ycen
  n = (size(array))[1]
  z = findgen(n)
  dist_x = z - xcen
  dist_y = z - ycen
  dist_y = dist_y
  x = fltarr(n, n, /nozero)     ;x coords
  y = fltarr(n, n, /nozero)     ;y coords
  FOR i = 0, n - 1 DO BEGIN     ;populate coord arrays
    x(0, i) = dist_x
    y(0, i) = dist_y
  ENDFOR 
  y = transpose(y)
  return, {x:x, y:y}
END

function coord_grids_test
  a = make_array(5, 5, /float)
  print, 'x coords'
  print, (coord_grids(a, 2, 2)).x
  print, 'y coords'
  print, (coord_grids(a, 2, 2)).y
  return, a
END
