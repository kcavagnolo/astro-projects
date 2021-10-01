FUNCTION getcoords, map, index
  ncols = (size(map, /dimensions))[0]
  nrows = (size(map, /dimensions))[1]
  x = index MOD ncols
  y = index / ncols
  return, [x, y]
END

