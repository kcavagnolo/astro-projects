PRO make_map
  readcol, 'specfits.dat', FORMAT='(F,F)', $
           bin, tx
  ninds = n_elements(tx)
  a = mrdfits('contbin_binmap.fits',0,hdr)
  dim = size(a,/dimensions)
  b = dblarr(dim[0],dim[1])
  FOR i = 0L, dim[0]-1L DO BEGIN
     FOR j = 0L, dim[1]-1L DO BEGIN
        IF ((a[i,j] GT ninds) OR (a[i,j] LT 0)) THEN $
           a[i,j] = -1.0 ELSE BEGIN
           ind = a[i,j]
           b[i,j] = tx[ind]
        ENDELSE
     ENDFOR
  ENDFOR
  mwrfits,b,'tmap.fits',hdr
END
