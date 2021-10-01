PRO plotmap

!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 2
!Y.THICK  = 2
!Z.THICK  = 2
plotsym, 0, psize, /fill
set_plot, 'PS'
device, filename='a.eps', $
        /encapsulated, $
        /portrait, $
        /color, $
        set_font='Times-Roman'
image = mrdfits('/mnt/DROBO/merged/HYDRA_A/tempmap/cbin_150/fe_map.fits',0)
femin = min(image)
femax = max(image)
print, 'Fe min max: ',femin, femax
di = n_elements(image[*,0])
dj = n_elements(image[0,*])
inc = [0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9]
colors = maken(0,250,n_elements(inc))
FOR i=0, di-1 DO BEGIN
   FOR j=0, dj-1 DO BEGIN
      fe = image[i,j]
      k = 0
      WHILE fe GT inc[k] DO k++
      image[i,j] = colors[k]
   ENDFOR
ENDFOR
loadct, 39
tvscl, image
COLORBAR, NCOLORS = 250, $
          POSITION = [0.15, 0.76, 0.85, 0.79], $
          range = [min(inc),max(inc)], $
          format = '(F5.1)', $
          /vertical, $
          /right, $
          charsize = 1.0, $
          divisions = n_elements(inc)
device,/close

END
