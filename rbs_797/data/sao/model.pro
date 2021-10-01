PRO model

;# Column 1: wavelength in Angstroms 
;# Column 2: log wavelength (in Angstroms) 
;# Column 3: Flux in wavelength units
;# Column 4: log of column 3

;# stuff
angstrom = STRING(197B)
xrange = [1000, 10000]
yrange = [1d-2, 2d3]

;# load files
push, files, 'galmod_12_100.dat'
push, files, 'galmod_6_100.dat'
push, files, 'galmod_3_100.dat'
push, files, 'galmod_1.5_100.dat'

;# plot stuff
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 3
!Y.THICK  = 3
!Z.THICK  = 3
set_plot,'PS'
xtex = 'Emitted Wavelength ['+angstrom+']'
ytex = textoidl('Relative F_{\lambda}')
device, filename='specmodel.eps', $
        /encapsulated, $
        /portrait, $
        /isolatin1
plot, xrange, yrange, $
      /nodata, $
      /ylog, $
      /xsty, /ysty, $
      xrange = xrange, $
      yrange = yrange, $
      xtitle = xtex, $
      ytitle = ytex, $
      linestyle = 0, $
      charsize = 1.0
;lint = 0
FOR i=0,n_elements(files)-1 DO BEGIN
   readcol, files[i], FORMAT='F,F,F,F', comment='#', $
            lam, loglam, flux, logflux
   oplot, lam, flux;, linestyle=lint
;   lint++
ENDFOR
device, /close

END
