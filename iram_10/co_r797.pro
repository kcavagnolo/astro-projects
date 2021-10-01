PRO co_r797

set_plot, 'PS'
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 2
!Y.THICK  = 2
!Z.THICK  = 2

;# IRAM info
z = 0.354                       ;# source redshift
nu = [115.2d9, 230.5d9, 345.7d9]
nu = nu/(1.0+z)
theta = [28.9, 14.5, 9.6]
Tsys = [127.2+16.0, 305.1+6.0, 370.4]
dnu = [0.284d6, 0.568d6, 0.852d6]
myptitle = [textoidl('CO (1-0)'), textoidl('CO (2-1)'), textoidl('CO (3-2)')]

;# set constants
vres = 50.                      ;# velocity res of IRAM
vfwhm = 300.                    ;# FWHM of 250 km/s
etas = 0.87                     ;# backend eff
etat = 1.0                      ;# tele eff
cosmology, z, result, /silent
dl = result[2]                  ;# dl in km
KJy = 6.8*(1.0+z)^(-1./2.)      ;# K to Jy for IRAM 30m

;# plot the curves
device, filename= 'r797_iram_mh2.eps', $
        /encapsulated, $
        /portrait, $
        set_font='Times-Roman'
xmin = 2.5
xmax = 10.0
ymin = -0.5
ymax = 1.0
multiplot,[3,1], /square, $
          mxtitle = textoidl('Telescope Time [hrs]')

;# for each freq
FOR j=0,n_elements(nu)-1 DO BEGIN
   plot, [xmin,xmax], [ymin,ymax], $
         /nodata, $
         /xsty, /ysty, $
         ticklen = 0.05, $
         title = myptitle[j], $
         xrange = [xmin,xmax], $
         yrange = [ymin,ymax], $
         charsize = 1.0

   ;# perform calc
   ttel = maken(xmin,xmax,100)*3600.
   Trms = sqrt((4.0*Tsys[j]^2.)/(etas^2.*etat*(vres*dnu[j])*ttel))
   Ico = (3.0*Trms*vfwhm)/sqrt(vfwhm/vres)
   Sco = Ico*KJy
   Lco = 3.25d7*Sco*(nu[j]/1d9)^(-2.)*dl^2.*(1.0+z)^(-3.) ;# Salome 2008 for MS0735
   Mh2 = 4.6*Lco
   Mh2 = alog10(Mh2)-10.
   oplot, ttel/3600., Ico, linestyle=0, thick=3
   oplot, ttel/3600., Trms*1000., linestyle=3, thick=3
   oplot, ttel/3600., Mh2, linestyle=1, thick=3

   ;# legend
   IF j EQ 0 THEN BEGIN
      items = [textoidl('I_{CO} [K km s^{-1}]'), textoidl('rms [mK]'), textoidl('(log M(H_2))-10 [M'+sunsymbol()+']')]
      legend, items, /fill, linestyle=[0,3,1], psym=[0,0,0], $
              box=0, charsize=0.7, thick=3, /bottom, /right
   ENDIF
   multiplot
ENDFOR
device, /close
END

;# evans 32
;Ico = 8.2/4.75
;theta = 10.3
;vres = 31
;Trms = 6.2d-3
;vfwhm = 250.
;# evans 21
;vres = 38
;Ico = 6.4/4.75
;theta = 15.4
;Trms = 4.6d-3
;vfwhm = 250.
;print, Mh2
;stop
