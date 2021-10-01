PRO calc_co

set_plot, 'PS'
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 2
!Y.THICK  = 2
!Z.THICK  = 2

;# IRAM info
tartime = 8.0                            ;# target tele time
nu    = [79.9d9, 159.9d9, 239.8d9]       ;, 319.7d9] ;# Hz
spin  = [1.0, 2.0, 3.0]                  ;, 4.0]     ;# J->J-1 transition state
theta = [30.8, 15.4, 10.3]               ;, 7.7      ;# arcsec
Tsys  = [143.7+16.0, 270.5+6.0, 488.1]   ;, 10760.6] ;# K
dnu   = [0.267d6, 0.533d6, 0.8d6]        ;, 1.066d6] ;# MHz/(km/s)
myptitle = [textoidl('CO (1-0)'), textoidl('CO (2-1)'), textoidl('CO (3-2)')] ;, textoidl('CO (4-3)')]

;# set constants
vres = 100.                      ;# velocity res of IRAM
vfwhm = 500.                     ;# FWHM of 250 km/s
etas = 0.87                      ;# backend eff
etat = 1.0                       ;# tele eff
z = 0.4418                       ;# source redshift
cosmology, z, result, /silent
dl = result[2]                  ;# dl in km
Jy = 1d-26                      ;# kg/s^2
c = 299792.458                  ;# c in km/s
kmMpc = 3.08568025d19           ;# Mpc in km
kmpc  = 3.08568025d13           ;# pc in km
k = 1.3806503d-29               ;# km^2 kg/s^2/K
KJy = 6.8*(1.0+z)^(-1./2.)      ;# K to Jy for IRAM 30m

;# plot the curves
device, filename= 'iram_mh2.eps', $
        /encapsulated, $
        /portrait, $
        set_font='Times-Roman'
xmin = 2.5
xmax = 10.0
ymin = -0.1
ymax = 1.5
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
;   Lco = (2.4d3/0.7^2.)*Sco*dl^2.*spin[j]^(-2.)*(1+z)^(-1.) ;# evans 1998
   Mh2 = 4.6*Lco
   ord = where((ttel GE 0.99*tartime*3600.) AND (ttel LE 1.01*tartime*3600.))
   ord = ord[0]
   print, '--------------'
   print, format='(A-20, F10.2)', 'Target time (hrs): ',tartime
   print, format='(A-20, F10.2)', 'v_res (km/s): ', vres
   print, format='(A-20, F10.2)', 'CO v_fwhm (km/s): ', vfwhm
   print, format='(A-20, F10.2)', 'Obs freq (GHz): ', nu[j]/1d9
   print, format='(A-20, F10.3)', 'Trms at TTime (mK): ', Trms[ord]*1000.
   print, format='(A-20, E10.2)', 'Mass at TTime (Msol): ', Mh2[ord]
   print, '--------------'
   Mh2 = alog10(Mh2)-10.
   oplot, ttel/3600., Ico, linestyle=0, thick=3
   oplot, ttel/3600., Trms*1000., linestyle=3, thick=3
   oplot, ttel/3600., Mh2, linestyle=1, thick=3
   IF j EQ 1 THEN BEGIN
      Sco = 6.4
      Lco = 3.25d7*Sco*(nu[j]/1d9)^(-2.)*dl^2.*(1.0+z)^(-3.) ;# Salome 2008 for MS0735
;      Lco = (2.4d3/0.7^2.)*Sco*dl^2.*spin[j]^(-2.)*(1+z)^(-1.)
      evm = 4.6*Lco
      evm = alog10(evm)-10.
      plotsym, 1, 2.0, thick=3
      oplot, [6,6.5], replicate(evm,2), linestyle=0, thick=3
      oplot, [-1d10,6.25], replicate(evm,2), linestyle=0, psym=8
   ENDIF
   IF j EQ 2 THEN BEGIN
      Sco = 8.2
      Lco = 3.25d7*Sco*(nu[j]/1d9)^(-2.)*dl^2.*(1.0+z)^(-3.) ;# Salome 2008 for MS0735
;      Lco = (2.4d3/0.7^2.)*Sco*dl^2.*spin[j]^(-2.)*(1+z)^(-1.)
      evm = 4.6*Lco
      evm = alog10(evm)-10.
      plotsym, 1, 2.0, thick=3
      oplot, [6,6.5], replicate(evm,2), linestyle=0, thick=3
      oplot, [-1d10,6.25], replicate(evm,2), linestyle=0, psym=8
   ENDIF

   ;# legend
   IF j EQ 0 THEN BEGIN
      items = [textoidl('I_{CO} [K km s^{-1}]'), textoidl('T_{rms} [mK]'), textoidl('(log M(H_2))-10 [M'+sunsymbol()+']')]
      legend, items, /fill, linestyle=[0,3,1], psym=[0,0,0], $
              box=0, charsize=0.7, thick=3, /top, /right
   ENDIF
   multiplot
ENDFOR
device, /close
END

;# evans 32

;# evans 21
;vres = 38

;theta = 15.4
;Trms = 4.6d-3
;vfwhm = 250.
;print, Mh2
;stop
;Mh2 = 1.18d4*Sco*dl^2.                     ;# Edge 2001
;Mh2 = 2.95d-19*Ico*theta[j]^2*dl^2.*2.3d20 ;# Salome 2003
