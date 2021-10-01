PRO pevo

;# define some constants
t0 = 1.0                        ;# gas temp in keV
tnorm = 0.2
beta = 2./3.                    ;# canonical value from Jones & Forman 99
rc = 2.                         ;# core radius in kpc
gam = 4./3.                     ;# relativ ratio of spec heats
rho0 = 0.1                      ;# central gas density in cm^-3

;# setup initial atmosphere
r = maken(1,1000,10000)         ;# radii
tr = t0*(1.0-(tnorm/r))           ;# temperature
rho = rho0*(1.0+(r/rc)^2.)^(-3.*beta/2) ;# density
p = tr*rho*1.60217646d-9                ;# pressure in erg cm^-3

;# Device settings
set_plot, 'PS'
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 1
!Y.THICK  = 1
!Z.THICK  = 1
!P.MULTI  = [0,2,3,0]
device, filename='mock.eps', $
        /encapsulated, $
        /color, $
        /portrait, $
        set_font='Times-Roman', $
        bits=16
plotsym, 0, 0.3, /fill
xmin = 0.8*min(r)
xmax = 1.2*max(r)
ymin = 0.8*min(tr)
ymax = 1.2*max(tr)
plot, r, tr, $
      /xlog, $
      /xsty, /ysty, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      xtitle = 'Radius [kpc]', $
      ytitle = 'Temperature [keV]', $
      charsize = 1.0
xmin = 0.8*min(r)
xmax = 1.2*max(r)
ymin = 0.8*min(rho)
ymax = 1.2*max(rho)
plot, r, rho, $
      /xlog, /ylog, $
      /xsty, /ysty, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      xtitle = 'Radius [kpc]', $
      ytitle = textoidl('Density [cm^{-3}]'), $
      charsize = 1.0
xmin = 0.8*min(r)
xmax = 1.2*max(r)
ymin = 0.8*min(p)
ymax = 1.2*max(p)
plot, r, p, $
      /xlog, /ylog, $
      /xsty, /ysty, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      xtitle = 'Radius [kpc]', $
      ytitle = textoidl('Pressure [erg cm^{-3}]'), $
      charsize = 1.0

;# setup initial cavity
D0 = 10.                        ;# fiducial distance in kpc
r0 = 5.                         ;# fid. radius in kpc
D = maken(D0,100*D0,20)         ;# array of distances in kpc
conv = 3.08568025d21            ;# conversion from kpc to cm

;# fiducials
tr = t0*(1.0-(tnorm/D0))                 ;# inital temp in keV
rho = rho0*(1.0+(D0/rc)^2.)^(-3.*beta/2) ;# initial density in cm^-3
pD0 = tr*rho*1.60217646d-9               ;# initial pressure in erg cm^-3
VD0 = (4./3.)*!pi*(r0*conv)^3.           ;# initial volume in cm^3
pc0 = pD0*VD0                            ;# initial pV in erg

;# run of values
tr = t0*(1.0-(tnorm/D))                 ;# temperatures
rho = rho0*(1.0+(D/rc)^2.)^(-3.*beta/2) ;# densities
pD = tr*rho*1.60217646d-9               ;# initial pressure in erg cm^-3
rcav = (D*r0/D0)*(D/D0)^(-1.)*(pD/pD0)^(-1./(3.*gam)) ;# evolution of cavity size in kpc
VD = (4./3.)*!pi*(rcav*conv)^3.                       ;# volume in cm^3
pc = pD*VD                                            ;# run of pV

;# plot stuff
oplot, D, pD, psym=8
ttv = pc0/pD
ymin = 0.8*min(VD)
ymax = 1.2*max(ttv)
plotsym, 0, 0.3
plot, D, ttv, $
      /xlog, /ylog, $
      /xsty, /ysty, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      xtitle = 'Distance [kpc]', $
      ytitle = textoidl('Cavity Volume [cm^3]'), $
      psym = 8, $
      charsize = 1.0
plotsym, 0, 0.3, /fill
oplot, D, VD, psym=8

ymin = 0.8*min(pc)
ymax = 1.2*max(pc)
plot, D, pc, $
      /xlog, $
      /xsty, /ysty, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      xtitle = 'Distance [kpc]', $
      ytitle = textoidl('pV [erg]'), $
      psym = 8, $
      charsize = 1.0

y = pc/pc0
ymin = 0.8*min(y)
ymax = 1.2*max(y)
plot, D, y, $
      /xlog, $
      /xsty, /ysty, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      xtitle = 'Distance [kpc]', $
      ytitle = textoidl('pV/p_0V_0'), $
      psym = 8, $
      charsize = 1.0

device, /close

END
