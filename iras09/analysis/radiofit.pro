PRO radiofit

;# plot stuff
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 3
!Y.THICK  = 3
!Z.THICK  = 3
csize = 1.4

;# some constants
z = 0.4418                      ;# object redshift
;datfile = '../data/radiospec_all.dat'
;vc = 12.9d9                     ;# full source
;alpha = 1.10                    ;# full source
;Vrad = 2d69                     ;# radio source volume

datfile = '../data/nucleus.dat'
vc = 10^(10.4)                  ;# nucleus only
alpha = 1.03                    ;# nucleus only
Vrad = 3.35d64                  ;# radio source volume

;datfile = '../data/lobe.dat'
;vc = 1.30d9                     ;# lobes only
;alpha = 1.65                    ;# lobes only
;Vrad = 1d69                     ;# radio source volume

;datfile = '../data/birzan.dat'
;vc = 1010d6
;alpha = 1.08
;Vrad = 1d69                     ;# radio source volume

;# constants
phi = 1.0                       ;# max volume filling fraction for 
nu1 = 0.01d9                    ;# min frequency
nu2 = 100d9                     ;# max frequency
c12 = c12(alpha, nu1, nu2)      ;# Pacholcyzk c12 constant
k = maken(1,1000,10000)         ;# range of k values
Jy = 1.0d-23                    ;# 1Jy = 10**-23 ergs sec**-1 cm**-2 Hz**-1
cmMpc = 3.08568025d24           ;# 1 Mpc = 3.08x10**24 cm

;# read data
readcol,datfile,format='D,D,D',freq,flux,err,comment='#'
readcol,datfile+'.KP',format='D,F',kfreq,kflux,comment='#'
readcol,datfile+'.JP',format='D,F',jfreq,jflux,comment='#'
readcol,datfile+'.CI',format='D,F',cfreq,cflux,comment='#'

;# from microJy to Jy
flux = flux*1d-6
err = err*1d-6
kflux = kflux*1d-6
jflux = jflux*1d-6
cflux = cflux*1d-6

;# calc bolo lumin
ord = where((jfreq GE nu1) AND (jfreq LE nu2))
x = jfreq[ord]
f = jflux[ord]*Jy
intx = maken(nu1,nu2,10000)
intf = interpol(f, x, intx)
frad = int_tabulated(intx,intf)
cosmology, z, result, /silent
dl = result[2]
lrad = 4 * !PI * (dl * cmMpc)^2 * frad
print, ''
print, format='(A-30, E10.4, A6)','Bolometric luminosity:',lrad, 'erg/s'

;# calc Beq
beq = ((6.0*c12*!PI*lrad*(1+k))/(Vrad*phi))^(2./7.)
hbeq = ((6.0*c12*!PI*lrad*(1+k))/(Vrad*(0.5*phi)))^(2./7.)
tbeq = ((6.0*c12*!PI*lrad*(1+k))/(Vrad*(0.1*phi)))^(2./7.)
dum = [beq,hbeq,tbeq]
print, format='(A-30, F10.4, A6)','Max min. energy B-field:',max(dum)/1d-6, 'uG'
print, format='(A-30, F10.4, A6)','Min min. energy B-field:',min(dum)/1d-6, 'uG'

;# calc tsyn
bm = 3.25*(1+z)^2.
beq = beq/1d-6
hbeq = hbeq/1d-6
tbeq = tbeq/1d-6
tsyn = (1590.*sqrt(beq))/((beq^2.+bm^2.)*(sqrt((1+z)*vc/1d9)))
htsyn = (1590.*sqrt(hbeq))/((hbeq^2.+bm^2.)*(sqrt((1+z)*vc/1d9)))
ttsyn = (1590.*sqrt(tbeq))/((tbeq^2.+bm^2.)*(sqrt((1+z)*vc/1d9)))
dum = [tsyn,htsyn,ttsyn]
print, format='(A-30, F10.4, A6)','Max synchrotron age:',max(dum), 'Myr'
print, format='(A-30, F10.4, A6)','Min synchrotron age:',min(dum), 'Myr'
print, ''

;# plot ages vs. k
xmin = min(k)
xmax = max(k)
ymin = 0.9*min(dum)
ymax = 1.1*max(dum)
ymin = 0.
ymax = 0.5
set_plot, 'PS'
device, filename='tsyn_k.eps', $
        /encapsulated, $
        /portrait, $
        set_font='Times-Roman'
plot, k, tsyn, $
      /nodata, $
      /xlog, $
      /xsty, /ysty, $
      xtitle=textoidl('k'), $
;      ytitle=textoidl('Whole source t_{sync} [Myr]'), $
;      ytitle=textoidl('Lobes only t_{sync} [Myr]'), $
      ytitle=textoidl('t_{sync} [Myr]'), $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      POSITION = ASPECT(1.0), $
      charsize = csize
oplot, k, tsyn, linestyle=0, thick=2
oplot, k, htsyn, linestyle=2, thick=2
oplot, k, ttsyn, linestyle=3, thick=2
psyarr = replicate(0,3)
items = [textoidl('\Phi = 1.0'), $
         textoidl('\Phi = 0.5'), $         
         textoidl('\Phi = 0.1')]         
legend, items, linestyle=[0,2,3], box=0, thick=2, charsize=1.0, /top, /right
oplot, [xmin,xmax], [7d4/1d6,7d4/1d6], linestyle=1, thick=4
device, /close

;# plot device
nuscale = 1d9
fscale = 1000
xmin = 0.1*min([freq,kfreq,jfreq,cfreq])
xmax = 10.0*max([freq,kfreq,jfreq,cfreq])
ymin = 0.1*min([flux,kflux,jflux,cflux])
ymax = 10.0*max([flux,kflux,jflux,cflux])
xmin = 1.
xmax = 20.
ymin = 0.1
ymax = 7.
;xticks = LOGLEVELS([xmin,xmax])
;xnticks = N_Elements(xticks)
;yticks = LOGLEVELS([ymin,ymax])
;ynticks = N_Elements(yticks)
set_plot, 'PS'
device, filename='radiofit.eps', $
        /encapsulated, $
        /portrait, $
        set_font='Times-Roman'
plotsym, 0, 0.8, /fill
plot, freq, flux, $
      /nodata, $
;      /xlog, /ylog, $
      /xsty, /ysty, $
      xtitle='Frequency [GHz]', $
;      ytitle='Whole Source Flux [Jy]', $
;      ytitle='Lobes only Flux [mJy]', $
      ytitle='Nuclear Flux [mJy]', $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
;      ytickformat='logticks_exp', $
;      yticks = ynticks-1, $
;      ytickv = yticks, $
;      xticks = xnticks-1, $
;      xtickv = xticks, $
      psym = 8, $
      POSITION = ASPECT(1.0), $
      charsize = csize
oplot, kfreq/nuscale, kflux*fscale, linestyle=1, thick=2
oplot, jfreq/nuscale, jflux*fscale, linestyle=2, thick=2
oplot, cfreq/nuscale, cflux*fscale, linestyle=3, thick=2
oploterror, freq/nuscale, flux*fscale, err*fscale, psym=8
oplot, freq/nuscale, flux*fscale, psym=8

;# check the low freq slope
;vu = maken(xmin,xmax,1000)
;dum = 4000.0*vu^(alpha)
;oplot, vu, dum
;oplot, replicate(vc,10)/nuscale, maken(1d-10,1d10,10), linestyle=1
;oplot, replicate(9.9d8,10)/nuscale, maken(1d-10,1d10,10), linestyle=2

;# 1-sig 14.9 ul to plot
;x = [14.9d9/nuscale, 1d-100]
;y = [0.84/1000/3, 1d-100]
;plotsym, 3, 1.0, /fill
;oplot, x, y, psym=8
;plotsym, 1, 2.0, thick=2
;oplot, x, y, psym=8

;# add 74 MHz
;x = [74d6/nuscale, 1d-100]
;y = [196.26/1000, 1d-100]
;yerr = [130.06/1000., 1d-100]
;plotsym, 3, 1.0, /fill
;oploterror, x, y, yerr, psym=8
;oplot, x, y, psym=8

;# 1-sig lobe 8.4 ul to plot
;x = [8.4d9/nuscale, 1d-100]
;y = [0.4/1000, 1d-100]
;plotsym, 0, 0.8, /fill
;oplot, x, y, psym=8
;plotsym, 1, 2.0, thick=2
;oplot, x, y, psym=8

;# add legend
psyarr = replicate(0,3)
legend, ['KP','JP','CI'], linestyle=[1,2,3], box=0, thick=2, charsize=csize, /top, /right
device, /close

END
