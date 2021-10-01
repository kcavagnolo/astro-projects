PRO radiofit

;# plot stuff
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 3
!Y.THICK  = 3
!Z.THICK  = 3

;# some constants
;datfile = '../data/radiospec_all.dat'
datfile = '../data/nuc_sync.dat'
Jy = 1.0d-23                    ;# 1Jy = 10**-23 ergs sec**-1 cm**-2 Hz**-1
cmMpc = 3.08568025d24           ;# 1 Mpc = 3.08x10**24 cm
z = 0.354                       ;# object redshift
vc = 9.59034d13                 ;# full source
alpha = 1.08                    ;# full source
nu1 = 10d6                      ;# min frequency
nu2 = 10000d6                   ;# max frequency
c12 = c12(alpha, nu1, nu2)      ;# Pacholcyzk c12 constant
Vrad = 1.98d69                  ;# radio source volume
phi = 1.0                       ;# max volume filling fraction for 
k = maken(0.1,1000,1000)        ;# range of k values

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
ymin = 0.9*min([tsyn,htsyn,ttsyn])
ymax = 1.1*max([tsyn,htsyn,ttsyn])

xmin = 1.0
ymin = 0.
ymax = 12.0

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
      ytitle=textoidl('t_{sync} [Myr]'), $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      POSITION = ASPECT(1.0), $
      charsize = 1.0
oplot, k, tsyn, linestyle=1, thick=2
oplot, k, htsyn, linestyle=2, thick=2
oplot, k, ttsyn, linestyle=3, thick=2
psyarr = replicate(0,3)
items = [textoidl('\Phi = 0.1'), $
         textoidl('\Phi = 0.5'), $         
         textoidl('\Phi = 1.0')]         
legend, items, linestyle=[3,2,1], box=0, thick=2, charsize=1.0, number=1, /top, /right
;legend, items, linestyle=[3,2,1], box=0, thick=2, charsize=1.0, number=1, /bottom, /left
device, /close

;# plot device
xmin = 0.1*min([freq,kfreq,jfreq,cfreq])
xmax = 10.0*max([freq,kfreq,jfreq,cfreq])
ymin = 0.1*min([flux,kflux,jflux,cflux])
ymax = 10.0*max([flux,kflux,jflux,cflux])
xmin = 1d2
xmax = 1d13
ymin = 1d-9
ymax = 1d-1
xticks = LOGLEVELS([xmin,xmax])
xnticks = N_Elements(xticks)
yticks = LOGLEVELS([ymin,ymax])
ynticks = N_Elements(yticks)
set_plot, 'PS'
device, filename='radiofit.eps', $
        /encapsulated, $
        /portrait, $
        set_font='Times-Roman'
plotsym, 0, 0.8, /fill
plot, freq/1d6, flux, $
      /nodata, $
      /xlog, /ylog, $
      /xsty, /ysty, $
      xtitle='Frequency [MHz]', $
      ytitle='Flux [Jy]', $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      ytickformat='logticks_exp', $
      yticks = ynticks-1, $
      ytickv = yticks, $
      xticks = xnticks-1, $
      xtickv = xticks, $
      psym = 8, $
      POSITION = ASPECT(1.0), $
      charsize = 1.0
;oplot, kfreq/1d6, kflux, linestyle=1, thick=2
;oplot, jfreq/1d6, jflux, linestyle=2, thick=2
oplot, cfreq/1d6, cflux, linestyle=3, thick=2
oploterror, freq/1d6, flux, err, psym=8
oplot, freq/1d6, flux, psym=8

;oplot, [1.96e+15/1d6, 1.32e+15/1d6], [19.5e-6, 6.50e-6], psym=2
plotsym, 4, 0.8, /fill
oplot, [1.96e+15/1d6, 1.32e+15/1d6], [19.2e-6, 5.9e-6], psym=8

;# add legend
psyarr = replicate(0,3)
;legend, ['KP','JP','CI'], linestyle=[1,2,3], box=0, thick=2, charsize=1.0, number=1, /top, /right
legend, ['CI'], linestyle=[3], box=0, thick=2, charsize=1.0, number=1, /top, /right
device, /close

END
