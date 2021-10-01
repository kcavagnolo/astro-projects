pro bgd_fig

; set the char size
myhome = GETENV('HOME')
in1 = myhome+'/research/redux/redux_info/all_bgd_9-12.dat'
;in2 = myhome+'/research/me_temp_proj/me_fits/dat/c2fits_final_r5000-50_fefree_7-7.dat'
in2 = 'junk'
output = 'bgd_fig.eps'
thick = 3
csize = 0.9
xmin = 0.7
xmax = 1.8
ymin = 0.
ymax = 50.

; restore the fit template and read some variables
restore,myhome+'/research/redux/scripts/bgd_ratio.sav'
dat1 = read_ascii(in1, template = bgd_ratio)
restore,myhome+'/research/redux/scripts/xspectemp_rin_normerr_src.sav'
dat2 = read_ascii(in2, template = xspectemp_rin_normerr_src)

obsids = dat2.obsid
FOR jj=0,n_elements(obsids)-1 DO BEGIN
    tobs = obsids[jj]
    get = where(dat1.obsid EQ tobs)
    get = get[0]
    push, ratio, dat1.ratio[get]
ENDFOR

print, 'Total clusters: ',n_elements(ratio)

; define the labels for the plot axes
ytx = textoidl("Clusters per bin")
xtx = textoidl("9.5-12 keV count rate ratio for Target/Blank-Sky")

; make a hardcopy
odevice = !d.name
set_plot, 'PS'
device, filename = output, $
  /encapsulated, $
  /portrait, $
  /helvetica
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = thick
!Y.THICK  = thick
!Z.THICK  = thick

plothist, ratio, $
;  peak = 1, $
  bin=0.05, $
  xtitle = xtx, $
  ytitle = ytx, $
  /xsty, $
  /ysty, $
  xran = [xmin,xmax], $
  yran = [ymin,ymax], $
  thick = thick, $
  charsize = csize

; overplot the lines y=0.8, 1, 1.2
y  = findgen(n_elements(dat1.ratio))
x2 = replicate(1.2,n_elements(y))
x3 = replicate(0.8,n_elements(y))
oplot, x2, y, linestyle=2, psym=0, thick=thick
oplot, x3, y, linestyle=2, psym=0, thick=thick

device, /close
set_plot, odevice

END
