pro cc_ncc

output = 'cc_ncc.ps'
dat1 = 'dat/inner50_cash.dat'
dat2 = 'dat/culled_r2500-50_7-7.dat'
dat3 = 'dat/culled_r2500-50_2-7.dat'
;dat2 = 'dat/culled_r5000-50_7-7.dat'
;dat3 = 'dat/culled_r5000-50_2-7.dat'
dat4 = 'dat/counts_inner50.dat'
sig = 2.0

; restore the fit template and read some variables
restore,"/home/cavagnolo/research/redux/scripts/xspectemp_rin_normerr_src.sav"
i50 = read_ascii(dat1, template = xspectemp_rin_normerr_src)
r77 = read_ascii(dat2, template = xspectemp_rin_normerr_src)
r27 = read_ascii(dat3, template = xspectemp_rin_normerr_src)
readcol, dat4, FORMAT='A,F', inname, incts

obsids = r77.obsid
names = r77.cluster
zs = r77.z
FOR jj=0,n_elements(obsids)-1 DO BEGIN
    tempobs = obsids[jj]
    get = where(i50.obsid EQ tempobs)
    get = get[0]
    push, i50tx, i50.tx[get]
    push, i50lo, i50.tlo[get]
    push, i50hi, i50.thi[get]
    push, src, i50.src[get]
    push, ctr, i50.cr[get]
    get = where(r27.obsid EQ tempobs)
    get = get[0]
    push, r27tx, r27.tx[get]
    push, r27thi, r27.tlo[get]
    push, r27tlo, r27.thi[get]
    push, allz, zs[jj]
ENDFOR
FOR jj=0,n_elements(names)-1 DO BEGIN
    tnam = names[jj]
    get = where(inname EQ tnam)
    push, cts, incts
ENDFOR

; get rid of negative values
ord = where(i50hi EQ 0)
IF ord[0] NE -1 THEN i50hi[ord] = 79.9
ord = where(i50lo EQ 0)
IF ord[0] NE -1 THEN i50lo[ord] = 0.1

; set errs
r77lo = r77.tx - r77.tlo
r77hi = r77.thi - r77.tx
r27lo = r27tx - r27tlo
r27hi = r27thi - r27tx
i50lo = i50tx-i50lo
i50hi = i50hi-i50tx

; calc values
z     = r77.z
names = r77.cluster
x     = r77.tx
xhi   = r77hi
xlo   = r77lo

; calc Tdec
y   = i50tx/x
yhi = y*sqrt((i50hi/i50tx)^2.+(r77hi/x)^2.)
ylo = y*sqrt((i50lo/i50tx)^2.+(r77lo/x)^2.)

;calc Thfr
tf   = r27tx/r77.tx
tfhi = tf*(sqrt((r27hi/r27tx)^2.+(r77hi/r77.tx)^2.))
tflo = tf*(sqrt((r27lo/r27tx)^2.+(r77lo/r77.tx)^2.))

; for plotting
xtx = textoidl('T_{R2500-CORE} [keV]')
ytx = textoidl('T_{Inner50}/T_{R2500-CORE}')
xover = indgen(100)

; get rid of negative tdeclo
ord = where(y-ylo LT 0)
ylo[ord] = y[ord]

; print weird values to log file
ord = where(y GT 2.0)
openw, /get_lun, lun, 'tdecgt2.dat'
printf, lun, format='(A-20,A15,A10,A10,A10,A10,A10,A10,A10,A10,A10)',$
        "#Cluster","Obsid",$
        "Tx","Thi","Tlo",$
        "T50","T50hi","T50lo",$
        "Tdec","Tdechi","Tdeclo"
FOR jj=0,n_elements(ord)-1 DO BEGIN
tord = ord[jj]
printf, lun, format='(A-20,A15,F10.2,F10.2,F10.2,F10.2,F10.2,F10.2,F10.2,F10.2,F10.2)',$
        names[tord],obsids[tord],$
        r77.tx[tord],r77.tx[tord]+r77hi[tord],r77.tx[tord]-r77lo[tord],$
        i50tx[tord],i50tx[tord]+i50hi[tord],i50tx[tord]-i50lo[tord],$
        y[tord],yhi[tord]+y[tord],y[tord]-ylo[tord]
ENDFOR
close, lun

; isolate cc
cc = where(y+sig*yhi LT 1.)
zcc = z[cc]
xcc = x[cc]
xcclo = xlo[cc]
xcchi = xhi[cc]
ycc = y[cc]
ycclo = ylo[cc]
ycchi = yhi[cc]
ccsrc = src[cc]
ccctr = ctr[cc]
cccts = cts[cc]

; isolate ncc
ncc = where(y+sig*yhi GE 1.)
zncc = z[ncc]
xncc = x[ncc]
xncclo = xlo[ncc]
xncchi = xhi[ncc]
yncc = y[ncc]
yncclo = ylo[ncc]
yncchi = yhi[ncc]
nccsrc = src[ncc]
nccctr = ctr[ncc]
ncccts = cts[ncc]

; interesting #
print, 'Percent CC of total: ',(float(n_elements(cc))/float(n_elements(x)))*100.,'%'

; isolate tf > 1.1
thfr = where(tf-tflo GT 1.1)
zthfr = z[thfr]
xthfr = x[thfr]
xthfrlo = xlo[thfr]
xthfrhi = xhi[thfr]
ythfr = y[thfr]
ythfrlo = ylo[thfr]
ythfrhi = yhi[thfr]
thfrsrc = src[thfr]
thfrctr = ctr[thfr]
thfrcts = cts[thfr]

; isolate cc thfr
ccthfr = where(ythfr+sig*ythfrhi LT 1.)
zccthfr = zthfr[ccthfr]
xccthfr = xthfr[ccthfr]
xccthfrlo = xthfrlo[ccthfr]
xccthfrhi = xthfrhi[ccthfr]
yccthfr = ythfr[ccthfr]
yccthfrlo = ythfrlo[ccthfr]
yccthfrhi = ythfrhi[ccthfr]
ccthfrsrc = thfrsrc[ccthfr]
ccthfrctr = thfrctr[ccthfr]
ccthfrcts = thfrcts[ccthfr]

; isolate ncc thfr
nccthfr = where(ythfr+sig*ythfrhi GE 1.)
znccthfr = zthfr[nccthfr]
xnccthfr = xthfr[nccthfr]
xnccthfrlo = xthfrlo[nccthfr]
xnccthfrhi = xthfrhi[nccthfr]
ynccthfr = ythfr[nccthfr]
ynccthfrlo = ythfrlo[nccthfr]
ynccthfrhi = ythfrhi[nccthfr]
nccthfrsrc = thfrsrc[nccthfr]
nccthfrctr = thfrctr[nccthfr]
nccthfrcts = thfrcts[nccthfr]

; make a hardcopy
set_plot, 'PS'
device, filename=output
!fancy = 4
!p.font = 0
!linetype = 0
!p.font = 0
plotsym, 0, /fill
xrange = [0.8*min(x),1.2*max(x)]
yrange = [0.8*min(y),1.2*max(y)]
plot, xcc, ycc, $
      psym = 8, $
      symsize = 0.8, $
      xtitle = xtx, $
      ytitle = ytx, $
      /xsty, /ysty, $
      xrange = xrange, $
      yrange = yrange, $
      charsize = 1.0
;oploterror, xcc, ycc, xcchi, ycchi, psym=8, /hibar
;oploterror, xcc, ycc, xcclo, ycclo, psym=8, /lobar

plotsym, 0
oplot, xncc, yncc, psym=8
;oploterror, xncc, yncc, xncchi, yncchi, psym=8, /hibar
;oploterror, xncc, yncc, xncclo, yncclo, psym=8, /lobar

; overplot the line x=y
oplot, xover, replicate(1,n_elements(xover)), linestyle=2, psym=0

; make table entries
items = ['CC: '+num2str(n_elements(cc)),$
         'NCC: '+num2str(n_elements(ncc)),$
         'Total: '+num2str(n_elements(x)),$
         textoidl(num2str(sig,2)+'\sigma')]
linearr = replicate(-99,n_elements(items))
psyarr = replicate(-99,n_elements(items))
legend, items, linestyle=linearr, psym=psyarr, charsize=0.8, /top, box=0, /right_legend
device, /close

loadct, 39, /silent
set_plot, 'PS'
device, filename='t1.ps',/color
!fancy = 4
!p.font = 0
!linetype = 0
!p.font = 0
plotsym, 0, /fill
xrange = [min(src)-0.2*min(src),max(src)+0.1*max(src)]
yrange = [0,3]
plot, ccsrc, ycc, $
      psym = 8, $
      symsize = 0.8, $
      xtitle = textoidl('Source/(Source+Background)'),$
      ytitle = ytx, $
      /xsty, /ysty, /xlog,$
      xrange = xrange, $
      yrange = yrange, $
      charsize = 1.0
plotsym, 0
oplot, nccsrc, yncc, psym=8
plotsym, 3, /fill
oplot, ccthfrsrc, yccthfr, psym=8
plotsym, 3
oplot, nccthfrsrc, ynccthfr, psym=8
device,/close

set_plot, 'PS'
device, filename='t2.ps',/color
!fancy = 4
!p.font = 0
!linetype = 0
!p.font = 0
plotsym, 0, /fill
xrange = [0.0001,max(ctr)+0.1*max(ctr)]
yrange = [0,3]
plot, ccctr, ycc, $
      psym = 8, $
      symsize = 0.8, $
      xtitle = textoidl('Count Rate'),$
      ytitle = ytx, $
      /xsty, /ysty, /xlog,$
      xrange = xrange, $
      yrange = yrange, $
      charsize = 1.0
plotsym, 0
oplot, nccctr, yncc, psym=8
plotsym, 3, /fill
oplot, ccthfrctr, yccthfr, psym=8
plotsym, 3
oplot, nccthfrctr, ynccthfr, psym=8
device,/close

set_plot, 'PS'
device, filename='t3.ps',/color
!fancy = 4
!p.font = 0
!linetype = 0
!p.font = 0
plotsym, 0, /fill
xrange = [min(z)-0.05*min(z),max(z)+0.1*max(z)]
yrange = [0,3]
plot, zcc, ycc, $
      psym = 8, $
      symsize = 0.8, $
      xtitle = textoidl('Redshift'),$
      ytitle = ytx, $
      /xsty, /ysty, /xlog,$
      xrange = xrange, $
      yrange = yrange, $
      charsize = 1.0
plotsym, 0
oplot, zncc, yncc, psym=8
plotsym, 3, /fill
oplot, zccthfr, yccthfr, psym=8
plotsym, 3
oplot, znccthfr, ynccthfr, psym=8
device,/close

set_plot, 'PS'
device, filename='t4.ps',/color
!fancy = 4
!p.font = 0
!linetype = 0
!p.font = 0
plotsym, 0, /fill
xrange = [min(cts)-0.05*min(cts),max(cts)+0.1*max(cts)]
yrange = [0,3]
plot, cccts, ycc, $
      psym = 8, $
      symsize = 0.8, $
      xtitle = textoidl('Counts'),$
      ytitle = ytx, $
      /xsty, /ysty, /xlog,$
      xrange = xrange, $
      yrange = yrange, $
      charsize = 1.0
plotsym, 0
oplot, ncccts, yncc, psym=8
plotsym, 3, /fill
oplot, ccthfrcts, yccthfr, psym=8
plotsym, 3
oplot, nccthfrcts, ynccthfr, psym=8
device,/close

set_plot, 'PS'
device, filename='t5.ps',/color
!fancy = 4
!p.font = 0
!linetype = 0
!p.font = 0
plotsym, 0, /fill
xrange = [0.8*min(allz),1.2*max(allz)]
yrange = [0,3]
plot, allz, ycc, $
      psym = 8, $
      symsize = 0.8, $
      xtitle = textoidl('Redshift'),$
      ytitle = ytx, $
      /xsty, /ysty, /xlog,$
      xrange = xrange, $
      yrange = yrange, $
      charsize = 1.0
plotsym, 0
device,/close

; make all these ps files into one ps file
SPAWN, 'ls t?.ps > list'
SPAWN, 'cat list | perl /home/cavagnolo/research/redux/scripts/pscat.pl 1 cc_ncc_sys.ps'
SPAWN, 'rm -f t?.ps'
SPAWN, 'rm -f list'

END
