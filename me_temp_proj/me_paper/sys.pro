PRO sys

ploterr = 'no'
addex   = 'yes'
output  = 'sys.eps'
csize   = 1.3
psize   = 0.4
IF addex EQ 'yes' THEN BEGIN
    mrow = 2
    mcol = 3
ENDIF ELSE BEGIN
    mrow = 2
    mcol = 2
ENDELSE
; load the data files
restore,"/Users/cavagnolo/research/redux/scripts/xspectemp_rin_normerr_src.sav"
dat1 = '../me_fits/dat/culled_r2500-50_7-7.dat'
dat2 = '../me_fits/dat/culled_r2500-50_2-7.dat'
dat3 = '../me_fits/dat/culled_r5000-50_7-7.dat'
dat4 = '../me_fits/dat/culled_r5000-50_2-7.dat'
r2full = read_ascii(dat1, template = xspectemp_rin_normerr_src)
r2hard = read_ascii(dat2, template = xspectemp_rin_normerr_src)
r5full = read_ascii(dat3, template = xspectemp_rin_normerr_src)
r5hard = read_ascii(dat4, template = xspectemp_rin_normerr_src)

obs = r2full.obsid
nam = r2full.cluster
FOR ss=0,n_elements(obs)-1 DO BEGIN
    tobs = obs[ss]
    get = where(r5full.obsid EQ tobs)
    IF get NE -1 THEN BEGIN
        push, obsids, tobs
        push, z, r2full.z[ss]
        push, nh, r2full.nh[ss]
        push, names, r2full.cluster[ss]
        push, src, r2full.src[ss]
        push, r2src, r2full.src[ss]
        push, r2fulltx, r2full.tx[ss]
        push, r2fulllo, r2full.tlo[ss]
        push, r2fullhi, r2full.thi[ss]
        push, r5fulltx, r5full.tx[get]
        push, r5fulllo, r5full.tlo[get]
        push, r5fullhi, r5full.thi[get]
        get = where(r2hard.obsid EQ tobs)
        push, r2hardtx, r2hard.tx[get]
        push, r2hardlo, r2hard.tlo[get]
        push, r2hardhi, r2hard.thi[get]
        get = where(r5hard.obsid EQ tobs)
        push, r5hardtx, r5hard.tx[get]
        push, r5hardlo, r5hard.tlo[get]
        push, r5hardhi, r5hard.thi[get]
    ENDIF ELSE print, '## No ',tobs,' ',nam[ss]
ENDFOR

r2fulllo = r2fulltx - r2fulllo
r2fullhi = r2fullhi - r2fulltx
r2hardlo = r2hardtx - r2hardlo
r2hardhi = r2hardhi - r2hardtx
r2tf    = r2hardtx/r2fulltx
r2tfhi  = r2hardtx/r2fulltx*(sqrt((r2hardhi/r2hardtx)^2.+(r2fullhi/r2fulltx)^2.))
r2tflo  = r2hardtx/r2fulltx*(sqrt((r2hardlo/r2hardtx)^2.+(r2fulllo/r2fulltx)^2.))

r5fulllo = r5fulltx - r5fulllo
r5fullhi = r5fullhi - r5fulltx
r5hardlo = r5hardtx - r5hardlo
r5hardhi = r5hardhi - r5hardtx
r5tf    = r5hardtx/r5fulltx
r5tfhi  = r5hardtx/r5fulltx*(sqrt((r5hardhi/r5hardtx)^2.+(r5fullhi/r5fulltx)^2.))
r5tflo  = r5hardtx/r5fulltx*(sqrt((r5hardlo/r5hardtx)^2.+(r5fulllo/r5fulltx)^2.))

ord = where(r2tf LT 0.95)
tnam = names[ord]
tobs = obsids[ord]
trat = r2tf[ord]
trathi = r2tfhi[ord]
tratlo = r2tflo[ord]
for d=0,n_elements(trat)-1 do begin
    print, format='(A-20,I10,A10,F10.3,F10.3,F10.3)',$
           tnam[d],tobs[d],trat[d],trat[d]+trathi[d],trat[d]-tratlo[d]
ENDFOR

y   = r2tf
ylo = r2tflo
yhi = r2tfhi
plotsym, 0, psize, /fill
set_plot,'PS'
device, filename = output, $
        /encapsulated, $
        /portrait, $
        /helvetica
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 2
!Y.THICK  = 2
!Z.THICK  = 2
!P.MULTI  = [0,mrow,mcol,0]
x = z
plot, x, y, $
      /xsty, /ysty, $
      /xlog, $
      psym = 8, $
      xtitle = textoidl('Redshift'), $
      ytitle = textoidl('T_{HBR}'), $
      xrange = [0.9*min(x),1.1*max(x)], $
      yrange = [0.9*min(y),1.1*max(y)], $
      charsize = csize
IF ploterr EQ 'yes' THEN BEGIN
    oploterror, x, y, ylo, psym=8, /lobar, /nohat
    oploterror, x, y, yhi, psym=8, /hibar, /nohat
ENDIF
oplot, maken(0.0001,10000,n_elements(x)), replicate(1,n_elements(x)), linestyle=2
x = src
plot, x, y, $
      /xsty, /ysty, $
      psym = 8, $
      xtitle = textoidl('Source/(Source+Background)'), $
      ytitle = textoidl('T_{HBR}'), $
      xrange = [0.9*min(x),100.], $
      yrange = [0.9*min(y),1.1*max(y)], $
      charsize = csize
IF ploterr EQ 'yes' THEN BEGIN
    oploterror, x, y, ylo, psym=8, /lobar, /nohat
    oploterror, x, y, yhi, psym=8, /hibar, /nohat
ENDIF
oplot, maken(0.0001,10000,n_elements(x)), replicate(1,n_elements(x)), linestyle=2
x = nh
plot, x, y, $
      /xsty, /ysty, $
      /xlog, $
      psym = 8, $
      xtitle = textoidl('N_{H} [10^{20} cm^{-2}]'), $
      ytitle = textoidl('T_{HBR}'), $
      xrange = [0.9*min(x),1.1*max(x)], $
      yrange = [0.9*min(y),1.1*max(y)], $
      charsize = csize
IF ploterr EQ 'yes' THEN BEGIN
    oploterror, x, y, ylo, psym=8, /lobar, /nohat
    oploterror, x, y, yhi, psym=8, /hibar, /nohat
ENDIF
oplot, maken(0.0001,10000,n_elements(x)), replicate(1,n_elements(x)), linestyle=2
y   = r5tf/r2tf
yhi = r5tf/r2tf*(sqrt((r2tfhi/r2tf)^2.+(r5tfhi/r5tf)^2.))
ylo = r5tf/r2tf*(sqrt((r2tflo/r2tf)^2.+(r5tflo/r5tf)^2.))
plot, y, $
      /xsty, /ysty, $
      psym = 8, $
      xtitle = textoidl('Cluster'), $
      ytitle = textoidl('T_{HBR}(R_{5000-CORE})/T_{HBR}(R_{2500-CORE})'), $
      yrange = [0.9*min(y),1.1*max(y)], $
      charsize = csize
IF ploterr EQ 'yes' THEN BEGIN
    oploterror, y, ylo, psym=8, /lobar, /nohat
    oploterror, y, yhi, psym=8, /hibar, /nohat
ENDIF
oplot, maken(0.0001,10000,n_elements(x)), replicate(1,n_elements(x)), linestyle=2

;######################################
;######################################
;######################################
;# Add extra sys plots

IF addex EQ 'yes' THEN BEGIN

;# global vars
datadir = '/Volumes/GALACTUS/'
file1   = '../me_fits/dat/c2fits_culled_r2500-50_7-7.dat'
file2   = '../me_fits/dat/c2fits_culled_r2500-50_2-7.dat'

;# read files
restore,"/Users/cavagnolo/research/redux/scripts/xspectemp_rin_normerr_src.sav"        
full = read_ascii(file1, template = xspectemp_rin_normerr_src)
hard = read_ascii(file2, template = xspectemp_rin_normerr_src)

;; all the following steps assume the files are in the same order
tx77 = full.tx
tx27 = hard.tx
lo77 = full.tx - full.tlo
hi77 = full.thi - full.tx
lo27 = hard.tx - hard.tlo
hi27 = hard.thi - hard.tx
obsids = full.obsid
names  = full.cluster
src    = full.src

;# check for file existance
check = full.obsid/hard.obsid
uhoh = where(check NE 1)
IF uhoh NE -1 THEN BEGIN
    print,'Uh oh... out of order file'
    exit
ENDIF

;# fill data arrays
thfr   = tx27/tx77
thfrhi = (tx27/tx77)*(sqrt((hi27/tx27)^2.+(hi77/tx77)^2.))
thfrlo = (tx27/tx77)*(sqrt((lo27/tx27)^2.+(lo77/tx77)^2.))
y   = thfr
ylo = thfrlo
yhi = thfrhi

;# get values for particular clusters
FOR i=0,n_elements(obsids)-1 DO BEGIN
    check = 'yes'
    temp = names[i]
    ord = where(names EQ temp)
    IF n_elements(ord) GT 1 THEN GOTO, ORK
    IF src[i] LT 75. THEN GOTO, ORK
    obs = strcompress(obsids[i],/remove_all)
    file = datadir+'/'+obs+'/reprocessed/'+obs+'_exclude.fits'
    fitshead = headfits(file, ext=1)

    ;# format the date
    date = sxpar(fitshead,'DATE-OBS')
    date = split('T',date)
    date = split('-',date[0])
    date = julday(date[1],date[2],date[0],0,0,0)

    ;# distinguish between thfr=1 and thfr>1
    IF y[i]-ylo[i] GT 1.0 THEN BEGIN
        push, htime, date
        push, hgoodtx, y[i]
        push, hgoodtlo, ylo[i]
        push, hgoodthi, yhi[i]
    ENDIF ELSE BEGIN
        push, time, date
        push, goodtx, y[i]
        push, goodtlo, ylo[i]
        push, goodthi, yhi[i]
    ENDELSE
ORK:
ENDFOR

;# plotting calls
xmin = min([time,htime])-100.
xmax = max([time,htime])+100.
y = [goodtx,hgoodtx]
ylo = [goodtlo,hgoodtlo]
yhi = [goodthi,hgoodthi]
ymin = 0.9*min(y)
ymax = 1.1*max(y)
dummy = label_date(date_format='%N/%Z')
plot, [xmin,xmax], [ymin,ymax], $
      /nodata, $
      psym = 8, $
      charsize = csize, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      xtickformat = 'label_date', $
      xtickunits = 'TIME', $
      /xsty, /ysty,$
      xtitle = textoidl('Observation Date [Month/Year]'), $
      ytitle = textoidl('T_{HBR}')
oplot, time, goodtx, psym=8, thick=3
oplot, htime, hgoodtx, psym=8, thick=3
oplot, [xmin,xmax], replicate(1.0,n_elements(y)), linestyle=2

;# plot errors
IF ploterr EQ 'yes' THEN BEGIN
    oploterror, time, goodtx, goodtlo, psym=3, symsize=0.01, /lobar, /nohat
    oploterror, time, goodtx, goodthi, psym=3, symsize=0.01, /hibar, /nohat
    oploterror, htime, hgoodtx, hgoodtlo, psym=3, symsize=0.01, /lobar, /nohat
    oploterror, htime, hgoodtx, hgoodthi, psym=3, symsize=0.01, /hibar, /nohat
ENDIF

; input files array
file = '../me_fits/dat/culled_r2500_7-7.dat'
afile = '../me_fits/dat/asca.dat'

restore,"../me_fits/ascadat.sav"
asca = read_ascii(afile,template=ascadat)
restore,"~/research/redux/scripts/xspectemp_rin_normerr_src.sav"
chan = read_ascii(file, template=xspectemp_rin_normerr_src)

atx    = asca.tx
alo    = asca.tlo
ahi    = asca.thi
aerrlo = atx-alo
aerrhi = ahi-atx
anames = asca.cluster

rawnh  = chan.nh
ctx    = chan.tx
ctlo   = chan.tlo
cthi   = chan.thi
cerrlo = ctx-ctlo
cerrhi = cthi-ctx
cnames = chan.cluster

;; empty arrays
void, ascatx
void, ascalo
void, ascahi
void, chantx
void, chanlo
void, chanhi
void, names
void, plname
void, ctemps
void, atemps

FOR i=0,n_elements(anames)-1 DO BEGIN
    ord = where(cnames EQ anames[i])
    IF ord GT 0 THEN BEGIN
        push, ascatx, atx[i]
        push, ascalo, aerrlo[i]
        push, ascahi, aerrhi[i]
        push, chantx, ctx[ord]
        push, chanlo, cerrlo[ord]
        push, chanhi, cerrhi[ord]
        push, nh, rawnh[ord]
        push, names, anames[i]
    ENDIF
ENDFOR
print, n_elements(names)
diff = chantx/ascatx
difflo = diff*sqrt((ascalo/ascatx)^2.+(chanlo/chantx)^2.)
diffhi = diff*sqrt((ascahi/ascatx)^2.+(chanhi/chantx)^2.)

;; grab clusters which have variations > 2-sigma
FOR i=0,n_elements(difflo)-1 DO BEGIN
    IF ((diff[i]-2*difflo[i] GE 1.0) OR $
        (diff[i]+2*diffhi[i] LE 1.0)) THEN BEGIN
        push, plname, names[i]
        push, ctemps, chantx[i]
        push, atemps, ascatx[i]
    ENDIF ELSE BEGIN
        push, plname, " "
        push, ctemps, 0.0
        push, atemps, 0.0
    ENDELSE
ENDFOR

;; plotting input
x    = chantx
xlo  = chanlo
xhi  = chanhi
y    = diff
ylo  = difflo
yhi  = diffhi
xmin = min(x-xlo)-0.1*min(x-xlo)
xmax = max(x+xhi)+0.05*max(x+xhi)
ymin = min(y-ylo)-0.1*min(y-ylo)
ymax = max(y+yhi)+0.05*max(y+yhi)

;; plotting commands
xtex = textoidl('T_{Chandra} [keV]')
ytex = textoidl('T_{Chandra}/T_{ASCA}')
plotsym, 0, psize, /fill
plot, x, y, $
      psym = 8, $
      /xlog, /xsty, /ysty, $
      xran = [xmin,xmax], $
      yran = [ymin,ymax], $
      charsize = csize, $
      xtitle = xtex, $
      ytitle = ytex
oploterror, x, y, xlo, ylo, /lobar, psym=8
oploterror, x, y, xhi, yhi, /hibar, psym=8
xov = findgen(100)
oplot, xov, replicate(1.0,n_elements(xov)), linestyle=2

ENDIF

device, /close

END
