PRO sys3

myhome = GETENV('HOME')
datadir = '/mnt/SINISTER'
ploterr = 'no'
output  = 'sys3.eps'
csize   = 1.3
psize   = 0.4
mcol = 2
mrow = 3

;# load the data files
ytex = textoidl('T_{HBR}')
restore,myhome+'/research/redux/scripts/xspectemp_rin_normerr_src.sav'
afile  = myhome+'/research/me_temp_proj/me_fits/dat/asca.dat'
;dat1  = myhome+'/research/me_temp_proj/me_fits/dat/culled_r2500-50_7-7.dat'
;dat2  = myhome+'/research/me_temp_proj/me_fits/dat/culled_r2500-50_2-7.dat'
;cfile = myhome+'/research/me_temp_proj/me_fits/dat/culled_r2500_7-7.dat'
;legtex = textoidl('R_{2500-CORE}')

dat1  = myhome+'/research/me_temp_proj/me_fits/dat/culled_r5000-50_7-7.dat'
dat2  = myhome+'/research/me_temp_proj/me_fits/dat/culled_r5000-50_2-7.dat'
cfile = myhome+'/research/me_temp_proj/me_fits/dat/culled_r5000_7-7.dat'
legtex = textoidl('R_{5000-CORE}')

;# read all the data into strucs
full = read_ascii(dat1, template = xspectemp_rin_normerr_src)
hard = read_ascii(dat2, template = xspectemp_rin_normerr_src)
fulltx = full.tx
fulllo = full.tx - full.tlo
fullhi = full.thi - full.tx
hardtx = hard.tx
hardlo = hard.tx - hard.tlo
hardhi = hard.thi - hard.tx
z = full.z
src = full.src
nh = full.nh
obs = full.obsid
name = full.cluster
tf    = hardtx/fulltx
tfhi  = hardtx/fulltx*(sqrt((hardhi/hardtx)^2.+(fullhi/fulltx)^2.))
tflo  = hardtx/fulltx*(sqrt((hardlo/hardtx)^2.+(fulllo/fulltx)^2.))

;# set up the plotting area
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
!P.MULTI  = [0,mcol,mrow,0]

temp = [z]
xmin = 0.9*min(temp)
xmax = 1.1*max(temp)
temp = [tf]
ymin = 0.9*min(temp)
ymax = 1.1*max(temp)
plotsym, 0, psize, /fill
plot, z, tf, $
      /xsty, /ysty, $
      /xlog, $
      psym = 8, $
      xtitle = textoidl('Redshift'), $
      ytitle = ytex, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      charsize = csize
IF ploterr EQ 'yes' THEN BEGIN
    oploterror, z, tf, tflo, psym=8, /lobar, /nohat
    oploterror, z, tf, tfhi, psym=8, /hibar, /nohat
ENDIF
oplot, maken(0.0001,10000,10), replicate(1,10), linestyle=2
items = [legtex]
linearr = replicate(-99,n_elements(items))
psyarr = replicate(-99,n_elements(items))
legend, items, linestyle=linearr, psym=psyarr, charsize=0.5*csize, $
        charthick=cthick, /top, box=0, /left_legend

temp = [src]
xmin = 0.9*min(temp)
xmax = 1.1*max(temp)
temp = [tf]
ymin = 0.9*min(temp)
ymax = 1.1*max(temp)
plotsym, 0, psize, /fill
plot, src, tf, $
      /xsty, /ysty, $
      psym = 8, $
      xtitle = textoidl('Source/(Source+Background)'), $
      ytitle = ytex, $
      xrange = [xmin,100.], $
      yrange = [ymin,ymax], $
      charsize = csize
IF ploterr EQ 'yes' THEN BEGIN
    oploterror, src, tf, tflo, psym=8, /lobar, /nohat
    oploterror, src, tf, tfhi, psym=8, /hibar, /nohat
ENDIF
oplot, maken(0.0001,10000,10), replicate(1,10), linestyle=2
items = [legtex]
linearr = replicate(-99,n_elements(items))
psyarr = replicate(-99,n_elements(items))
legend, items, linestyle=linearr, psym=psyarr, charsize=0.5*csize, $
        charthick=cthick, /top, box=0, /left_legend

temp = [nh]
xmin = 0.9*min(temp)
xmax = 1.1*max(temp)
temp = [tf]
ymin = 0.9*min(temp)
ymax = 1.1*max(temp)
plotsym, 0, psize, /fill
plot, nh, tf, $
      /xsty, /ysty, $
      /xlog, $
      psym = 8, $
      xtitle = textoidl('N_{H} [10^{20} cm^{-2}]'), $
      ytitle = ytex, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      charsize = csize
IF ploterr EQ 'yes' THEN BEGIN
    oploterror, nh, tf, tflo, psym=8, /lobar, /nohat
    oploterror, nh, tf, tfhi, psym=8, /hibar, /nohat
ENDIF
oplot, maken(0.0001,10000,10), replicate(1,10), linestyle=2
items = [legtex]
linearr = replicate(-99,n_elements(items))
psyarr = replicate(-99,n_elements(items))
legend, items, linestyle=linearr, psym=psyarr, charsize=0.5*csize, $
        charthick=cthick, /top, box=0, /left_legend

FOR i=0,n_elements(tflo)-1 DO BEGIN
   IF tflo[i] GT tfhi[i] THEN push, tferr, tflo[i] ELSE push, tferr, tfhi[i]
ENDFOR
temp = [abs(1.0-tf)/tferr]
ord = where(temp GT 0.0)
temp = temp[ord]
xmin = 0.9*min(temp)
xmax = 1.1*max(temp)
temp = [tf]
ymin = 0.9*min(temp)
ymax = 1.1*max(temp)
plotsym, 0, psize, /fill
plot, 1.6*abs(1.0-tf)/tferr, tf, $
      /xsty, /ysty, $
      /xlog, $
      psym = 8, $
      ytitle = ytex, $
      xtitle = textoidl('|1-T_{HBR}|/\sigma'), $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      charsize = csize
oplot, maken(0.0001,10000,10), replicate(1,10), linestyle=2
items = [legtex]
linearr = replicate(-99,n_elements(items))
psyarr = replicate(-99,n_elements(items))
legend, items, linestyle=linearr, psym=psyarr, charsize=0.5*csize, $
        charthick=cthick, /top, box=0, /left_legend

;# get values for particular clusters
dfile = myhome+'/research/redux/redux_info/me_ref.list'
readcol, dfile, FORMAT='A,A,F,F,F,F,F,F,F,F,F,A,F,A,F,A', comment='#', $
         aclusters, duma, xs, ys, rmaxs, minctss, zs, nhs,$
         txs, fes, lbols, chips, eobss, diffs, robss, locs
allnames = [name]
allobs = [obs]
allsrc = [src]
ally   = [tf]
allylo = [tflo]
allyhi = [tfhi]
FOR i=0,n_elements(allobs)-1 DO BEGIN
;    check = STRCMP(allobs[i], '_', /FOLD_CASE)  
    IF allsrc[i] LT 75. THEN GOTO, ORK
    temp = allnames[i]
    ord = where(aclusters EQ temp)
    IF n_elements(ord) GT 1 THEN GOTO, ORK
    tobs = strcompress(allobs[i],/remove_all)
    file = datadir+'/'+tobs+'/reprocessed/'+tobs+'_exclude.fits'
    fitshead = headfits(file, ext=1)

    ;# format the date
    date = sxpar(fitshead,'DATE-OBS')
    date = split('T',date)
    date = split('-',date[0])
    date = julday(date[1],date[2],date[0],0,0,0)

    ;# distinguish between thfr=1 and thfr>1
    IF ally[i]-allylo[i] GT 1.0 THEN BEGIN
        push, htime, date
        push, hgoodtx, ally[i]
        push, hgoodtlo, allylo[i]
        push, hgoodthi, allyhi[i]
    ENDIF ELSE BEGIN
        push, time, date
        push, goodtx, ally[i]
        push, goodtlo, allylo[i]
        push, goodthi, allyhi[i]
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
plotsym, 0, psize, /fill
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
      ytitle = ytex
oplot, time, goodtx, psym=8, thick=3
oplot, htime, hgoodtx, psym=8, thick=3
oplot, [xmin,xmax], replicate(1.0,n_elements(y)), linestyle=2
items = [legtex]
linearr = replicate(-99,n_elements(items))
psyarr = replicate(-99,n_elements(items))
legend, items, linestyle=linearr, psym=psyarr, charsize=0.5*csize, $
        charthick=cthick, /top, box=0, /left_legend

;# plot errors
IF ploterr EQ 'yes' THEN BEGIN
    oploterror, time, goodtx, goodtlo, psym=3, symsize=0.01, /lobar, /nohat
    oploterror, time, goodtx, goodthi, psym=3, symsize=0.01, /hibar, /nohat
    oploterror, htime, hgoodtx, hgoodtlo, psym=3, symsize=0.01, /lobar, /nohat
    oploterror, htime, hgoodtx, hgoodthi, psym=3, symsize=0.01, /hibar, /nohat
ENDIF

;# input files array
restore,myhome+'/research/me_temp_proj/me_fits/ascadat.sav'
asca = read_ascii(afile,template=ascadat)
restore,myhome+'/research/redux/scripts/xspectemp_rin_normerr_src.sav'
chan = read_ascii(cfile, template=xspectemp_rin_normerr_src)

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

;# empty arrays
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

;# grab clusters which have variations > 2-sigma
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

;# plotting input
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

;# plotting commands
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
items = [legtex]
linearr = replicate(-99,n_elements(items))
psyarr = replicate(-99,n_elements(items))
legend, items, linestyle=linearr, psym=psyarr, charsize=0.5*csize, $
        charthick=cthick, /top, box=0, /left_legend

;; obs = full.obsid
;; nam = full.cluster
;; void, fulltx
;; void, fulllo
;; void, fullhi
;; void, hardtx
;; void, hardlo
;; void, hardhi
;; void, r5fulltx
;; void, r5fullhi
;; void, r5fulllo
;; void, r5hardtx
;; void, r5hardlo
;; void, r5hardhi
;; FOR ss=0,n_elements(obs)-1 DO BEGIN
;;     tobs = obs[ss]
;;     get = where(r5full.obsid EQ tobs)
;;     IF get NE -1 THEN BEGIN
;;         push, obsids, tobs
;;         push, z, full.z[ss]
;;         push, nh, full.nh[ss]
;;         push, names, full.cluster[ss]
;;         push, src, full.src[ss]
;;         push, src, full.src[ss]
;;         push, fulltx, full.tx[ss]
;;         push, fulllo, full.tlo[ss]
;;         push, fullhi, full.thi[ss]
;;         push, r5fulltx, r5full.tx[get]
;;         push, r5fulllo, r5full.tlo[get]
;;         push, r5fullhi, r5full.thi[get]
;;         get = where(hard.obsid EQ tobs)
;;         push, hardtx, hard.tx[get]
;;         push, hardlo, hard.tlo[get]
;;         push, hardhi, hard.thi[get]
;;         get = where(r5hard.obsid EQ tobs)
;;         push, r5hardtx, r5hard.tx[get]
;;         push, r5hardlo, r5hard.tlo[get]
;;         push, r5hardhi, r5hard.thi[get]
;;     ENDIF ELSE print, '## No ',tobs,' ',nam[ss]
;; ENDFOR

;; fulllo = fulltx - fulllo
;; fullhi = fullhi - fulltx
;; hardlo = hardtx - hardlo
;; hardhi = hardhi - hardtx
;; tf    = hardtx/fulltx
;; tfhi  = hardtx/fulltx*(sqrt((hardhi/hardtx)^2.+(fullhi/fulltx)^2.))
;; tflo  = hardtx/fulltx*(sqrt((hardlo/hardtx)^2.+(fulllo/fulltx)^2.))

;; r5fulllo = r5fulltx - r5fulllo
;; r5fullhi = r5fullhi - r5fulltx
;; r5hardlo = r5hardtx - r5hardlo
;; r5hardhi = r5hardhi - r5hardtx
;; r5tf    = r5hardtx/r5fulltx
;; r5tfhi  = r5hardtx/r5fulltx*(sqrt((r5hardhi/r5hardtx)^2.+(r5fullhi/r5fulltx)^2.))
;; r5tflo  = r5hardtx/r5fulltx*(sqrt((r5hardlo/r5hardtx)^2.+(r5fulllo/r5fulltx)^2.))

;; y   = r5tf/tf
;; yhi = r5tf/tf*(sqrt((tfhi/tf)^2.+(r5tfhi/r5tf)^2.))
;; ylo = r5tf/tf*(sqrt((tflo/tf)^2.+(r5tflo/r5tf)^2.))
;; plotsym, 0, psize, /fill
;; plot, y, $
;;       /xsty, /ysty, $
;;       psym = 8, $
;;       xtitle = textoidl('Cluster'), $
;;       ytitle = textoidl('T_{HBR}(R_{5000-CORE})/T_{HBR}(R_{2500-CORE})'), $
;;       yrange = [0.9*min(y),1.1*max(y)], $
;;       charsize = csize
;; IF ploterr EQ 'yes' THEN BEGIN
;;     oploterror, y, ylo, psym=8, /lobar, /nohat
;;     oploterror, y, yhi, psym=8, /hibar, /nohat
;; ENDIF
;; oplot, maken(0.0001,10000,10), replicate(1,10), linestyle=2

device, /close

END
