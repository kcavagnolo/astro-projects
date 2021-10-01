PRO sys2

myhome = GETENV('HOME')
datadir = '/mnt/SINISTER'
ploterr = 'no'
output  = 'sys2.eps'
csize   = 1.3
psize   = 0.4
mrow = 2
mcol = 4

;# load the data files
restore,myhome+'/research/redux/scripts/xspectemp_rin_normerr_src.sav'
dat1 = myhome+'/research/me_temp_proj/me_fits/dat/culled_r2500-50_7-7.dat'
dat2 = myhome+'/research/me_temp_proj/me_fits/dat/culled_r2500-50_2-7.dat'
dat3 = myhome+'/research/me_temp_proj/me_fits/dat/culled_r5000-50_7-7.dat'
dat4 = myhome+'/research/me_temp_proj/me_fits/dat/culled_r5000-50_2-7.dat'
r2full = read_ascii(dat1, template = xspectemp_rin_normerr_src)
r2hard = read_ascii(dat2, template = xspectemp_rin_normerr_src)
r5full = read_ascii(dat3, template = xspectemp_rin_normerr_src)
r5hard = read_ascii(dat4, template = xspectemp_rin_normerr_src)

r2fulltx = r2full.tx
r2fulllo = r2full.tx - r2full.tlo
r2fullhi = r2full.thi - r2full.tx
r2hardtx = r2hard.tx
r2hardlo = r2hard.tx - r2hard.tlo
r2hardhi = r2hard.thi - r2hard.tx
r2z = r2full.z
r2src = r2full.src
r2nh = r2full.nh
r2obs = r2full.obsid
r2name = r2full.cluster
r2tf    = r2hardtx/r2fulltx
r2tfhi  = r2hardtx/r2fulltx*(sqrt((r2hardhi/r2hardtx)^2.+(r2fullhi/r2fulltx)^2.))
r2tflo  = r2hardtx/r2fulltx*(sqrt((r2hardlo/r2hardtx)^2.+(r2fulllo/r2fulltx)^2.))

obs1 = r2full.obsid
obs2 = r5full.obsid
FOR i=0,n_elements(obs2)-1 DO BEGIN
    tobs = obs2[i]
    ord = where(obs1 EQ tobs)
    IF ord EQ -1 THEN BEGIN
        push, r5fulltx, r5full.tx[i]
        push, r5fulllo, r5full.tx[i] - r5full.tlo[i]
        push, r5fullhi, r5full.thi[i] - r5full.tx[i]
        push, r5hardtx, r5hard.tx[i]
        push, r5hardlo, r5hard.tx[i] - r5hard.tlo[i]
        push, r5hardhi, r5hard.thi[i] - r5hard.tx[i]
        push, r5z, r5full.z[i]
        push, r5src, r5full.src[i]
        push, r5nh, r5full.nh[i]
        push, r5name, r5full.cluster[i]
        push, r5obs, tobs
    ENDIF
ENDFOR
r5tf = r5hardtx/r5fulltx
r5tfhi = r5hardtx/r5fulltx*(sqrt((r5hardhi/r5hardtx)^2.+(r5fullhi/r5fulltx)^2.))
r5tflo = r5hardtx/r5fulltx*(sqrt((r5hardlo/r5hardtx)^2.+(r5fulllo/r5fulltx)^2.))

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
!P.MULTI  = [0,mrow,mcol,0]

temp = [r2z,r5z]
xmin = 0.9*min(temp)
xmax = 1.1*max(temp)
temp = [r2tf,r5tf]
ymin = 0.9*min(temp)
ymax = 1.1*max(temp)
plotsym, 0, psize, /fill
plot, r2z, r2tf, $
      /xsty, /ysty, $
      /xlog, $
      psym = 8, $
      xtitle = textoidl('Redshift'), $
      ytitle = textoidl('T_{HBR}'), $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      charsize = csize
plotsym, 0, psize
oplot, r5z, r5tf, psym=8
IF ploterr EQ 'yes' THEN BEGIN
    oploterror, r2z, r2tf, r2tflo, psym=8, /lobar, /nohat
    oploterror, r2z, r2tf, r2tfhi, psym=8, /hibar, /nohat
    oploterror, r5z, r5tf, r5tflo, psym=8, /lobar, /nohat
    oploterror, r5z, r5tf, r5tfhi, psym=8, /hibar, /nohat
ENDIF
oplot, maken(0.0001,10000,10), replicate(1,10), linestyle=2

temp = [r2src,r5src]
xmin = 0.9*min(temp)
xmax = 1.1*max(temp)
temp = [r2tf,r5tf]
ymin = 0.9*min(temp)
ymax = 1.1*max(temp)
plotsym, 0, psize, /fill
plot, r2src, r2tf, $
      /xsty, /ysty, $
      psym = 8, $
      xtitle = textoidl('Source/(Source+Background)'), $
      ytitle = textoidl('T_{HBR}'), $
      xrange = [xmin,100.], $
      yrange = [ymin,ymax], $
      charsize = csize
plotsym, 0, psize
oplot, r5src, r5tf, psym=8
IF ploterr EQ 'yes' THEN BEGIN
    oploterror, r2src, r2tf, r2tflo, psym=8, /lobar, /nohat
    oploterror, r2src, r2tf, r2tfhi, psym=8, /hibar, /nohat
    oploterror, r5src, r5tf, r5tflo, psym=8, /lobar, /nohat
    oploterror, r5src, r5tf, r5tfhi, psym=8, /hibar, /nohat
ENDIF
oplot, maken(0.0001,10000,10), replicate(1,10), linestyle=2

temp = [r2nh,r5nh]
xmin = 0.9*min(temp)
xmax = 1.1*max(temp)
temp = [r2tf,r5tf]
ymin = 0.9*min(temp)
ymax = 1.1*max(temp)
plotsym, 0, psize, /fill
plot, r2nh, r2tf, $
      /xsty, /ysty, $
      /xlog, $
      psym = 8, $
      xtitle = textoidl('N_{H} [10^{20} cm^{-2}]'), $
      ytitle = textoidl('T_{HBR}'), $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      charsize = csize
plotsym, 0, psize
oplot, r5nh, r5tf, psym=8
IF ploterr EQ 'yes' THEN BEGIN
    oploterror, r2nh, r2tf, r2tflo, psym=8, /lobar, /nohat
    oploterror, r2nh, r2tf, r2tfhi, psym=8, /hibar, /nohat
    oploterror, r5nh, r5tf, r5tflo, psym=8, /lobar, /nohat
    oploterror, r5nh, r5tf, r5tfhi, psym=8, /hibar, /nohat
ENDIF
oplot, maken(0.0001,10000,10), replicate(1,10), linestyle=2

FOR i=0,n_elements(r2tflo)-1 DO BEGIN
   IF r2tflo[i] GT r2tfhi[i] THEN push, r2tferr, r2tflo[i] ELSE push, r2tferr, r2tfhi[i]
ENDFOR
FOR i=0,n_elements(r5tflo)-1 DO BEGIN
   IF r5tflo[i] GT r5tfhi[i] THEN push, r5tferr, r5tflo[i] ELSE push, r5tferr, r5tfhi[i]
ENDFOR

temp = [abs(1.0-r2tf)/r2tferr,abs(1.0-r5tf)/r5tferr]
xmin = 0.9*min(temp)
xmax = 1.1*max(temp)
temp = [r2tf,r5tf]
ymin = 0.9*min(temp)
ymax = 1.1*max(temp)
plotsym, 0, psize, /fill
plot, abs(1.0-r2tf)/r2tferr, r2tf, $
      /xsty, /ysty, $
      psym = 8, $
      ytitle = textoidl('T_{HBR}'), $
      xtitle = textoidl('T_{HBR}/\sigma'), $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      charsize = csize
plotsym, 0, psize
oplot, abs(1.0-r5tf)/r5tferr, r5tf, psym=8
oplot, maken(0.0001,10000,10), replicate(1,10), linestyle=2

;# get values for particular clusters
dfile = myhome+'/research/redux/redux_info/me_ref.list'
readcol, dfile, FORMAT='A,A,F,F,F,F,F,F,F,F,F,A,F,A,F,A', comment='#', $
         aclusters, duma, xs, ys, rmaxs, minctss, zs, nhs,$
         txs, fes, lbols, chips, eobss, diffs, robss, locs
allnames = [r2name,r5name]
allobs = [r2obs,r5obs]
allsrc = [r2src,r5src]
ally   = [r2tf,r5tf]
allylo = [r2tflo,r5tflo]
allyhi = [r2tfhi,r5tfhi]
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

;# input files array
file = myhome+'/research/me_temp_proj/me_fits/dat/culled_r2500_7-7.dat'
afile = myhome+'/research/me_temp_proj/me_fits/dat/asca.dat'

restore,myhome+'/research/me_temp_proj/me_fits/ascadat.sav'
asca = read_ascii(afile,template=ascadat)
restore,myhome+'/research/redux/scripts/xspectemp_rin_normerr_src.sav'
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

obs = r2full.obsid
nam = r2full.cluster
void, r2fulltx
void, r2fulllo
void, r2fullhi
void, r2hardtx
void, r2hardlo
void, r2hardhi
void, r5fulltx
void, r5fullhi
void, r5fulllo
void, r5hardtx
void, r5hardlo
void, r5hardhi
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

y   = r5tf/r2tf
yhi = r5tf/r2tf*(sqrt((r2tfhi/r2tf)^2.+(r5tfhi/r5tf)^2.))
ylo = r5tf/r2tf*(sqrt((r2tflo/r2tf)^2.+(r5tflo/r5tf)^2.))
plotsym, 0, psize, /fill
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
oplot, maken(0.0001,10000,10), replicate(1,10), linestyle=2

device, /close

END
