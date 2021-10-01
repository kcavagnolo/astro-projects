PRO tx_time

;# global vars
ploterr = 'yes'
output  = 'tx_time.ps'
datadir = '/Volumes/GALACTUS/'
file1   = 'dat/c2fits_culled_r2500-50_7-7.dat'
file2   = 'dat/c2fits_culled_r2500-50_2-7.dat'

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
set_plot,'PS'
device, filename = 'tx_time.eps', $
        /encapsulated, $
        /portrait, $
        /helvetica
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 3
!Y.THICK  = 3
!Z.THICK  = 3
xmin = min([time,htime])-100.
xmax = max([time,htime])+100.
y = [goodtx,hgoodtx]
ylo = [goodtlo,hgoodtlo]
yhi = [goodthi,hgoodthi]
IF ploterr EQ 'yes' THEN BEGIN
    ymin = 0.9*min(y-ylo)
    ymax = 1.1*max(y+yhi)
ENDIF ELSE BEGIN
    ymin = 0.9*min(y)
    ymax = 1.1*max(y)
ENDELSE
dummy = label_date(date_format='%N/%Z')
plot, [xmin,xmax], [ymin,ymax], $
      /nodata, $
      psym = 8, $
      charsize = 0.8, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      xtickformat = 'label_date', $
      xtickunits = 'TIME', $
      /xsty, /ysty,$
      xtitle = textoidl('Observation Date [Month/Year]'), $
      ytitle = textoidl('T_{HFR} = T_{2.0-7.0}/T_{0.7-7.0}')

;# thfr ~1
plotsym, 0, 0.8
oplot, time, goodtx, psym=8, thick=3

;# thfr > 1
plotsym, 0, 0.8, /fill
oplot, htime, hgoodtx, psym=8, thick=3

;# y=1 line
oplot, [xmin,xmax], replicate(1.0,n_elements(y)), linestyle=2

;# plot errors
IF ploterr EQ 'yes' THEN BEGIN
    oploterror, time, goodtx, goodtlo, psym=3, symsize=0.01, /lobar, /nohat
    oploterror, time, goodtx, goodthi, psym=3, symsize=0.01, /hibar, /nohat
    oploterror, htime, hgoodtx, hgoodtlo, psym=3, symsize=0.01, /lobar, /nohat
    oploterror, htime, hgoodtx, hgoodthi, psym=3, symsize=0.01, /hibar, /nohat
ENDIF

device, /close
 
END
