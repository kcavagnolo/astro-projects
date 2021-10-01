pro alltx, reffile

;# options
goal = 15
doind = 'no'
ktype = 'flat'            ;# type of K to use: 'flat' or 'itpl'
model = 'nonzero'         ;# type of model to use: 'zero' or 'nonzero'

radfile = 'dat/radio.dat'

;# read files
tempfitfile = 'dat/pf_temp_profs.dat'
restore,"~/research/redux/scripts/reflist_template.sav"
ref = read_ascii(reffile, template = reflist_template)
restore,'~/research/redux/scripts/xspectemp_rin_normerr_src.sav'        
fitfile = read_ascii(tempfitfile,template=xspectemp_rin_normerr_src)
restore,'~/research/redux/scripts/s_resultstemplate.sav'
readcol, radfile, format='A,A,A,F,F,F,F,F,F,F,A', comment='#', $
         rname, robs, rtype, rz, rflux, rferr, rpower, rperr, rlum, rlerr, rsurv

;# get values specific to model
IF (ktype EQ 'flat') THEN BEGIN
   IF (model EQ 'nonzero') THEN ind = 2 ELSE ind = 3
ENDIF
IF (ktype EQ 'itpl') THEN BEGIN
   IF (model EQ 'nonzero') THEN ind = 0 ELSE ind = 1
ENDIF

;# set graphical info
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 3
!Y.THICK  = 3
!Z.THICK  = 3

;# open log files    
GET_LUN, LOGLUN                 ; Get logical unit number LUN
OPENW, LOGLUN, "err_alltx.log"
GET_LUN, TXLUN
OPENW, TXLUN, 'txlist'
GET_LUN, FELUN
OPENW, FELUN, 'felist'

;# get global vars
obsids = ref.obsid
zs = ref.z
txs = ref.tx
names = ref.cluster

;# start plot loop
FOR i = 0,n_elements(obsids)-1 DO BEGIN
    multi = 'no'
    obsid = strcompress(obsids[i],/remove_all)
    name = strcompress(names[i],/remove_all)
    z = zs[i]
    tx = txs[i]
    cosmology, z, result, /silent
    rv = (rdelta(180,z,tx,/silent))*1000.
    as2kpc = result[4]
    ord = where(names EQ name)
    IF n_elements(ord) EQ 1 THEN obsid = obsid ELSE BEGIN
        temp = obsids[ord]
        FOR j=1,n_elements(temp)-1 DO BEGIN
            obsid = obsid+'+'+strcompress(temp[j],/remove_all)
        ENDFOR
        multi = 'yes'
    ENDELSE

    ;# check for file existance
    ord  = where(fitfile.obsid EQ obsid)
    notx = 'no'
    IF ord[0] EQ -1 THEN BEGIN
        notx = 'yes'
        GOTO, err
    ENDIF
    file2 = '~/research/pf_clusters/pf_fits/s_results/'+obsid+'_results.log'
    check = findfile(file2,count=count2)
    IF (count2 NE 1) THEN GOTO,err
    datafit  = read_ascii(file2, template = s_resultstemplate)
    k0 = datafit.k0[ind]
    k0err = datafit.k0err[ind]
    push, allk0, k0
    push, allk0err, k0err

    ;# get values
    rin  = fitfile.rin[where(fitfile.obsid EQ obsid)]
    rout = fitfile.rout[where(fitfile.obsid EQ obsid)]
    r    = ((rin+rout)/60.)*result[4]*1000.
    tx   = fitfile.tx[where(fitfile.obsid EQ obsid)]
    txhi = fitfile.thi[where(fitfile.obsid EQ obsid)]
    txlo = fitfile.tlo[where(fitfile.obsid EQ obsid)]
    tlo  = tx-txlo
    thi  = txhi-tx
    fe   = fitfile.fe[where(fitfile.obsid EQ obsid)]
    felo = fitfile.felo[where(fitfile.obsid EQ obsid)]
    fehi = fitfile.fehi[where(fitfile.obsid EQ obsid)]
    felo = fe-felo
    fehi = fehi-fe

    ;# save all values for stacked plotting
    push, alltx, ptr_new(tx)
    push, alltlo, ptr_new(tlo)
    push, allthi, ptr_new(thi)
;    push, allr, ptr_new(r/rv)
    push, allr, ptr_new(r)
    push, allfe, ptr_new(fe)
;    push, allfe0, 0.5*(fe[0]+fe[1])
    push, allfe0, fe[0]
    push, allfe0lo, felo[0]
    push, allfe0hi, fehi[0]
    push, allfelo, ptr_new(felo)
    push, allfehi, ptr_new(fehi)
    IF k0 LE 30. THEN push, allcolor, 50
    IF ((k0 GT 30.) AND (k0 LE 50.)) THEN push, allcolor, 150
    IF k0 GT 50. THEN push, allcolor, 250
;    IF tx[0] LE 4. THEN push, allcolor, 50
;    IF ((tx[0] LE 8.) AND (tx[0] GT 4.)) THEN push, allcolor, 150
;    IF tx[0] GT 8. THEN push, allcolor, 250
    push, allfe0, fe[0]
    push, allfe0lo, felo[0]
    push, allfe0hi, fehi[0]
    ;# get radio info
    ord = where(rname EQ name, num)
    IF (num GE 1) THEN BEGIN
       ord = ord[0]
       push, l14, rpower[ord]
       push, l14err, rperr[ord]
       push, l14fe, fe[0]
       push, l14felo, felo[0]
       push, l14fehi, fehi[0]
    ENDIF

    IF doind EQ 'yes' THEN BEGIN

        ;# plot the results
        set_plot, 'PS'
        printf, TXLUN, obsid+'_txprof.ps'
        device, filename = obsid+'_txprof.ps', $
                /portrait, $
                /times
        xran = [0.9*min(r),1.1*max(r)]
        yran = [0.9*min(tx-tlo),1.1*max(tx+thi)]
        plot, xran, yran, $
              /nodata, $
              xran = xran, $
              yran = yran, $
              /xlog, $
              /xsty, /ysty, $
              charsize = 0.8, $
              xtitle = textoidl('R_{mid} [kpc]'), $
              ytitle = textoidl('Temperature [keV]'), $
              title  = name+'   '+obsid
        plotsym, 0, 0.8, /fill
        oplot, r, tx, psym=0, thick=2
        oplot, r, tx, psym=8, thick=1
        oploterror, r, tx, tlo, psym=8, thick=2, /lobar
        oploterror, r, tx, thi, psym=8, thick=2, /hibar
        device, /close

        ;# plot the results
        set_plot, 'PS'
        printf, FELUN, obsid+'_feprof.ps'
        device, filename = obsid+'_feprof.ps', $
                /portrait, $
                /times
        xran = [0.9*min(r),1.1*max(r)]
        yran = [0.9*min(fe-felo),1.1*max(fe+fehi)]
        plot, xran, yran, $
              /nodata, $
              xran = xran, $
              yran = yran, $
              /xlog, $
              /xsty, /ysty, $
              charsize = 0.8, $
              xtitle = textoidl('R_{mid} [kpc]'), $
              ytitle = textoidl('Z/Z'+sunsymbol()), $
              title  = name+'   '+obsid
        plotsym, 0, 0.8, /fill
        oplot, r, fe, psym=0, thick=2
        oplot, r, fe, psym=8, thick=1
        oploterror, r, fe, felo, psym=8, thick=2, /lobar
        oploterror, r, fe, fehi, psym=8, thick=2, /hibar
        device, /close
    ENDIF
err:
    IF notx EQ 'yes' THEN printf, LOGLUN, name,' ',obsid,' no temperature fit'
ENDFOR

free_lun, LOGLUN
free_lun, TXLUN
free_lun, FELUN
set_plot, 'X'

IF doind EQ 'yes' THEN BEGIN
   SPAWN, 'cat txlist | pscat 6 txprofs.ps'
   SPAWN, 'rm -f txlist'
   SPAWN, 'rm -f *txprof.ps'
   SPAWN, 'cat felist | pscat 6 feprofs.ps'
   SPAWN, 'rm -f felist'
   SPAWN, 'rm -f *feprof.ps'
ENDIF

;# Plot, Johnny!, PLOOOOTTTT!!!
loadct, 13, /silent
set_plot, 'PS'
device, filename='alltx.eps', $
        /color, $
        /encapsulated, $
        /portrait, $
        /times
;xran = [0.0003,0.5]
xran = [1,1000]
yran = [0.8,30.]
plot, xran, yran, $
      /nodata, $
      xran = xran, $
      yran = yran, $
      /xlog, /ylog, $
      /xsty, /ysty, $
      charsize = 0.8, $
      xtitle = textoidl('R_{mid} [kpc]'), $
      ytitle = textoidl('Temperature [keV]')

FOR j=0,n_elements(alltx)-1 DO BEGIN
    pcolor = allcolor[j]
    x   = *allr[j]
    y   = *alltx[j]
    ylo = *alltlo[j]
    yhi = *allthi[j]
    oplot, x, y, psym=0, thick=2, color=pcolor
ENDFOR
device, /close

;# Plot, Johnny!, PLOOOOTTTT!!!
loadct, 13, /silent
set_plot, 'PS'
device, filename='allfe.eps', $
        /color, $
        /encapsulated, $
        /portrait, $
        /times
xran = [1,1000]
yran = [0.,2.0]
plot, xran, yran, $
      /nodata, $
      xran = xran, $
      yran = yran, $
      /xlog, $
      /xsty, /ysty, $
      charsize = 0.8, $
      xtitle = textoidl('R_{mid} [kpc]'), $
      ytitle = textoidl('Metal Abundance [Z/Z'+sunsymbol()+']')

FOR j=0,n_elements(allfe)-1 DO BEGIN
    pcolor = allcolor[j]
    x   = *allr[j]
    y   = *allfe[j]
    ylo = *allfelo[j]
    yhi = *allfehi[j]
    oplot, x, y, psym=0, thick=2, color=pcolor
ENDFOR
device, /close

;# Plot, Johnny!, PLOOOOTTTT!!!
loadct, 13, /silent
set_plot, 'PS'
xmin = 0.8*min(allk0)
xmax = 1.2*max(allk0)
ymin = 0.8*min(allfe0)
ymax = 1.2*max(allfe0)
device, filename='fe0k0.eps', $
  /color, $
  /encapsulated, $
  /portrait, $
  /times
plot, allk0, allfe0, $
  /nodata, $
  xran = xran, $
  yran = yran, $
  /xlog, $
  /xsty, /ysty, $
  charsize = 0.8, $
  xtitle = textoidl('K_0 [keV cm^2]'), $
  ytitle = textoidl('Core Metal Abundance [Z/Z'+sunsymbol()+']')
plotsym, 0, 0.8, /fill
oplot, allk0, allfe0, color=0, psym=8
oploterror, allk0, allfe0, allk0err, allfe0lo, psym=8, thick=2, /lobar
oploterror, allk0, allfe0, allk0err, allfe0hi, psym=8, thick=2, /hibar
plotsym, 0, 0.8*0.8, /fill
oplot, allk0, allfe0, color=225, psym=8
device, /close

;# try binning l14
x = alog10(l14)
y = l14fe
yhi = l14fehi
ylo = l14felo
ord = sort(x)
x = x[ord]
y = y[ord]
yhi = yhi[ord]
ylo = ylo[ord]
count = 0
total = 0
void, xwav
void, ywav
void, ysigwav
void, xlo
void, xhi
;goal = round(n_elements(x)/5)
WHILE (total NE n_elements(x)) DO BEGIN
   IF (count LT goal) THEN BEGIN
      push, xtemp, x[total]
      push, ytemp, y[total]
      IF yhi[total] GT ylo[total] THEN push,yweight,1/(yhi[total])^2. ELSE push,yweight,1/(ylo[total])^2.
      count++
   ENDIF
   IF ((count EQ goal) OR (total EQ n_elements(x)-1)) THEN BEGIN
      push, ywav, (total(yweight*ytemp))/total(yweight)
      push, xwav, median(xtemp)
      push, ysigwav, (1./sqrt(total(yweight)))
      push, xlo, median(xtemp)-min(xtemp)
      push, xhi, max(xtemp)-median(xtemp)
      void, xtemp
      void, ytemp
      void, yweight
      count = 0
   ENDIF
   total++
ENDWHILE

;# plot fe0 vs. l14
set_plot, 'PS'
xmin = 0.8*min(xwav)
xmax = 1.2*max(xwav)
xmin = -1
xmax = 1
ymin = 0.8*min(ywav)
ymax = 1.2*max(ywav)
device, filename='fe0l14.eps', $
  /color, $
  /encapsulated, $
  /portrait, $
  /times
plot, xwav, ywav, $
 /nodata, $
 xran = [xmin,xmax], $
 yran = [ymin,ymax], $
 /xsty, /ysty, $
 charsize = 0.8, $
 xtitle = textoidl('log L_{1.4} [40 erg s^{-1}]'), $
 ytitle = textoidl('Core Abundance [Z/Z'+sunsymbol()+']')
plotsym, 0, 0.8, /fill
oplot, xwav, ywav, color=0, psym=8
oploterror, xwav, ywav, xlo, ylo, psym=8, thick=2, /lobar
oploterror, xwav, ywav, xhi, yhi, psym=8, thick=2, /hibar
plotsym, 0, 0.8*0.8, /fill
oplot, xwav, ywav, color=225, psym=8

;; x = l14
;; xlo = l14err
;; xhi = l14err
;; y = l14fe
;; yhi = l14fehi
;; ylo = l14felo
;; xmin = 0.8*min(x)
;; xmax = 1.2*max(x)
;; ymin = 0.8*min(y)
;; ymax = 1.2*max(y)
;; plot, x, y, $
;;       /nodata, $
;;       xran = [xmin,xmax], $
;;       yran = [ymin,ymax], $
;;       /xlog, $
;;       /xsty, /ysty, $
;;       charsize = 0.8, $
;;       xtitle = textoidl('L_{1.4} [10^{40} erg s^{-1}]'), $
;;       ytitle = textoidl('Core Abundance [Z/Z'+sunsymbol()+']')
;; plotsym, 0, 0.8, /fill
;; oplot, x, y, color=0, psym=8
;; ;oploterror, x, y, xlo, ylo, psym=8, thick=2, /lobar
;; ;oploterror, x, y, xhi, yhi, psym=8, thick=2, /hibar
;; plotsym, 0, 0.8*0.8, /fill
;; oplot, x, y, color=225, psym=8

device, /close

END
