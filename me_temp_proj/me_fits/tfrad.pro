pro tfrad

cut = -100.0
xmin = 0.08
xmax = 80000.
ymin = 0.4
ymax = 3.0

; input files array
push, files, 'dat/c2fits_final_r2500-50_fefree_7-7.dat'
push, files, 'dat/c2fits_final_r2500-50_fefree_2-7.dat'
;radfile = 'me_nvss_radiolx.dat'
radfile = 'me_nvss_radiolx_comp.dat'

readcol, radfile, FORMAT='A,I,A,F,F,F,F',$
         name, robsids, type, z, flux, err, rlum, rlumerr, comment='#'
restore,"~/research/redux/scripts/xspectemp_rin_normerr_src.sav"        
full = read_ascii(files[0], template = xspectemp_rin_normerr_src)
hard = read_ascii(files[1], template = xspectemp_rin_normerr_src)

fobs = full.obsid
hobs = hard.obsid
robs = robsids
fname = full.cluster
hname = hard.cluster
fulltx = full.tx
fulllo = full.tx - full.tlo
fullhi = full.thi - full.tx
hardtx = hard.tx
hardlo = hard.tx - hard.tlo
hardhi = hard.thi - hard.tx

openw, /get_lun, log, 'tfrad.log'
openw, /get_lun, errlog, 'err_tfrad.log'
FOR i=0,n_elements(robs)-1 DO BEGIN
    fget = where(fobs EQ robs[i])
    hget = where(hobs EQ robs[i])
    check = "yes"
    IF (fget LT 0 OR $
        hget LT 0 OR $
        rlum[i] LE 0 OR $
        type[i] EQ "none" $
       ) THEN BEGIN
        check = "no"
        goto, next
    ENDIF
    fn = fname[fget]
    hn = hname[hget]
    frac = hardtx[hget]/fulltx[fget]
    fraclo = frac*(sqrt((hardlo[hget]/hardtx[hget])^2.+(fulllo[fget]/fulltx[fget])^2.))
    frachi = frac*(sqrt((hardhi[hget]/hardtx[hget])^2.+(fullhi[fget]/fulltx[fget])^2.))
    IF frac-fraclo LT cut THEN BEGIN
        check = "no"
        goto, next
    ENDIF
    push, obs, robs[i]
    push, allrlum, rlum[i]
    push, allrlumerr, rlumerr[i]
    push, allrlumweight, 1./(rlumerr[i])^2.
    push, allfrac, frac
    push, allfraclo, fraclo
    push, allfrachi, frachi
    printf, log, format='(A-20,A20,A10,F10.2,F10.2)',fn,hn,robs[i],rlum[i],frac
    IF (type[i] EQ "F") THEN BEGIN
        push, frlum, rlum[i]
        push, frlumerr, rlumerr[i]
        push, ffrac, frac
        push, ffraclo, fraclo
        push, ffrachi, frachi
    ENDIF ELSE BEGIN
        push, nfrlum, rlum[i]
        push, nfrlumerr, rlumerr[i]
        push, nffrac, frac
        push, nffraclo, fraclo
        push, nffrachi, frachi
    ENDELSE
    IF (frac GT 1.5) THEN print, name[i]," ",strcompress(robs[i],/remove_all)," ",num2str(frac,4)," ",num2str(rlum[i],4)
next:
    IF (check EQ "no") THEN $
      printf, errlog, format='(A-20,A10,A12)',name[i], robsids[i]," no fit data"
ENDFOR
close, log
close, errlog

; calculate the weighted average and associated error for bins
binmin = 0.01
binmax = 0.1
mm = 0
FOR ss = 0,round(max(allrlum)) DO BEGIN
    IF mm GT 0 THEN BEGIN
        IF (n_elements(allrlumtemp) GT 0) THEN BEGIN
            void,allrlumtemp
            void,allfractemp
            void,allfracweight
        ENDIF
    ENDIF
    FOR k = 0,n_elements(allrlum)-1 DO BEGIN
        IF (allrlum[k] GE binmin AND allrlum[k] LE binmax) THEN BEGIN
            push, allrlumtemp, allrlum[k]
            push, allrlumtemperr, allrlumerr[k]
            push, allfractemp, allfrac[k]
            IF allfrachi[k] GT allfraclo[k] THEN push,allfracweight,1/(allfrachi[k])^2. ELSE push,allfracweight,1/(allfraclo[k])^2.
        ENDIF
    ENDFOR
    IF (n_elements(allrlumtemp) GT 0) THEN BEGIN
        push, allfracwav, (total(allfracweight*allfractemp))/total(allfracweight)
        push, allfracsigwav, (1./sqrt(total(allfracweight)))
        push, allfracstdwav, sqrt((total(allfracweight*(allfractemp-allfracwav)^2.))/((n_elements(allfracweight)-1)*total(allfracweight)/n_elements(allfracweight)))
        push, allrlumwav, median(allrlumtemp)
        push, allrlumsigwav, (1./sqrt(total(allrlumtemperr)))
        push, allrlumstdwav, sqrt((total(allrlumweight*(allrlumtemp-allrlumwav)^2.))/((n_elements(allrlumweight)-1)*total(allrlumweight)/n_elements(allrlumweight)))
        push, allrlumlo, median(allrlumtemp)-binmin
        push, allrlumhi, binmax-median(allrlumtemp)
    ENDIF
    mm++
    binmin = binmax
    binmax = binmax*2.
ENDFOR

; do all the plotting
set_plot,'PS'
device, filename = 'tfrad.eps', /color, /encapsulated
!fancy = 4
!linetype = 0
!p.font = 0
plotsym, 0, /fill
check = n_elements(frlum)
IF check EQ 0 THEN BEGIN
    x = [xmin,xmax]
    y = [ymin,ymax]
ENDIF ELSE BEGIN
    x = frlum
    y = ffrac
ENDELSE

plot, x, y, $
      /xlog, $
      /xsty, /ysty, $
      psym=8, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      charsize=0.8, $
      symsize=0.8, $
      xtitle = textoidl('L_{radio} [10^{40} ergs sec^{-1}]'), $
      ytitle = textoidl('T_{HFR}')

; overplot the errors
IF check NE 0 THEN BEGIN
    oploterror, frlum, ffrac, frlumerr, ffraclo, psym=8, /nohat, /lobar, symsize=0.8
    oploterror, frlum, ffrac, frlumerr, ffrachi, psym=8, /nohat, /hibar, symsize=0.8
ENDIF

; NFs
plotsym, 0
oplot, nfrlum, nffrac, symsize=0.8, psym=8
plotsym, 6, 2, thick=2
oplot, nfrlum, nffrac, symsize=0.8, psym=8
oploterror, nfrlum, nffrac, nffraclo, psym=8, /nohat, /lobar, symsize=0.8
oploterror, nfrlum, nffrac, nffrachi, psym=8, /nohat, /hibar, symsize=0.8
items = ['Not Found','Found']
linearr = [0,0]
psyarr = [5,6]
legend, items, linestyle=linearr, psym=psyarr, charsize=0.8, $
        /top, /right_legend, box=0

; overplot the line y=1
ax = maken(xmin,xmax,10)
ay = replicate(1,n_elements(ax))
oplot, ax, ay, linestyle=2

; fit a line to data
;FOR h=0,n_elements(allfrac)-1 DO BEGIN
;    IF allfraclo[h] GT allfrachi[h] THEN push, ayerr, allfraclo[h] ELSE push, ayerr, allfrachi[h]
;ENDFOR
;bc = bces(allrlum, allfrac);, error=bcerr, xerror=allrlumerr, yerror=ayerr)
;ax = maken(xmin,xmax,10)
;ay = bc[1]*ax+bc[0]
;oplot, ax, ay, linestyle=1

;; close device
device, /close

; more plotting
set_plot,'PS'
device, filename = 'tfrad2.eps', /color, /encapsulated
!fancy = 4
!linetype = 0
!p.font = 0
plotsym, 4, /fill
plot, allrlumwav, allfracwav, $
      /xlog, $
      /xsty, /ysty, $
      psym=8, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      charsize=0.8, $
      symsize=0.8, $
      xtitle = textoidl('L_{radio} [10^{40} ergs sec^{-1}]'), $
      ytitle = textoidl('T_{HFR}')

; overplot the errors
oploterror, allrlumwav, allfracwav, allrlumlo, allfracsigwav, psym=8, /lobar, symsize=0.8
oploterror, allrlumwav, allfracwav, allrlumhi, allfracsigwav, psym=8, /hibar, symsize=0.8

; overplot the line y=1
ax = maken(xmin,xmax,10)
ay = replicate(1,n_elements(ax))
oplot, ax, ay, linestyle=2

device, /close

END
