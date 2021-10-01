;+
; NAME:
;
; PURPOSE:
;
;
; AUTHOR:
;
;
; CATEGORY:
;
;
; CALLING SEQUENCE:
;
;
; REQUIRED INPUTS:
;
;
; OPTIONAL INPUTS
;
;
; OPTIONAL KEYWORD PARAMETERS:
;
;
; COMMON BLOCKS:
;
; EXAMPLE:
;-

;#####################
;#####################

PRO intr_sct, fidtf, sigint, binning, iter

; return to caller on error
ON_ERROR, 2
csize=0.5

; check for correct number of parameters
IF n_params() EQ 0. THEN BEGIN
    print, 'Syntax - INTR_SCT, <tfrac>, <intrinsic_scatter_array>, <binning>, <iterations>'
    print, 'IE: intr_sct, 1.14, [0.2, 0.1, 0.05], 0.025, 10'
    return
ENDIF

; read the data files
restore,"../scripts/xspectemp_rin_normerr_src.sav"
push, dat, '../me_fits/dat/culled_r2500-50_7-7.dat'
push, dat, '../me_fits/dat/culled_r2500-50_2-7.dat'
;push, dat, '../me_fits/dat/final_r2500-50_fefree_7-7.dat'
;push, dat, '../me_fits/dat/final_r2500-50_fefree_2-7.dat'

full = read_ascii(dat[0], template = xspectemp_rin_normerr_src)
hard = read_ascii(dat[1], template = xspectemp_rin_normerr_src)
tf   = hard.tx/full.tx
tfhi = (hard.tx/full.tx)*(sqrt(((hard.thi-hard.tx)/hard.tx)^2.+((full.thi-full.tx)/full.tx)^2.))
tflo = (hard.tx/full.tx)*(sqrt(((hard.tx-hard.tlo)/hard.tx)^2.+((full.tx-full.tlo)/full.tx)^2.))

FOR i=0,n_elements(sigint)-1 DO BEGIN
    IF i GT 0 THEN BEGIN
        void, newtf
        void, err
    ENDIF
    FOR j=0,n_elements(tf)-1 DO BEGIN
        IF tfhi[j] LT tflo[j] THEN push, err, tfhi[j] ELSE push, err, tflo[j]
        push, newtf, (fidtf + err[j]*randomn(k,iter) + sigint[i]*randomn(k,iter))
;        push, newtf, (fidtf + err[j]*randomn(k,iter)) ; data err only
;        push, newtf, (fidtf + sigint[i]*randomn(k,iter)) ; intrinsic err only
    ENDFOR
    wg = 1./(err^2.)
    mu = (total(wg*newtf))/total(wg)
    dev = stddev(newtf)
    print,sigint[i]
    print,"Mean : ",num2str(mu,3)
    print,"StdDev : ",num2str(dev,3)
    print,'********'
    IF i EQ 0 THEN BEGIN
        set_plot, 'PS'
        device, filename='intr_sct.ps'
        multiplot,[1,n_elements(sigint)+1]
    ENDIF
    IF i NE 0 THEN multiplot
    plothist, newtf,$
              bin=binning,$
              peak=1,$
              xrange=[0.5,2.0],$
              yrange=[0,1.1],$
              /xsty,/ysty,$
              charsize=csize
ENDFOR

i=0
void, err
WHILE i LT n_elements(dat) DO BEGIN
    full = read_ascii(dat[i], template = xspectemp_rin_normerr_src)
    hard = read_ascii(dat[i+1], template = xspectemp_rin_normerr_src)
    tf = (hard.tx/full.tx)
    tfhi = (hard.tx/full.tx)*(sqrt(((hard.thi-hard.tx)/hard.tx)^2.+((full.thi-full.tx)/full.tx)^2.))
    tflo = (hard.tx/full.tx)*(sqrt(((hard.tx-hard.tlo)/hard.tx)^2.+((full.tx-full.tlo)/full.tx)^2.))
    IF i GT 0 THEN void, err
    FOR j=0,n_elements(tf)-1 DO BEGIN
        IF tfhi[j] GT tflo[j] THEN push, err, tfhi[j] ELSE push, err, tflo[j]
    ENDFOR
    multiplot & plothist, tf,$
      bin=binning,$
      peak=1,$
      xrange=[0.5,2.0],$
      yrange=[0,1.1],$
      /xsty,/ysty,$
      charsize=csize
    wg = 1./(err^2.)
    mu = (total(wg*tf))/total(wg)
    dev = stddev(tf)
    print,"Mean ",dat[i],": ",num2str(mu,3)
    print,"StdDev ",dat[i],": ",num2str(dev,3)
    print,'********'
    i = i+2
ENDWHILE

xtex = textoidl('T_{frac}')
xyouts, 0.52, 0.08, xtex, /normal, charsize=csize
ytex = textoidl('Normalized Number of Clusters Per Bin')
xyouts, 0.08, 0.5, ytex, /normal, orientation=90, charsize=csize

device,/close

END
