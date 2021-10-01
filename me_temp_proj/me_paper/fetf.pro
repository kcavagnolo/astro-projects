pro fetf

!quiet=1
csize = 0.9
cthick = 3
psize = 0.4
dofilter = "no"

; set-up device
set_plot,'PS'
device, $
  filename = 'fetf.eps', $
  /encapsulated, $
  /portrait, $
  /helvetica
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 3
!Y.THICK  = 3
!Z.THICK  = 3
!X.OMARGIN = [-4,0]
!Y.OMARGIN = [-1,0]
multiplot,[1,3],/rowmajor

; input files array
push, pstitle, textoidl('R_{2500-CORE}')
push, pstitle, textoidl('R_{5000-CORE}')
push, pstitle, textoidl('Control')
push, files, '../me_fits/dat/culled_r2500-50_7-7.dat'
push, files, '../me_fits/dat/culled_r2500-50_2-7.dat'
push, files, '../me_fits/dat/culled_r5000-50_7-7.dat'
push, files, '../me_fits/dat/culled_r5000-50_2-7.dat'
push, files, '../me_fits/dat/old_fak_control_7-7.dat'
push, files, '../me_fits/dat/old_fak_control_2-7.dat'

i = 0
m = 0

FOR j = 0,n_elements(pstitle)-1 DO BEGIN

    restore,"/home/cavagnolo/research/redux/scripts/xspectemp_rin_normerr_src.sav"        
    full = read_ascii(files[i], template = xspectemp_rin_normerr_src)
    hard = read_ascii(files[i+1], template = xspectemp_rin_normerr_src)

    ; all the following steps assume the files are in the same order
    tx77 = full.tx
    tx27 = hard.tx
    fe77 = full.fe
    fe27 = hard.fe
    z = full.z
    names = full.cluster
    fulllo = full.tx - full.tlo
    fullhi = full.thi - full.tx
    hardlo = hard.tx - hard.tlo
    hardhi = hard.thi - hard.tx
    ffelo = full.fe - full.felo
    ffehi = full.fehi - full.fe
    hfelo = hard.fe - hard.felo
    hfehi = hard.fehi - hard.fe

;    ;; XCRF correction
;    IF (pstitle[j] NE "Control") THEN tx77 = tx77*1.1

    ;; check for files
    check = full.obsid/hard.obsid
    uhoh = where(check NE 1)
    IF uhoh NE -1 THEN BEGIN
        print,'Uh oh... out of order file'
        exit
    ENDIF

    ord  = sort(tx77)
    names = names[ord]
    tx77 = tx77[ord]
    lo77 = fulllo[ord]
    hi77 = fullhi[ord]
    tx27 = tx27[ord]
    lo27 = hardlo[ord]
    hi27 = hardhi[ord]
    fe77 = fe77[ord]
    felo77 = ffelo[ord]
    fehi77 = ffehi[ord]
    fe27 = fe27[ord]
    felo27 = hfelo[ord]
    fehi27 = hfehi[ord]
    z    = z[ord]
    x    = fe77
    xlo  = ffelo
    xhi  = ffehi
    y    = tx27/tx77
    yhi  = (tx27/tx77)*(sqrt((hi27/tx27)^2.+(hi77/tx77)^2.))
    ylo  = (tx27/tx77)*(sqrt((lo27/tx27)^2.+(lo77/tx77)^2.))

    IF dofilter EQ "yes" THEN BEGIN
        src = full.src
        filter = where(z LE 0.6)
        names = names[filter]
        z = z[filter]
        x = x[filter]
        xhi = xhi[filter]
        xlo = xlo[filter]
        y = y[filter]
        yhi = yhi[filter]
        ylo = ylo[filter]
        tx77 = tx77[filter]
        lo77 = lo77[filter]
        hi77 = hi77[filter]
        tx27 = tx27[filter]
        lo27 = lo27[filter]
        hi27 = hi27[filter]
        fe77 = fe77[filter]
        felo77 = felo77[filter]
        fehi77 = fehi77[filter]
        fe27 = fe27[filter]
        felo27 = felo27[filter]
        fehi27 = fehi27[filter]
    ENDIF

    ; draw a legend
    items = [pstitle[j]]
    linearr = replicate(-99,n_elements(items))
    psyarr = replicate(-99,n_elements(items))

    ; overplot the full points
    plotsym, 0, psize-0.25*psize, /fill
    IF (m NE 0) THEN multiplot
    IF (m EQ 0) THEN BEGIN
        xmin = -0.05
        xmax = 1.05
    ENDIF
    ymin = 0.8*min(y)
    ymax = 1.1*max(y)
    plot, x, y, $
      /xsty, /ysty, $            
      psym=8, $
      xran = [xmin,xmax], $
      yran = [ymin,ymax], $
      charsize = csize, $
      charthick = cthick
;    oploterror, x, y, xlo, ylo, psym=8, /lobar, /nohat
;    oploterror, x, y, xhi, yhi, psym=8, /hibar, /nohat

    ; draw a legend
    legend, items, linestyle=linearr, psym=psyarr, charsize=csize, $
            charthick=cthick, /top, box=0, /right_legend

    ;# performing fit of linear relation
    void, yerr
    void, xerr
    FOR ii=0,n_elements(y)-1 DO BEGIN
       IF ylo[ii] GT yhi[ii] THEN push, yerr, ylo[ii] ELSE push, yerr, yhi[ii]
       IF xlo[ii] GT xhi[ii] THEN push, xerr, xlo[ii] ELSE push, xerr, xhi[ii]
    ENDFOR
    result = linfit(x, y, chisq=chisq, sigma=sigma, prob=prob, /double)
    bc = bces(x, y, error=bcerr, xerror=xerr, yerror=yerr)
    dof = n_elements(y)-2
    ox = maken(-1d4,1d4,10)
    oy = result[1]*ox+result[0]
    oplot, ox, oy, psym=0, linestyle=2, thick=2
    print, pstitle[j]
    print, '**LINFIT**'
    print, FORMAT='(A-10,F10.2,A10,F10.2)','Slope:',result[1],'+/-',sigma[1]
    print, FORMAT='(A-10,F10.2,A10,F10.2)','Inter:',result[0],'+/-',sigma[0]
    print, FORMAT='(A-10,F10.2,A10,F10.2)','Chisq:',chisq,'DOF:',dof
    print, FORMAT='(A-10,F10.2,A10,F10.2)','Redch:',chisq/dof,'Prob:',prob
    print, '**BCES**'
    print, FORMAT='(A-10,F10.2,A10,F10.2)','Slope:',bc[1],'+/-',bcerr[1]
    print, FORMAT='(A-10,F10.2,A10,F10.2)','Inter:',bc[0],'+/-',bcerr[0]
    print, ''

    i = i+2
    m++
ENDFOR

; labels
xtex = textoidl('Z/Z'+sunsymbol())
ytex = textoidl('T_{HBR} = T_{2.0-7.0}/T_{0.7-7.0}')
xyouts, 0.5, 0.03, xtex, /normal, charsize=1.0, charthick=cthick
xyouts, 0.035, 0.40, ytex, /normal, orientation=90, charsize=1.0, charthick=cthick

device,/close
!quiet=0

END

