pro ftxcount

myhome = GETENV('HOME')
!quiet=1
goal = 25
simgoal = 1000
csize = 0.9
cthick = 3
psize = 0.4
xmin = 1.
xmax = 21.
ymin = 0.95
ymax = 1.25
fullmin = 0.6
fullmax = 2.2
dofilter = "no"

; set-up device
set_plot,'PS'
device, $
  filename = 'ftxcount.eps', $
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
multiplot,[3,2],/rowmajor

; input files array
push, pstitle, textoidl('R_{2500-CORE}')
push, pstitle, textoidl('R_{5000-CORE}')
push, pstitle, textoidl('Control')
push, files, myhome+'/research/me_temp_proj/me_fits/dat/culled_r2500-50_7-7.dat'
push, files, myhome+'/research/me_temp_proj/me_fits/dat/culled_r2500-50_2-7.dat'
push, files, myhome+'/research/me_temp_proj/me_fits/dat/culled_r5000-50_7-7.dat'
push, files, myhome+'/research/me_temp_proj/me_fits/dat/culled_r5000-50_2-7.dat'
push, files, myhome+'/research/me_temp_proj/me_fits/dat/old_fak_control_7-7.dat'
push, files, myhome+'/research/me_temp_proj/me_fits/dat/old_fak_control_2-7.dat'

i = 0
m = 0

FOR j = 0,n_elements(pstitle)-1 DO BEGIN

    restore,myhome+'/research/redux/scripts/xspectemp_rin_normerr_src.sav'
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
    x    = tx77
    y    = tx27/tx77
    yhi  = (tx27/tx77)*(sqrt((hi27/tx27)^2.+(hi77/tx77)^2.))
    ylo  = (tx27/tx77)*(sqrt((lo27/tx27)^2.+(lo77/tx77)^2.))
    prefil = n_elements(x)

    ; an ad-hoc filter
    ; filter on source percent
    IF pstitle[j] EQ "Simulated" THEN BEGIN
        tx2 = full.rin
        eta = full.rout
        z = full.z
        filter = where(z LE 0.6)
        names = names[filter]
        x   = x[filter]
        y   = y[filter]
        z   = z[filter]
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
        dofilter = "no"
    ENDIF
    IF dofilter EQ "yes" THEN BEGIN
        src = full.src
        filter = where(z LE 0.6)
        names = names[filter]
        z = z[filter]
        x = x[filter]
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
    postfil = n_elements(x)

    ; get wavg for full sample
    IF m GT 0 THEN void, allwg
    FOR kk=0,n_elements(y)-1 DO BEGIN
        IF yhi[kk] GT ylo[kk] THEN push,allwg,1/(yhi[kk])^2. ELSE push,allwg,1/(ylo[kk])^2.
    ENDFOR
    allwav    = wtd_mean(y,allwg)
    allsigwav = (1./sqrt(total(allwg)))
    allstdwav = sqrt((total(allwg*y^2.)*total(allwg)-(total(allwg*y))^2.)/((total(allwg))^2.-total(allwg^2.)))

    ; get wavg for 77
    IF m GT 0 THEN void, wg77
    FOR kk=0,n_elements(tx77)-1 DO BEGIN
        IF hi77[kk] GT lo77[kk] THEN push,wg77,1/(hi77[kk])^2. ELSE push,wg77,1/(lo77[kk])^2.
    ENDFOR
    wv77  = wtd_mean(tx77,wg77)
    sig77 = (1./sqrt(total(wg77)))
    std77 = sqrt((total(wg77*tx77^2.)*total(wg77)-(total(wg77*tx77))^2.)/((total(wg77))^2.-total(wg77^2.)))

    ; get wavg for 27
    IF m GT 0 THEN void, wg27
    FOR kk=0,n_elements(tx27)-1 DO BEGIN
        IF hi27[kk] GT lo27[kk] THEN push,wg27,1/(hi27[kk])^2. ELSE push,wg27,1/(lo27[kk])^2.
    ENDFOR
    wv27  = wtd_mean(tx27,wg27)
    sig27 = (1./sqrt(total(wg27)))
    std27 = sqrt((total(wg27*tx27^2.)*total(wg27)-(total(wg27*tx27))^2.)/((total(wg27))^2.-total(wg27^2.)))

    ; some quik stats
    print, format='(A-20,A10,A10,A10,A10,A10,A10,A10,A10,A10)',$
      pstitle[j],"W77","SD","SDOM","W27","SD","SDOM","WAV","SD","SDOM"
    print, format='(A-20,F10.3,F10.3,F10.3,F10.3,F10.3,F10.3,F10.3,F10.3,F10.3)',$
      " ",wv77,std77,sig77,wv27,std27,sig27,allwav,allstdwav,allsigwav
    print, "Num of clusters, pre-filter: ",num2str(prefil,4)
    print, "Num of clusters, post-filter: ",num2str(postfil,4)
    ngt1 = float(n_elements(where(y-ylo GT 1.0)))
    per = (ngt1/n_elements(y))*100.
    print, "Number sims w/ Tf > 1.0 @ 1-sigma: ",num2str(ngt1)
    print, "Freq: ",num2str(per,4),"%"
    ngt1 = float(n_elements(where(y+yhi LT 1.0)))
    per = (ngt1/n_elements(y))*100.
    print, "Number sims w/ Tf < 1.0 @ 1-sigma: ",num2str(ngt1)
    print, "Freq: ",num2str(per,4),"%"

    IF ((pstitle[j] EQ "Simulated") OR (pstitle[j] EQ "Control")) THEN goal = simgoal
    mm = 0
    count = 0
    total = 0
    IF (m GT 0) THEN BEGIN
        void, xwav
        void, ywav
        void, ysigwav
        void, ystdwav
        void, xlo
        void, xhi
        total = 0
    ENDIF
    WHILE (total NE n_elements(x)) DO BEGIN
        IF (count LT goal) THEN BEGIN
            push, xtemp, x[total]
            push, ytemp, y[total]
            IF yhi[total] GT ylo[total] THEN push,yweight,1/(yhi[total])^2. ELSE push,yweight,1/(ylo[total])^2.
            count++
        ENDIF
        IF ((count EQ goal) OR (total EQ n_elements(x)-1)) THEN BEGIN
            tywav = (total(yweight*ytemp))/total(yweight)
            push, ywav, tywav
            push, xwav, median(xtemp)
            push, ysigwav, (1./sqrt(total(yweight)))
            push, ystdwav, sqrt((total(yweight*(ytemp-tywav)^2.))/((n_elements(yweight)-1)*total(yweight)/n_elements(yweight)))
            push, xlo, median(xtemp)-min(xtemp)
            push, xhi, max(xtemp)-median(xtemp)
            IF (total EQ n_elements(x)-1) THEN print, "last bin has: ", num2str(n_elements(xtemp),3)
            void, xtemp
            void, ytemp
            void, yweight
            count = 0
        ENDIF
        total++
    ENDWHILE

    plotsym, 0, psize, /fill
    IF (m NE 0) THEN multiplot
    plot, xwav, ywav, $
      /xsty, /ysty, $
      psym = 8, $
      xrange   = [xmin,xmax], $
      yrange   = [ymin,ymax], $
      symsize  = 1.75*psize, $
      charsize = csize, $
      charthick = cthick

    ; overplot the line y=1
    oplot,findgen(xmax*1000)+xmin,replicate(1,1000), linestyle=2

    ; overplot the wavg for the whole sample
    wx = findgen(100)
    wy = replicate(sigfig(allwav,3),100)
    oplot, wx, wy, linestyle=3

    ; overplot wavg errors
    oploterror, xwav, ywav, xlo, 2*ysigwav, psym=8, /lobar 
    oploterror, xwav, ywav, xhi, 2*ysigwav, psym=8, /hibar

    ; draw a legend
    items = [pstitle[j]]
    linearr = replicate(-99,n_elements(items))
    psyarr = replicate(-99,n_elements(items))
    legend, items, linestyle=linearr, psym=psyarr, charsize=csize, $
            charthick=cthick, /top, box=0, /right_legend

    ; overplot the full points
    plotsym, 0, psize-0.25*psize, /fill
    multiplot & plot, x, y, $
          /xsty, /ysty, $            
          psym=8, $
          xran = [xmin,xmax], $
          yran = [fullmin,fullmax], $
          charsize = csize, $
          charthick = cthick

    ; overplot the line y=1
    oplot,findgen(xmax*1000)+xmin,replicate(1,1000), linestyle=2

    ; overplot the wavg for the whole sample
    oplot, wx, wy, linestyle=3

    ; draw a legend
    legend, items, linestyle=linearr, psym=psyarr, charsize=csize, $
            charthick=cthick, /top, box=0, /right_legend

    i = i+2
    m++
ENDFOR

; labels
xtex = textoidl('T_{0.7-7.0} [keV]')
ytex = textoidl('T_{HBR} = T_{2.0-7.0}/T_{0.7-7.0}')
xyouts, 0.45, 0.03, xtex, /normal, charsize=1.0, charthick=cthick
xyouts, 0.035, 0.40, ytex, /normal, orientation=90, charsize=1.0, charthick=cthick

device,/close
!quiet=0

END

