pro ftxbin

myhome = GETENV('HOME')
!quiet=1
prefix = 'culled'
mpcol = 2
mprow = 4
output = 'ftxbin.ps'
dofilter = "yes"
filter = 50.
cut = 1.0
csize = 0.6
psize = 0.4
upper = 2.5
bins = 2.0
xmin = 1.
xmax = 18.
ymin = 0.92
ymax = 1.31

; input files array
push, pstitle, textoidl('R_{2500-CORE}')
push, pstitle, textoidl('R_{2500}')
push, pstitle, textoidl('R_{5000-CORE}')
push, pstitle, textoidl('R_{5000}')
push, files, '../me_fits/dat/'+prefix+'_r2500-50_7-7.dat'
push, files, '../me_fits/dat/'+prefix+'_r2500-50_2-7.dat'
push, files, '../me_fits/dat/'+prefix+'_r2500_7-7.dat'
push, files, '../me_fits/dat/'+prefix+'_r2500_2-7.dat'
push, files, '../me_fits/dat/'+prefix+'_r5000-50_7-7.dat'
push, files, '../me_fits/dat/'+prefix+'_r5000-50_2-7.dat'
push, files, '../me_fits/dat/'+prefix+'_r5000_7-7.dat'
push, files, '../me_fits/dat/'+prefix+'_r5000_2-7.dat'

i = 0
m = 0
FOR j = 0,n_elements(pstitle)-1 DO BEGIN

    restore,myhome+"/research/redux/scripts/xspectemp_rin_normerr_src.sav"        
    full = read_ascii(files[i], template = xspectemp_rin_normerr_src)
    hard = read_ascii(files[i+1], template = xspectemp_rin_normerr_src)

    fulllo = full.tx - full.tlo
    fullhi = full.thi - full.tx
    hardlo = hard.tx - hard.tlo
    hardhi = hard.thi - hard.tx

    tx77 = full.tx
    lo77 = fulllo
    hi77 = fullhi
    tx27 = hard.tx
    lo27 = hardlo
    hi27 = hardhi
    x    = tx77
    y    = hard.tx/full.tx
    yhi  = hard.tx/full.tx*(sqrt((hardhi/hard.tx)^2.+(fullhi/full.tx)^2.))
    ylo  = hard.tx/full.tx*(sqrt((hardlo/hard.tx)^2.+(fulllo/full.tx)^2.))
    num = n_elements(where(y GT cut))
    signum = n_elements(where(y-ylo GT cut))

;for s=0,n_elements(y)-1 DO BEGIN
;    dur = 0.25
;    fr1 = abs(1-(yhi[s]+y[s])/y[s])
;    fr2 = abs(1-(ylo[s]+y[s])/y[s])
;    IF ((fr1 GE dur) OR (fr2 GE dur)) THEN print, full.cluster[s]," ",full.obsid[s]," ",fr1," ",fr2
;endfor
;stop
;    IF (j EQ 0) THEN BEGIN
;        FOR jj=0,n_elements(y)-1 DO BEGIN
;            IF y[jj]-ylo[jj] GT 1.1 THEN BEGIN
;                push, names, full.cluster[jj]
;                push, tf, y[jj]-ylo[jj]
;            ENDIF
;;            print, $
;;              full.cluster[jj]," ",full.obsid[jj]," ",num2str(y[jj],3)," + ",$
;;              num2str(yhi[jj])," - ",num2str(ylo[jj])
;        ENDFOR
;        ord = sort(tf)
;        ord = reverse(ord)
;        names = names[ord]
;        tf = tf[ord]
;        FOR i = 0,n_elements(names)-1 DO BEGIN
;            print, names[i]," ",tf[i]
;        ENDFOR
;        void, tf
;        void, names
;    ENDIF

    ; an ad-hoc filter
    ; filter on source percent
    IF dofilter EQ "yes" THEN BEGIN
        src = full.src
;        ord = where(yhi LE 1.0 AND ylo LE 1.0)
;        ord = where(yhi/y LE 0.5)
;        ord = where(yhi+y LE 2.5)
        ord = where(src GT filter)
        x   = x[ord]
        y   = y[ord]
        yhi = yhi[ord]
        ylo = ylo[ord]
        tx77 = tx77[ord]
        lo77 = lo77[ord]
        hi77 = hi77[ord]
        tx27 = tx27[ord]
        lo27 = lo27[ord]
        hi27 = hi27[ord]
        numcut = n_elements(where(y GT cut))
        signumcut = n_elements(where(y-ylo GT cut))
    ENDIF

    ; get wavg for full sample
    IF m GT 0 THEN void, allwg
    FOR kk=0,n_elements(y)-1 DO BEGIN
        IF yhi[kk] GT ylo[kk] THEN push,allwg,1/(yhi[kk])^2. ELSE push,allwg,1/(ylo[kk])^2.
    ENDFOR
    allwav = (total(allwg*y))/total(allwg)
    allsigwav = (1./sqrt(total(allwg)))

    ; get wavg for 77
    IF m GT 0 THEN void, wg77
    FOR kk=0,n_elements(tx77)-1 DO BEGIN
        IF hi77[kk] GT lo77[kk] THEN push,wg77,1/(hi77[kk])^2. ELSE push,wg77,1/(lo77[kk])^2.
    ENDFOR
    wv77 = (total(wg77*tx77))/total(wg77)
    sig77 = (1./sqrt(total(wg77)))

    ; get wavg for 27
    IF m GT 0 THEN void, wg27
    FOR kk=0,n_elements(tx27)-1 DO BEGIN
        IF hi27[kk] GT lo27[kk] THEN push,wg27,1/(hi27[kk])^2. ELSE push,wg27,1/(lo27[kk])^2.
    ENDFOR
    wv27 = (total(wg27*tx27))/total(wg27)
    sig27 = (1./sqrt(total(wg27)))

    ; calculate the weighted average and associated error for bins of size
    ; 1keV to ease the look of the plot
    binmin = 0.
    binmax = 1.
    mm = 0
    count = 0
    total = 0
    IF (m GT 0) THEN BEGIN
        void, xwav
        void, ywav
        void, ysigwav
        void, xlo
        void, xhi
    ENDIF
    FOR ss = 0,round(max(x)) DO BEGIN
        IF mm GT 0 THEN BEGIN
            IF (n_elements(xtemp) GT 0) THEN BEGIN
                void,xtemp
                void,ytemp
                void,yweight
            ENDIF
        ENDIF
        FOR k = 0,n_elements(x)-1 DO BEGIN
            IF (x[k] GE binmin AND x[k] LE binmax) THEN BEGIN
                push, xtemp, x[k]
                push, ytemp, y[k]
                IF yhi[k] GT ylo[k] THEN push,yweight,1/(yhi[k])^2. ELSE push,yweight,1/(ylo[k])^2.
            ENDIF
        ENDFOR
        IF (n_elements(xtemp) GT 0) THEN BEGIN
            push, ywav, (total(yweight*ytemp))/total(yweight)
            push, xwav, median(xtemp)
            push, ysigwav, (1./sqrt(total(yweight)))
            push, xlo, median(xtemp)-binmin
            push, xhi, binmax-median(xtemp)
        ENDIF
        mm++
        IF binmax LT 10. THEN binsize=bins ELSE binsize=10.
        binmin = binmax
        binmax = binmax+binsize
    ENDFOR

    print, pstitle[j]
;    print, "Num clusters: ",num2str(n_elements(full.cluster))
;    print, "Num clusters Tfrac > ",num2str(cut,3)," @ 1sigma: ",num2str(signum)
;    print, "Num clusters Tfrac > ",num2str(cut,3)," @ 1sigma and Src > ",$
;           num2str(filter,3),": ",num2str(signumcut)
    print, "Filter: ",num2str(filter,3)," ",$
           "WV77: ",num2str(wv77,3)," +/- ",num2str(sig77,1)," ",$
           "WV27: ",num2str(wv27,3)," +/- ",num2str(sig27,1)," ",$
           "ALLWV: ",num2str(allwav,3)," +/- ",num2str(allsigwav,1)

    plotsym, 0, psize, /fill
    IF (m EQ 0) THEN BEGIN
        set_plot,'PS'
        device, filename = 'temp.ps'
        !fancy = 4
        !linetype = 0
        !p.font = 0
        multiplot,[mpcol,mprow],/rowmajor
    ENDIF
    IF (m NE 0) THEN multiplot
    plot, xwav, ywav, $
          /xsty, /ysty, $
          psym = 8, $
          xrange = [xmin,xmax], $
          yrange = [ymin,ymax], $
          symsize = psize, $
          charsize = csize

    ; overplot the line y=1
    oplot,findgen(xmax*1000)+xmin,replicate(1,1000), linestyle=2

    ; overplot the wavg for the whole sample
    wx = findgen(100)
    wy = replicate(allwav,100)
    oplot, wx, wy, linestyle=3

    ; overplot the errors
    oploterror, xwav, ywav, xlo, ysigwav, psym=8, /lobar 
    oploterror, xwav, ywav, xhi, ysigwav, psym=8, /hibar

    ; draw a legend
    items = [pstitle[j]]
    linearr = replicate(-99,n_elements(items))
    psyarr = replicate(-99,n_elements(items))
    legend, items, linestyle=linearr, psym=psyarr, charsize=csize, $
            /top, box=0, /right_legend

    IF (m NE 0 AND n_elements(upperx) NE 0) THEN BEGIN
        void,uppery
        void,upperx
    ENDIF

    FOR jj=0,n_elements(y)-1 DO BEGIN
        IF y[jj]+yhi[jj] GT upper THEN BEGIN
            yhi[jj]=0.
            push,uppery,y[jj]
            push,upperx,x[jj]
        ENDIF
    ENDFOR

    ; overplot the full points
    plotsym, 0, psize-0.25*psize, /fill
    multiplot & plot, x, y, $
          /xsty, /ysty, $            
          psym=8, $
          xran = [xmin,xmax], $
          yran = [0.2,upper], $
          charsize = csize
;    oploterror, x, y, ylo, psym=8, /lobar, /nohat
;    oploterror, x, y, yhi, psym=8, /hibar, /nohat
    oplot,findgen(xmax*1000)+xmin,replicate(1,1000), linestyle=2

    ; overplot upper bound arrows
    plotsym, 2, psize+1, /fill, thick=2
    IF (n_elements(upperx) NE 0) THEN oplot, upperx, uppery, psym=8

    ; overplot the wavg for the whole sample
    wx = findgen(100)
    wy = replicate(allwav,100)
    oplot, wx, wy, linestyle=3

    ; draw a legend
    legend, items, linestyle=linearr, psym=psyarr, charsize=csize, $
            /top, box=0, /right_legend

    ; increment counters
    i = i+2
    m++
ENDFOR

; horz label
xtex = textoidl('T_{0.7-7.0} [keV]')
xyouts, 0.52, 0.1, xtex, /normal, charsize=csize

; vert label
ytex = textoidl('T_{2.0-7.0}/T_{0.7-7.0}')
xyouts, 0.12, 0.52, ytex, /normal, orientation=90, charsize=csize

device,/close
!quiet=0

SPAWN, 'ps2ps temp.ps '+output
file_delete, 'temp.ps', /quiet

END

