pro fezbin

mpcol = 2
mprow = 4
output = 'fezbin.eps'
filter = 0.
csize = 0.6
psize = 0.4
upper = 5.0
bsize = 0.2
xmin = 0.0
xmax = 1.32
ymin = 0.
ymax = 0.45
fully = 1.0

; input files array
push, pstitle, textoidl('R_{2500-CORE}')
push, pstitle, textoidl('R_{2500}')
push, pstitle, textoidl('R_{5000-CORE}')
push, pstitle, textoidl('R_{5000}')
push, files, '../me_fits/dat/culled_r2500-50_7-7.dat'
push, files, '../me_fits/dat/culled_r2500_7-7.dat'
push, files, '../me_fits/dat/culled_r5000-50_7-7.dat'
push, files, '../me_fits/dat/culled_r5000_7-7.dat'

i = 0
m = 0
FOR j = 0,n_elements(pstitle)-1 DO BEGIN

    restore,"/Users/cavagnolo/research/redux/scripts/xspectemp_rin_normerr_src.sav"        
    full = read_ascii(files[i], template = xspectemp_rin_normerr_src)
    x = full.z
    y = full.fe
    ylo = full.fe - full.felo
    yhi = full.fehi - full.fe

    ; filter on source percent
    src = full.src
    ord = where(src GT filter)
    x   = x[ord]
    y   = y[ord]
    yhi = yhi[ord]
    ylo = ylo[ord]

    ; get wavg for full sample
    IF m GT 0 THEN void, allwg
    FOR kk=0,n_elements(y)-1 DO BEGIN
        IF yhi[kk] GT ylo[kk] THEN push,allwg,1/(yhi[kk])^2. ELSE push,allwg,1/(ylo[kk])^2.
    ENDFOR
    allwav = (total(allwg*y))/total(allwg)
    allsigwav = (1./sqrt(total(allwg)))
    print, pstitle[j]
    print, "Fe wavg for Src > ",num2str(filter,3),": ",num2str(allwav,3)," +/- ",num2str(allsigwav,4)

    ; calculate the weighted average and associated error for bins of size
    ; 0.05 to ease the look of the plot
    binmin = 0.
    binmax = bsize
    mm = 0
    IF (m GT 0) THEN BEGIN
        void, xwav
        void, ywav
        void, ysigwav
        void, xlo
        void, xhi
        IF n_elements(xempty) GT 0 THEN BEGIN
            void, xempty
            void, xloempty
            void, xhiempty
            void, yempty
            void, yloempty
            void, yhiempty
        ENDIF
    ENDIF
    FOR ss = 0,round(max(x)/bsize) DO BEGIN
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
        ; handle empty bins
        IF (n_elements(xtemp) EQ 0) THEN BEGIN
            push, xempty, (binmin+binmax)/2.
            push, xloempty, (binmin+binmax)/2-binmin
            push, xhiempty, binmax-(binmin+binmax)/2
            push, yempty, 0.05
            push, yloempty, 0.
            push, yhiempty, 0.
        ENDIF
        mm++
        IF binmax LT 0.79 THEN binsize=bsize ELSE binsize=1.0
        binmin = binmax
        binmax = binmax+binsize
    ENDFOR

    plotsym, 0, psize, /fill
    IF (m EQ 0) THEN BEGIN
        set_plot,'PS'
        device, filename = output, /encapsulated
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

    ; overplot the wavg for the whole sample
    wx = findgen(100)
    wy = replicate(allwav,100)
    oplot, wx, wy, linestyle=3

    ; overplot the errors
    oploterror, xwav, ywav, xlo, ysigwav, psym=8, /lobar 
    oploterror, xwav, ywav, xhi, ysigwav, psym=8, /hibar

    ; overplot the empty bins
    IF n_elements(xempty) GT 0 THEN BEGIN
        plotsym, 0, psize
        oplot, xempty, yempty, psym=8
        oploterror, xempty, yempty, xloempty, yloempty, psym=8, /lobar
        oploterror, xempty, yempty, xhiempty, yhiempty, psym=8, /hibar
    ENDIF

    ; draw a legend
    items = [pstitle[j]]
    linearr = replicate(-99,n_elements(items))
    psyarr = replicate(-99,n_elements(items))
    legend, items, linestyle=linearr, psym=psyarr, charsize=csize, $
            /top, box=0, /right_legend

    IF (m NE 0 and n_elements(upperx) GT 0) THEN BEGIN
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

    ; plot the full points
    plotsym, 0, psize-0.25*psize, /fill
    multiplot & plot, x, y, $
          /xsty, /ysty, $
          psym=8, $
          xran = [xmin,xmax], $
          yran = [0.,fully], $
          charsize = csize
;    oploterror, x, y, ylo, psym=8, /lobar, /nohat
;    oploterror, x, y, yhi, psym=8, /hibar, /nohat

;    ; overplot upper bound arrows
;    IF n_elements(upperx) GT 0 THEN BEGIN
;        plotsym, 2, psize+1, /fill, thick=2
;        oplot, upperx, uppery, psym=8
;    ENDIF

    ; overplot the wavg for the whole sample
    wx = findgen(100)
    wy = replicate(allwav,100)
    oplot, wx, wy, linestyle=3

    ; draw a legend
    legend, items, linestyle=linearr, psym=psyarr, charsize=csize, $
            /top, box=0, /right_legend

    ; increment counters
    i++
    m++
ENDFOR

; horz label
xtex = textoidl('Redshift')
xyouts, 0.52, 0.12, xtex, /normal, charsize=csize+0.5*csize

; vert label
ytex = textoidl('Z/Z'+sunsymbol())
xyouts, 0.15, 0.5, ytex, /normal, orientation=90, charsize=csize+0.5*csize

device,/close

END

