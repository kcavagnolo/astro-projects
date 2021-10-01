pro fezcount

mpcol = 2
mprow = 4
output = 'fezcount.eps'
goal = 30
csize = 0.6
psize = 0.4
xmin = 0.0
xmax = 1.32
ymin = 0.
ymax = 0.45

; input files array
push, pstitle, textoidl('R_{2500}-CORE')
push, pstitle, textoidl('R_{2500}')
push, pstitle, textoidl('R_{5000}-CORE')
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
    ord = sort(x)
    x = x[ord]
    y = full.fe
    y = y[ord]
    ylo = full.fe - full.felo
    ylo = ylo[ord]
    yhi = full.fehi - full.fe
    yhi = yhi[ord]

    ; get wavg for full sample
    IF m GT 0 THEN void, allwg
    FOR kk=0,n_elements(y)-1 DO BEGIN
        IF yhi[kk] GT ylo[kk] THEN push,allwg,1/(yhi[kk])^2. ELSE push,allwg,1/(ylo[kk])^2.
    ENDFOR
    allwav = (total(allwg*y))/total(allwg)
    allsigwav = (1./sqrt(total(allwg)))

    count = 0
    total = 0
    IF (m GT 0) THEN BEGIN
        void, xwav
        void, ywav
        void, ysigwav
        void, xlo
        void, xhi
    ENDIF
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
            IF (total EQ n_elements(x)-1) THEN print, pstitle[j]," last bin ", num2str(n_elements(xtemp),3)
            void, xtemp
            void, ytemp
            void, yweight
            count = 0
        ENDIF
        total++
    ENDWHILE

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

    ; draw a legend
    items = [pstitle[j]]
    linearr = replicate(-99,n_elements(items))
    psyarr = replicate(-99,n_elements(items))
    legend, items, linestyle=linearr, psym=psyarr, charsize=csize, $
            /top, box=0, /right_legend

    ; plot the full points
    plotsym, 0, psize-0.25*psize, /fill
    multiplot & plot, x, y, $
          /xsty, /ysty, $
          psym=8, $
          xran = [xmin,xmax], $
          yran = [0.,1.0], $
          charsize = csize

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
xyouts, 0.52, 0.1, xtex, /normal, charsize=csize

; vert label
ytex = textoidl('Z/Z'+sunsymbol())
xyouts, 0.12, 0.52, ytex, /normal, orientation=90, charsize=csize

device,/close

END

