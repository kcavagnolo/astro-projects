pro fetxbin

prefix = 'culled'
mpcol = 2
mprow = 4
output = 'fetxbin.ps'
csize = 0.6
psize = 0.4
upper = 1.2
bsize = 1.0
xmin = 1.
xmax = 18.
ymin = 0.28
ymax = 0.48

; input files array
files = strarr(4)
pstitle = strarr(4)

pstitle[0] = textoidl('r_{2500}-70')
pstitle[1] = textoidl('r_{2500}')
pstitle[2] = textoidl('r_{5000}-70')
pstitle[3] = textoidl('r_{5000}')

files[0] = '../me_fits/dat/'+prefix+'_r2500-50_7-7.dat'
files[1] = '../me_fits/dat/'+prefix+'_r2500_7-7.dat'
files[2] = '../me_fits/dat/'+prefix+'_r5000-50_7-7.dat'
files[3] = '../me_fits/dat/'+prefix+'_r5000_7-7.dat'

;files[0] = '../me_fits/dat/final_r2500-50_fefree_7-7.dat'
;files[1] = '../me_fits/dat/final_r2500_fefree_7-7.dat'
;files[2] = '../me_fits/dat/final_r5000-50_fefree_7-7.dat'
;files[3] = '../me_fits/dat/final_r5000_fefree_7-7.dat'

i = 0
m = 0
FOR j = 0,n_elements(pstitle)-1 DO BEGIN

    restore,"../scripts/xspectemp_rin_normerr_src.sav"        
    full = read_ascii(files[i], template = xspectemp_rin_normerr_src)
    x = full.tx
    y = full.fe
    ylo = full.fe - full.felo
    yhi = full.fehi - full.fe

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
        void, xempty
        void, xloempty
        void, xhiempty
        void, yempty
        void, yloempty
        void, yhiempty
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
        IF binmax LT 10. THEN binsize=bsize ELSE binsize=10.
        binmin = binmax
        binmax = binmax+binsize
    ENDFOR

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

    ; overplot the errors
    oploterror, xwav, ywav, xlo, ysigwav, psym=8, /lobar 
    oploterror, xwav, ywav, xhi, ysigwav, psym=8, /hibar

    ; overplot the empty bins
    plotsym, 0, psize
    oplot, xempty, yempty, psym=8
    oploterror, xempty, yempty, xloempty, yloempty, psym=8, /lobar
    oploterror, xempty, yempty, xhiempty, yhiempty, psym=8, /hibar

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
          yran = [0.,1.], $
          charsize = csize
    oploterror, x, y, ylo, psym=8, /lobar, /nohat
    oploterror, x, y, yhi, psym=8, /hibar, /nohat

    ; overplot upper bound arrows
    plotsym, 2, psize+1, /fill, thick=2
    IF n_elements(upperx) GT 0 THEN $
      oplot, upperx, uppery, psym=8

    ; draw a legend
    legend, items, linestyle=linearr, psym=psyarr, charsize=csize, $
            /top, box=0, /right_legend

    ; increment counters
    i++
    m++
ENDFOR

; horz label
xtex = textoidl('T_{0.7-7.0} [keV]')
xyouts, 0.52, 0.1, xtex, /normal, charsize=csize

; vert label
ytex = textoidl('Z/Z'+sunsymbol())
xyouts, 0.12, 0.52, ytex, /normal, orientation=90, charsize=csize

device,/close

SPAWN, 'ps2ps temp.ps '+output
file_delete, 'temp.ps', /quiet

END

