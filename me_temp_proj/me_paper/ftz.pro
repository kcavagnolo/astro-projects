pro ftz

output = 'ftz.eps'
errpl = "yes"
csize = 0.7
space = 0.6
psize = 0.5
ymin = 0.2
ymax = 3.2
xmin = 0.01
xmax = 2.

; input files array
push, pstitle,textoidl('R_{2500-CORE}')
push, pstitle,textoidl('R_{2500}')
push, files,'../me_fits/dat/culled_r2500-50_7-7.dat'
push, files,'../me_fits/dat/culled_r2500-50_2-7.dat'
push, files,'../me_fits/dat/culled_r2500_7-7.dat'
push, files,'../me_fits/dat/culled_r2500_2-7.dat'

i = 0
m = 0
FOR j = 0,n_elements(pstitle)-1 DO BEGIN

    restore,"~/research/redux/scripts/xspectemp_rin_normerr_src.sav"        
    full = read_ascii(files[i], template = xspectemp_rin_normerr_src)
    hard = read_ascii(files[i+1], template = xspectemp_rin_normerr_src)

    check = full.obsid/hard.obsid
    uhoh = where(check NE 1)
    IF uhoh NE -1 THEN BEGIN
        print,'## ERROR: OUT OF ORDER FILE: '+dat1+' '+dat2
        exit
    ENDIF

    fulllo = full.tx - full.tlo
    fullhi = full.thi - full.tx
    hardlo = hard.tx - hard.tlo
    hardhi = hard.thi - hard.tx

    x = full.z
    y = hard.tx/full.tx
    name = full.cluster
    yhi = hard.tx/full.tx*(sqrt((hardhi/hard.tx)^2.+(fullhi/full.tx)^2.))
    ylo = hard.tx/full.tx*(sqrt((hardlo/hard.tx)^2.+(fulllo/full.tx)^2.))

    IF (m NE 0) THEN void,names
    FOR ii=0,n_elements(full.tx)-1 DO BEGIN
        IF ((y[ii] LT 0.8) OR (y[ii] GT 2.5)) THEN $
          push, names, name[ii] ELSE $
          push, names, ''
    ENDFOR

; make a hardcopy of plaw fits
    IF (m EQ 0) THEN BEGIN
        set_plot, 'PS'
        device, filename = output, /color, /encapsulated
        !fancy = 4
        !linetype = 0
        !p.font = 0
        plotsym, 0, psize, /fill
        multiplot,[1,2]
    ENDIF
    IF (m NE 0) THEN multiplot
    plot, x, y, $
          /xsty, /ysty, $
          psym = 8, $
          xrange = [xmin,xmax], $
          yrange = [ymin,ymax], $
          symsize = psize, $
          charsize = csize, $
          yticks = 5, $
          xticks = 5
    oplot,findgen(100),replicate(1,100)
    oploterror, x, y, ylo, psym=8, /lobar, /nohat
    oploterror, x, y, yhi, psym=8, /hibar, /nohat
    items = [pstitle[j]]
    linearr = replicate(-99,n_elements(items))
    psyarr = replicate(-99,n_elements(items))
    legend, items, linestyle=linearr, psym=psyarr, charsize=csize, $
            spacing=space, /top, box=0, /right_legend
    xyouts, x, y, names, charsize=0.5
    names = names[where(names NE '')]
    items = [names]
    linearr = replicate(-99,n_elements(items))
    psyarr = replicate(-99,n_elements(items))
    legend, items, linestyle=linearr, psym=psyarr, charsize=0.5, $
            spacing=space, /bottom, box=0, /right_legend
    i = i+2
    m++
ENDFOR
xtex = textoidl('Redshift')
ytex = textoidl('T_{HFR}')
; horz label
xyouts, 0.52, 0.1, xtex, /normal, charsize=csize
; vert label
xyouts, 0.12, 0.52, ytex, /normal, orientation=90, charsize=csize
device, /close
END
