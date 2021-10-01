PRO fak_z

!quiet=1
dat1 = '../me_fits/dat/fak_final_7-7.dat'
dat2 = '../me_fits/dat/fak_final_2-7.dat'
dat3 = '../me_fits/dat/fak_control_7-7.dat'
dat4 = '../me_fits/dat/fak_control_2-7.dat'
csize = 0.8
space = 0.6
psize = 0.25
xmin = 0.04
xmax = 1.8
ymin = 0.5
ymax = 2.5
etamin = 0.8
etamax = 0.999
tx2min = 0.5
tx2max = 1.0
goal = 100
output = "fak_z.eps"

; restore the fit template and read some variables
restore,"/Users/cavagnolo/research/redux/scripts/xspectemp_rin_normerr_src.sav"
full  = read_ascii(dat1, template = xspectemp_rin_normerr_src)
hard  = read_ascii(dat2, template = xspectemp_rin_normerr_src)
fullc = read_ascii(dat3, template = xspectemp_rin_normerr_src)
hardc = read_ascii(dat4, template = xspectemp_rin_normerr_src)

check = full.obsid/hard.obsid
uhoh = where(check NE 1)
IF uhoh NE -1 THEN BEGIN
    print,'## ERROR: OUT OF ORDER FILE: '+dat1+' '+dat2
    exit
ENDIF

check = fullc.obsid/hardc.obsid
uhoh = where(check NE 1)
IF uhoh NE -1 THEN BEGIN
    print,'## ERROR: OUT OF ORDER FILE: '+dat3+' '+dat4
    exit
ENDIF

; store the hi and lo errors in variables
names  = full.cluster
herrlo = hard.tx - hard.tlo
herrhi = hard.thi - hard.tx
ferrlo = full.tx - full.tlo
ferrhi = full.thi - full.tx

etacrit = etamin
mineta = etamin-0.001
maxeta = etamin+0.001
aa = 0
WHILE etacrit LE etamax DO BEGIN
    IF aa GT 0 THEN BEGIN
        void, allx
        void, ally
        void, allyhi
        void, allylo
        void, xwav
        void, ywav
        void, ysigwav
        void, xlo
        void, xhi
    ENDIF

    ; build the empty plot
    odevice = !d.name
    IF aa EQ 0 THEN BEGIN
        set_plot, 'PS'
        device, filename = output, /encapsulated, /color;, /landscape
        !fancy = 4
        !linetype = 0
        !p.font = 0
        multiplot,[3,3]
        dumx = [0.,2000]
        dumy = dumx
    ENDIF
    IF (aa NE 0) THEN multiplot
    plot, $
      dumx, dumy, $
      /xlog, $
      /nodata, $
      linestyle = 0, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      /xsty, /ysty, $
      symsize = psize, $
      charsize = csize

    ; overplot the line of equality x=y
    x = findgen(xmax*5)+xmin
    y = replicate(1.0,n_elements(x))
    oplot, x, y, linestyle=2, psym=0, thick=2.0

    ; draw the legend for plot symbols
    etal = textoidl('\xi: '+strcompress(sigfig(etacrit,2),/remove_all))
    items = [etal]
    linearr = replicate(-99,n_elements(items))
    psyarr = replicate(-99,n_elements(items))
    legend, items, linestyle=linearr, psym=psyarr, spacing=space, charsize=csize, /top, /right, box=0
    
    ; draw the legend for T/Tbf colors
    loadct, 13
    colors = [50,150,250]
;    legcolor = (col^2.)*!d.n_colors
;    items = ['0.5 keV','0.75 keV','1.0 keV']
;    legend, items, psym=6, color=legcolor, spacing=space, charsize=csize, /top, /right, /fill, box=0

    ; loop through each value in the tx array
    m = 0
    tx2crit = tx2min
    mintx2 = tx2min-0.001
    maxtx2 = tx2min+0.001
    WHILE tx2crit LE tx2max DO BEGIN
        void,fulltx
        void,fullhi
        void,fulllo
        void,hardtx
        void,hardhi
        void,hardlo
        void,z

        i = 0 
        FOR i=0,n_elements(full.cluster)-1 DO BEGIN
            IF (full.rout[i] LT maxeta AND $
                full.rout[i] GT mineta AND $
                full.rin[i] LT maxtx2 AND $
                full.rin[i] GT mintx2) THEN BEGIN

                ; push values into array
                push, z, full.z[i]
                push, fulltx, full.tx[i]
                push, hardtx, hard.tx[i]
                push, fullhi, full.thi[i]-full.tx[i]
                push, fulllo, full.tx[i]-full.tlo[i]
                push, hardhi, hard.thi[i]-hard.tx[i]
                push, hardlo, hard.tx[i]-hard.tlo[i]
            ENDIF
        ENDFOR

        ; overplot the color coded values
        x    = z
        y    = hardtx/fulltx
        yhi  = hardtx/fulltx*(sqrt((hardhi/hardtx)^2.+(fullhi/fulltx)^2.))
        ylo  = hardtx/fulltx*(sqrt((hardlo/hardtx)^2.+(fulllo/fulltx)^2.))
        push, allx, x
        push, ally, y
        push, allyhi, yhi
        push, allylo, ylo
        plotsym, 0, /fill
        oplot, x, y, psym=8, symsize=psize, color=colors[m]

        ; increment the counters and continue on
        m++
        tx2crit = tx2crit+0.25
        mintx2 = mintx2+0.25
        maxtx2 = maxtx2+0.25
    ENDWHILE

    ; calculate the weighted average and associated error for bins
    mm = 0
    count = 0
    total = 0
    ord = sort(allx)
    allx = allx[ord]
    ally = ally[ord]
    allyhi = allyhi[ord]
    allylo = allylo[ord]
    WHILE (total NE n_elements(allx)) DO BEGIN
        IF (count LT goal) THEN BEGIN
            push, xtemp, allx[total]
            push, ytemp, ally[total]
            IF allyhi[total] GT allylo[total] THEN push,yweight,1/(allyhi[total])^2. ELSE push,yweight,1/(allylo[total])^2.
            count++
        ENDIF
        IF ((count EQ goal) OR (total EQ n_elements(allx)-1)) THEN BEGIN
            push, ywav, (total(yweight*ytemp))/total(yweight)
            push, xwav, median(xtemp)
            push, ysigwav, (1./sqrt(total(yweight)))
            push, xlo, median(xtemp)-min(xtemp)
            push, xhi, max(xtemp)-median(xtemp)
            IF (total EQ n_elements(allx)-1) THEN print, "last bin has ", num2str(n_elements(xtemp),3)
            void, xtemp
            void, ytemp
            void, yweight
            count = 0
        ENDIF
        total++
    ENDWHILE



    ; overplot the weighted average, errors, and bin demarcation
    plotsym, 0, /fill
    !linetype = 0
    oplot, xwav, ywav, psym=8

    ; get wavg for full sample
    void, allwg
    FOR kk=0,n_elements(ally)-1 DO BEGIN
        IF allyhi[kk] GT allylo[kk] THEN push,allwg,1/(allyhi[kk])^2. ELSE push,allwg,1/(allylo[kk])^2.
    ENDFOR
    allwav = (total(allwg*ally))/total(allwg)
    allsigwav = (1./sqrt(total(allwg)))
    print, ""
    print, "Eta ",num2str(etacrit,3)
    print, "Wavg ",num2str(allwav,3)," +/- ",num2str(allsigwav,4)
    print, ""


; overplot the errors
; too small to care
;    oploterror, xwav, ywav, ysigwav, psym=8, /lobar
;    oploterror, xwav, ywav, ysigwav, psym=8, /hibar
;    oploterror, xwav, ywav, xlo, ysigwav, psym=8, /lobar
;    oploterror, xwav, ywav, xhi, ysigwav, psym=8, /hibar

    ; move the counters
    IF (etacrit LT 0.80) THEN etastep = 0.10
    IF (etacrit GE 0.80 AND etacrit LT 0.95) THEN etastep = 0.05
    IF (etacrit GE 0.95) THEN etastep = 0.01
    etacrit = etacrit + etastep
    mineta  = mineta + etastep
    maxeta  = maxeta + etastep
    aa++

ENDWHILE

; store the hi and lo errors in variables
hardlo = hardc.tx - hardc.tlo
hardhi = hardc.thi - hardc.tx
fulllo = fullc.tx - fullc.tlo
fullhi = fullc.thi - fullc.tx
fulltx = fullc.tx
hardtx = hardc.tx
x   = fullc.z
y   = hardtx/fulltx
yhi = hardtx/fulltx*(sqrt((hardhi/hardtx)^2.+(fullhi/fulltx)^2.))
ylo = hardtx/fulltx*(sqrt((hardlo/hardtx)^2.+(fulllo/fulltx)^2.))

; do some quick stats
ngt1 = float(n_elements(where(ally-allylo GT  1.0)))
per = (ngt1/n_elements(ally))*100.
print, "Num. sims w/ Tf > 1.0 @ 1-sigma: ",num2str(ngt1)
print, "Freq: ",num2str(per,4),"%"
ngt1 = float(n_elements(where(y-ylo GT  1.0)))
per = (ngt1/n_elements(y))*100.
print, "Num. control w/ Tf > 1.0 @ 1-sigma: ",num2str(ngt1)
print, "Freq: ",num2str(per,4),"%"

; get wavg for full sample
void, allwg
FOR kk=0,n_elements(y)-1 DO BEGIN
    IF yhi[kk] GT ylo[kk] THEN push,allwg,1/(yhi[kk])^2. ELSE push,allwg,1/(ylo[kk])^2.
ENDFOR
allwav = (total(allwg*y))/total(allwg)
allsigwav = (1./sqrt(total(allwg)))
print, ""
print, "Eta 0.000 *CONTROL*"
print, "Wavg ",num2str(allwav,3)," +/- ",num2str(allsigwav,4)
print, ""

plotsym, 0, /fill
multiplot & plot, $
  x, y, $
  psym=8, $
  linestyle = 0, $
  /xlog, $
  xrange = [xmin,xmax], $
  yrange = [ymin,ymax], $
  /xsty, /ysty, $
  symsize = psize, $
  charsize = csize

; overplot the line of equality x=y
x = findgen(xmax*5)+xmin
y = replicate(1.0,n_elements(x))
oplot, x, y, linestyle=2, psym=0, thick=2.0

; draw the legend for plot symbols
items = [textoidl('Control Sample')]
linearr = replicate(-99,n_elements(items))
psyarr = replicate(-99,n_elements(items))
legend, items, linestyle=linearr, psym=psyarr, spacing=space, charsize=csize, /top, /left_legend, box=0

; horz label
xtex = textoidl('Redshift')
xyouts, 0.50, 0.05, xtex, /normal, charsize=csize

; vert label
ytex = textoidl('T_{HFR}')
xyouts, 0.11, 0.50, ytex, /normal, orientation=90, charsize=csize

device, /close
set_plot, odevice
!quiet=0

END
