PRO fak_fe

!quiet = 1
in1 = '../me_fits/dat/fak_final_7-7.dat'
output = 'fak_fe.eps'
goal = 150
csize = 0.7
space = 0.6
psize = 0.3
femin = 0.01
femax = 2.0
xmin = 0.01
xmax = 2.
ymin = 0.05
ymax = 2.05
etamin = 0.8
etamax = 0.999
tx2min = 0.5
tx2max = 1.0

; restore the fit template and read some variables
restore,"~/research/redux/scripts/xspectemp_rin_normerr_src.sav"
dat1 = read_ascii(in1, template = xspectemp_rin_normerr_src)

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
    ENDIF

    ; build the empty plot
    odevice = !d.name
    IF aa EQ 0 THEN BEGIN
        set_plot, 'PS'
        device, filename = output, /encapsulated, /color, /landscape
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
      /xlog, /ylog, $
      /nodata, $
      linestyle = 0, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      /xsty, /ysty, $
      symsize = psize, $
      charsize = csize

    ; draw the legend for plot symbols
    etal = textoidl('\xi: '+strcompress(sigfig(etacrit,2),/remove_all))
    items = [etal]
    linearr = replicate(-99,n_elements(items))
    psyarr = replicate(-99,n_elements(items))
    legend, items, linestyle=linearr, psym=psyarr, spacing=space, charsize=csize, /top, /left_legend, box=0
    
    ; draw the legend for T/Tbf colors
    loadct, 13
    col = [0.5,0.75,1.0]
    legcolor = (col^2.)*!d.n_colors
    items = ['0.5 keV','0.75 keV','1.0 keV']
    legend, items, psym=6, color=legcolor, spacing=space, charsize=csize, /bottom, /right, /fill, box=0

    ; loop through each value in the tx array
    m = 0
    tx2crit = tx2min
    mintx2 = tx2min-0.001
    maxtx2 = tx2min+0.001
    WHILE tx2crit LE tx2max DO BEGIN
        IF (m GT 0) THEN BEGIN
            void,x
            void,y
            void,ylo
            void,yhi
        ENDIF
        i = 0 
        FOR i=0,n_elements(dat1.cluster)-1 DO BEGIN
            IF (dat1.rout[i] LT maxeta AND $
                dat1.rout[i] GT mineta AND $
                dat1.rin[i] LT maxtx2 AND $
                dat1.rin[i] GT mintx2) THEN BEGIN

                IF (dat1.fe[i] LT femax AND dat1.fe[i] GT femin) THEN BEGIN
                    push, x, dat1.z[i]
                    push, y, dat1.fe[i]
                    push, ylo, dat1.fe[i]-dat1.felo[i]
                    push, yhi, dat1.fehi[i]-dat1.fe[i]
                ENDIF
            ENDIF
        ENDFOR

        ; build a master array
        plcolor = (tx2crit^2.)*!d.n_colors
        push, allx, x
        push, ally, y
        push, allylo, ylo
        push, allyhi, yhi
        oplot, x, y, psym=2, symsize=psize, color=plcolor

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

    IF (etacrit LT 0.80) THEN etastep = 0.10
    IF (etacrit GE 0.80 AND etacrit LT 0.95) THEN etastep = 0.05
    IF (etacrit GE 0.95) THEN etastep = 0.01
    etacrit = etacrit + etastep
    mineta  = mineta + etastep
    maxeta  = maxeta + etastep
    aa++
ENDWHILE

; plot the control sample
dat1 = read_ascii('../me_fits/dat/fak_control_7-7.dat', template = xspectemp_rin_normerr_src)

void,x
void,y
void,ylo
void,yhi
FOR i=0,n_elements(dat1.cluster)-1 DO BEGIN
    IF (dat1.fe[i] LT femax AND dat1.fe[i] GT femin) THEN BEGIN
        push, y, dat1.fe[i]
        push, x, dat1.z[i]
        push, ylo, dat1.fe[i]-dat1.felo[i]
        push, yhi, dat1.fehi[i]-dat1.fe[i]
    ENDIF
ENDFOR

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

multiplot & plot, $
  x, y, $
  psym=2, $
  /xlog, /ylog, $
  linestyle = 0, $
  xrange = [xmin,xmax], $
  yrange = [ymin,ymax], $
  xstyle = 1, $
  ystyle = 1, $
  symsize = psize, $
  charsize = csize

xtex = textoidl('Redshift')
ytex = 'Z/Z'+sunsymbol()

; draw the legend for plot symbols
etal = textoidl('\xi: '+strcompress(sigfig(etacrit,2),/remove_all))
items = [etal]
linearr = replicate(-99,n_elements(items))
psyarr = replicate(-99,n_elements(items))
legend, items, linestyle=linearr, psym=psyarr, spacing=space, charsize=csize, /top, /left_legend, box=0

; labels
xyouts, 0.50, 0.05, xtex, /normal, charsize=0.9
xyouts, 0.13, 0.50, ytex, /normal, orientation=90, charsize=0.9

device, /close
set_plot, odevice
!quiet=0

END
