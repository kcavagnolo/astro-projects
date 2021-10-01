pro fak_sys, dat1, dat2
; fak_sys,'dat/fak_final_2-7.dat','dat/fak_final_7-7.dat'
; fak_sys,'dat/fak_robs-70_nhfro_fefree_7-7.dat','dat/fak_robs-70_nhfro_fefree_2-7.dat'
; fak_sys,'fak_robs-70_nhfro_fefro_7-7.dat','fak_robs-70_nhfro_fefro_2-7.dat'
; fak_sys,'fak_control_nhfro_fefree_7-7.dat','fak_control_nhfro_fefree_2-7.dat'

psnum = '1'
tmin = 0.
tmax = 2.
etamin = 0.8
etamax = 0.999
tx2min = 0.5
tx2max = 1.0

; restore the fit template and read some variables
restore,"/Users/cavagnolo/research/redux/scripts/xspectemp_rin_normerr_src.sav"
sev = read_ascii(dat1, template = xspectemp_rin_normerr_src)
two = read_ascii(dat2, template = xspectemp_rin_normerr_src)

; build color arrays
red = REFORM([255,0,0],1,3)
TVLCT, red, 100
blue = REFORM([0,0,255],1,3)
TVLCT, blue, 300

etacrit = etamin
mineta = etamin-0.001
maxeta = etamin+0.001
WHILE etacrit LE etamax DO BEGIN

    ; build the empty plot
    tit = textoidl('\eta: '+strcompress(etacrit,/remove_all)+'')
    xtx = textoidl('[2.0-7.0]keV band')
    ytx = textoidl('[0.7-7.0]keV band')
    set_plot, 'PS'
    device, filename = 'temp_'+strcompress(etacrit,/remove_all)+'.ps', /color
    !fancy = 4
    !linetype = 0
    !p.font = 0
    dumx = [0.,2000]
    dumy = dumx
    plot, $
      dumx, $
      dumy, $
      /nodata, $
      linestyle = 0, $
      xrange = [tmin,tmax], $
      yrange = [tmin,tmax], $
      xstyle = 1, $
      ystyle = 1, $
      title  = tit, $
      xtitle = xtx, $
      ytitle = ytx, $
      charsize = 0.75

    m = 0
    tx2crit = tx2min
    mintx2 = tx2min-0.001
    maxtx2 = tx2min+0.001
    WHILE tx2crit LE tx2max DO BEGIN
        IF (m GT 0) THEN BEGIN
            void,sevnormbf
            void,twonormbf
            void,sevtxbf
            void,twotxbf
        ENDIF
        i = 0 
        FOR i=0,n_elements(sev.cluster)-1 DO BEGIN
            IF (sev.rout[i] LT maxeta AND $
                sev.rout[i] GT mineta AND $
                sev.rin[i] LT maxtx2 AND $
                sev.rin[i] GT mintx2) THEN BEGIN

                push,sevnormbf,sev.norm2[i]
                push,twonormbf,two.norm2[i]
                push,sevtxbf,sev.tx2[i]
                push,twotxbf,two.tx2[i]
            ENDIF
        ENDFOR

        ; build color arrays
        loadct, 13
        plcolor = (tx2crit^2.)*!d.n_colors

        ; make a legend for the colors
        push,legcolor,plcolor

        ; overplot the color coded values
        oplot, twonormbf, sevnormbf, psym=5, symsize=0.3, color=plcolor/2.
        oplot, twotxbf, sevtxbf, psym=2, symsize=0.3, color=plcolor

        ; increment the counters and continue on
        m++
        tx2crit = tx2crit+0.25
        mintx2 = mintx2+0.25
        maxtx2 = maxtx2+0.25
    ENDWHILE
    
    x = findgen(tmax+10)
    y = x
    yvert = findgen(tmax+10)
    xvert = replicate(1,n_elements(yvert))
    xhorz = findgen(tmax+10)
    yhorz = replicate(1,n_elements(yvert))
    yv20 = findgen(tmax+10)
    xv20 = replicate(1.2,n_elements(yvert))
    xh20 = findgen(tmax+10)
    yh20 = replicate(1.2,n_elements(yvert))
    yv80 = findgen(tmax+10)
    xv80 = replicate(0.8,n_elements(yvert))
    xh80 = findgen(tmax+10)
    yh80 = replicate(0.8,n_elements(yvert))
    oplot, x, y, psym=0, linestyle=0
    oplot, xvert, yvert, psym=0, linestyle=0
    oplot, xhorz, yhorz, psym=0, linestyle=0
    oplot, xv20, yv20, psym=0, linestyle=2
    oplot, xv80, yv80, psym=0, linestyle=2
    oplot, xh20, yh20, psym=0, linestyle=3
    oplot, xh80, yh80, psym=0, linestyle=3

    ; draw the legend for plot symbols
    eta = textoidl('norm/norm_{best-fit}')
    tbf = textoidl('T/T_{best-fit}')
    items = [eta, tbf]
    linearr = [0, 0]
    psyarr = [5, 6]
    legend, items, linestyle=linearr, psym=psyarr, charsize=0.75, /top, /left_legend
    
    ; draw the legend for T/Tbf colors
    items = ['0.5keV','0.75keV','1.0keV']
    legend, items, psym=6, color=legcolor, charsize=0.75, /bottom, /right, /fill

    ; draw the legend for norm/normbf colors
    items = ['0.5keV','0.75keV','1.0keV']
    legend, items, psym=6, color=legcolor/2., charsize=0.75, /top, /right, /fill

    device, /close

    IF (etacrit LT 0.80) THEN etastep = 0.10
    IF (etacrit GE 0.80 AND etacrit LT 0.95) THEN etastep = 0.05
    IF (etacrit GE 0.95) THEN etastep = 0.01
    etacrit = etacrit + etastep
    mineta  = mineta + etastep
    maxeta  = maxeta + etastep
ENDWHILE

; plot the control sample
a = read_ascii('dat/fak_control_7-7.dat', template = xspectemp_rin_normerr_src)
b = read_ascii('dat/fak_control_2-7.dat', template = xspectemp_rin_normerr_src)

tit = textoidl('Control Sample; \eta=1.0; no T_{2}')
xtx = textoidl('[2.0-7.0]keV band')
ytx = textoidl('[0.7-7.0]keV band')
set_plot, 'PS'
device, filename = 'temp_'+strcompress(etacrit+2,/remove_all)+'.ps', /color
!fancy = 4
!linetype = 0
!p.font = 0
dumx = [0.,2000]
dumy = dumx
plot, $
  dumx, $
  dumy, $
  /nodata, $
  linestyle = 0, $
  xrange = [tmin,tmax], $
  yrange = [tmin,tmax], $
  xstyle = 1, $
  ystyle = 1, $
  title  = tit, $
  xtitle = xtx, $
  ytitle = ytx, $
  charsize = 0.75

oplot, b.tx2, a.tx2, psym=5, symsize=0.3, color=100
oplot, b.norm2, a.norm2, psym=2, symsize=0.3, color=300
oplot, xvert, yvert, psym=0, linestyle=0
oplot, xhorz, yhorz, psym=0, linestyle=0
oplot, xv20, yv20, psym=0, linestyle=2
oplot, xv80, yv80, psym=0, linestyle=2
oplot, xh20, yh20, psym=0, linestyle=3
oplot, xh80, yh80, psym=0, linestyle=3

; draw the legend
eta = textoidl('norm/norm_{best-fit}')
tbf = textoidl('T/T_{best-fit}')
items = [eta, tbf]
linearr = [0, 0]
psyarr = [5, 6]
legend, items, linestyle=linearr, psym=psyarr, charsize=0.75, /top, /left_legend

device, /close

; make all these ps files into one ps file
SPAWN, 'ls temp_*.ps > list'
SPAWN, 'cat list | perl /Users/cavagnolo/research/redux/scripts/pscat.pl '+psnum+' sys_fak.ps'
SPAWN, 'rm -f temp*.ps'
SPAWN, 'rm -f list'

END

