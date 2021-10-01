pro ft_histo

output = 'ft_histo.eps'
mpcol = 1
mprow = 4
csize = 0.8
peak = 1.0
binsize = 0.075
xmin = 0.5
xmax = 2.0
ymin = 0.
ymax = peak+0.1

; load the data files
restore,"~/research/redux/scripts/xspectemp_rin_normerr_src.sav"
push, dat, '../me_fits/dat/fak_control_7-7.dat'
push, dat, '../me_fits/dat/fak_control_2-7.dat'
push, dat, '../me_fits/dat/fak_final_7-7.dat'
push, dat, '../me_fits/dat/fak_final_2-7.dat'
push, dat, '../me_fits/dat/culled_r2500-50_7-7.dat'
push, dat, '../me_fits/dat/culled_r2500-50_2-7.dat'
push, dat, '../me_fits/dat/culled_r5000-50_7-7.dat'
push, dat, '../me_fits/dat/culled_r5000-50_2-7.dat'

; read the data files
i = 0
WHILE i LT n_elements(dat) DO BEGIN
    full = read_ascii(dat[i], template = xspectemp_rin_normerr_src)
    hard = read_ascii(dat[i+1], template = xspectemp_rin_normerr_src)
    IF i EQ 2 THEN BEGIN
        tx2 = full.rin
        eta = full.rout
        filter = where(eta LT 0.96 AND eta GT 0.94 AND tx2 LT 0.76 AND tx2 GT 0.74)
        fulltx = full.tx[filter]
        hardtx = hard.tx[filter]
    ENDIF ELSE BEGIN
        fulltx = full.tx
        hardtx = hard.tx
    ENDELSE
    push, ftx, ptr_new(hardtx/fulltx)
    i = i+2
ENDWHILE

; make some histograms
odevice = !d.name
set_plot,'PS'
device, filename = output, /encapsulated
!fancy = 4
!linetype = 0
!p.font = 0
multiplot, [mpcol,mprow]
legtex = [textoidl('Control'),$
          textoidl('Simulated \xi=0.95, T_{2}=0.75 keV'),$
          textoidl('Real R_{2500-CORE}'),$
          textoidl('Real R_{5000-CORE}')]
FOR ii=0,n_elements(ftx)-1 DO BEGIN
    hist = *ftx[ii]
    IF (ii NE 0) THEN multiplot
    plothist, hist, $
              fx, fy, $
              bin=binsize, $
              peak=peak, $
              xran=[xmin,xmax], $
              yran=[ymin,ymax], $
              yminor = 1, $
              /ysty,/xsty, charsize=csize-0.2*csize

    ; draw a legend
    legend, legtex[ii], /top, box=0, /right_legend, charsize=csize
ENDFOR

; cumulative
;all = [*ftx[2],*ftx[3],*ftx[4],*ftx[5]]
;multiplot & plothist, all, $
;          fx, fy, $
;          bin=0.025, $
;          peak=peak, $
;          xran=[xmin,xmax], $
;          yran=[ymin,ymax], $
;          yminor = 1, $
;          /ysty,/xsty, charsize=csize-0.2*csize
;; draw a legend
;legend, legtex[n_elements(legtex)-1], /top, box=0, /right_legend, charsize=csize

; horz label
xtex = textoidl('T_{HFR}')
xyouts, 0.52, 0.12, xtex, /normal, charsize=csize

; vert label
ytex = textoidl('Normalized Number of Clusters Per Bin')
xyouts, 0.14, 0.3, ytex, /normal, orientation=90, charsize=csize

device, /close
set_plot, odevice

END
