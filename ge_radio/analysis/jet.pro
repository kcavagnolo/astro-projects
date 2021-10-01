PRO jet

;# external bces fit params
birzana = 0.35                  ;# from the 08 paper
birzanb = 1.85
;birzana = 0.746                 ;# from my bces fitting
;birzanb = 2.02
yxbcesa = 0.575
yxbcesb = 1.87
xybcesa = 1.05
xybcesb = 2.02
obcesa  = 0.720
obcesb  = 1.92

;# read files
file1 = '../mc_ge/target.list'
file2 = 'radlum_nvss_withz.dat'
file3 = 'cavity_radio_powers.dat'
file4 = 'croston_pcav.dat'
readcol, file1, format='A,D,D,D,D,D,D,D,D,D,D,A,A,A', comment='#', $
         pname, lx, ps, pslo, pshi, pb, pblo, pbhi, pf, pflo, pfhi, grade, gtype, dom
readcol, file2, format='A,A,A,F,F,F,F,F,F,F,A', comment='#', $
         rname, obs, rtype, z, flux, ferr, power, perr, lum, lerr, surv
readcol, file3, format='A,F,F,F,F,F,F,F', comment='#', $
         bname, bpcav, bplo, bphi, bl14, bl14err, bl3, bl3err
readcol, file4, format='A,F,F,F,D,D,D,D,D,D', comment='#', $
         cname, cz, cd, ckt, cecav, ctcs, cpcav, cperr, cl14, cl14err
cpcav = cpcav/1d42
cperr = cperr/1d42
cl14 = cl14/1d40
cl14err = cl14err/1d40

;# set plot defaults
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 2
!Y.THICK  = 2
!Z.THICK  = 2

;# convert paul's buoyancy numbers:
;# from 1pV -> 4pV
;# from watts -> erg/s
;# and scale by 10^42
ps = 4.0*ps*1d7/1d42
pslo = 4.0*pslo*1d7/1d42
pshi = 4.0*pshi*1d7/1d42
pb = 4.0*pb*1d7/1d42
pblo = 4.0*pblo*1d7/1d42
pbhi = 4.0*pbhi*1d7/1d42
pf = 4.0*pf*1d7/1d42
pflo = 4.0*pflo*1d7/1d42
pfhi = 4.0*pfhi*1d7/1d42

;# loop through each entry
FOR i=0,n_elements(pname)-1 DO BEGIN
   ord = where(rname EQ pname[i], num)
   IF num LT 1 THEN BEGIN
      print, 'ERROR: Oops, there is no ',pname[i],' in NVSS radio file ',file2
      GOTO, SKIP
   ENDIF
   IF gtype[i] EQ 'dw' THEN BEGIN
      GOTO, SKIP
      push, dwfx, power[ord]
      push, dwfxerr, perr[ord]
      push, dwfy, pb[i]
      push, dwfylo, pb[i]-pblo[i]
      push, dwfyhi, pbhi[i]-pb[i]
      GOTO, SKIP
   ENDIF
   IF dom[i] EQ 'jet' THEN BEGIN
      push, jetx, power[ord]
      push, jety, pb[i]
   ENDIF
   IF rtype[ord] EQ 'NF' THEN BEGIN
      push, nx, power[ord]
      push, ny, pb[i]
      push, nylo, pb[i]-pblo[i]
      push, nyhi, pbhi[i]-pb[i]
   ENDIF
   IF grade[i] EQ 'A' THEN BEGIN
      push, gradeax, power[ord]
      push, gradeaxerr, perr[ord]
      push, gradeay, pb[i]
      push, gradeaylo, pb[i]-pblo[i]
      push, gradeayhi, pbhi[i]-pb[i]
   ENDIF
   IF grade[i] EQ 'B' THEN BEGIN
      push, gradebx, power[ord]
      push, gradebxerr, perr[ord]
      push, gradeby, pb[i]
      push, gradebylo, pb[i]-pblo[i]
      push, gradebyhi, pbhi[i]-pb[i]
   ENDIF
   IF grade[i] EQ 'C' THEN BEGIN
      push, gradecx, power[ord]
      push, gradecxerr, perr[ord]
      push, gradecy, pb[i]
      push, gradecylo, pb[i]-pblo[i]
      push, gradecyhi, pbhi[i]-pb[i]
   ENDIF
   IF grade[i] EQ 'D' THEN BEGIN
      push, gradedx, power[ord]
      push, gradedxerr, perr[ord]
      push, gradedy, pb[i]
      push, gradedylo, pb[i]-pblo[i]
      push, gradedyhi, pbhi[i]-pb[i]
   ENDIF
SKIP:
ENDFOR

;# plot sur bri prof and nuker fit to own file
set_plot, 'PS'
loadct, 13, /silent
device, filename='jetted.eps', $
        /color, $
        /encapsulated, $
        /portrait, $
        set_font='Times-Roman', $
        bits=16
ytex = textoidl('P_{cav} [10^{42} erg s^{-1}]')
xtex = textoidl('L_{1.4} [10^{40} erg s^{-1}]')
xmin = 2d-1
xmax = 3d1
ymin = 2d-1
ymax = 3d1
x = maken(1,10,2)
y = x
plot, x, y, $
      /nodata, $
      /xlog, /ylog, $
      /xsty, /ysty, $
      ytitle = ytex, $
      xtitle = xtex, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      position= ASPECT(1.0), $
      psym = 8, $
      charsize = 1.0

;# external ortho best-fit bces relation
px = maken(1d-10,1d50,10)
py = (10^(obcesb))*(px^(obcesa))
oplot, px, py, psym=0, linestyle=2, thick=4, color=0

;# Birzan best-fit line
py = (10^birzanb)*(px^birzana)
oplot, px, py, psym=0, linestyle=1, thick=4, color=250

;# Heinz and Grimm line
py = 6.2d37*(px/1d30)^(0.704)
oplot, px/1d40, py/1d42, psym=0, linestyle=3, thick=4, color=150

;# plot 6/7
py = 3d38*((px/1d28)^(6./7.))/1d42*1d7
px = px*151d6*1d7*(4*!PI)/1d40
oplot, px, py, psym=0, linestyle=4, thick=4, color=50

;# plot upper-limits
IF n_elements(nx) EQ 1 THEN BEGIN
   push, nx, -1d20
   push, ny, -1d20
ENDIF
IF n_elements(nx) GT 1 THEN BEGIN
   plotsym, 6, 2.0
   oplot, nx, ny, psym=8, color=0
ENDIF ELSE print, '#ERROR: Not enough upper-limits to plot.'

;# plot grade D's
IF n_elements(gradedx) GT 1 THEN BEGIN
   plotsym, 0, 0.9, /fill
   oploterror, gradedx, gradedy, gradedxerr, gradedyhi, psym=8, /hibar, /nohat, color=0
   oploterror, gradedx, gradedy, gradedxerr, gradedylo, psym=8, /lobar, /nohat, color=0
   oplot, gradedx, gradedy, psym=8, color=0
ENDIF ELSE print, '#ERROR: Not enough grade-D to plot.'   

;# plot grade C's
IF n_elements(gradecx) GT 1 THEN BEGIN
   plotsym, 0, 0.9, /fill
   oploterror, gradecx, gradecy, gradecxerr, gradecyhi, psym=8, /hibar, /nohat, color=250
   oploterror, gradecx, gradecy, gradecxerr, gradecylo, psym=8, /lobar, /nohat, color=250
   oplot, gradecx, gradecy, psym=8, color=0
   plotsym, 0, 0.7, /fill
   oplot, gradecx, gradecy, psym=8, color=250
ENDIF ELSE print, '#ERROR: Not enough grade-C to plot.'   

;# plot grade B's
IF n_elements(gradebx) GT 1 THEN BEGIN
   plotsym, 0, 0.9, /fill
   oploterror, gradebx, gradeby, gradebxerr, gradebyhi, psym=8, /hibar, /nohat, color=50
   oploterror, gradebx, gradeby, gradebxerr, gradebylo, psym=8, /lobar, /nohat, color=50
   oplot, gradebx, gradeby, psym=8, color=0
   plotsym, 0, 0.7, /fill
   oplot, gradebx, gradeby, psym=8, color=50
ENDIF ELSE print, '#ERROR: Not enough grade-B to plot.'   

;# plot grade A's
IF n_elements(gradeax) GT 1 THEN BEGIN
   plotsym, 0, 0.9, /fill
   oploterror, gradeax, gradeay, gradeaxerr, gradeayhi, psym=8, /hibar, /nohat, color=150
   oploterror, gradeax, gradeay, gradeaxerr, gradeaylo, psym=8, /lobar, /nohat, color=150
   oplot, gradeax, gradeay, psym=8, color=0
   plotsym, 0, 0.7, /fill
   oplot, gradeax, gradeay, psym=8, color=150
ENDIF ELSE print, '#ERROR: Not enough grade-A to plot.'   

;# plot dwarfs
IF n_elements(dwfx) GT 1 THEN BEGIN
   plotsym, 8, 0.7, /fill
   oploterror, dwfx, dwfy, dwfxerr, dwfyhi, psym=8, /hibar, /nohat, color=0
   oploterror, dwfx, dwfy, dwfxerr, dwfylo, psym=8, /lobar, /nohat, color=0
   oplot, dwfx, dwfy, psym=8, color=0
ENDIF ELSE print, '#ERROR: Not enough dwarfs to plot.'

;# plot birzan's numbers
plotsym, 4, 0.9, /fill
oploterror, bl14, bpcav, bl14err, bphi, psym=8, /hibar, /nohat, color=225
oploterror, bl14, bpcav, bl14err, bplo, psym=8, /lobar, /nohat, color=225
oplot, bl14, bpcav, psym=8, color=0
plotsym, 4, 0.7, /fill
oplot, bl14, bpcav, psym=8, color=225

;# plot croston's numbers
plotsym, 8, 0.9, /fill
oploterror, cl14, cpcav, 2.0*cl14err, 2.0*cperr, psym=8, color=25, /nohat
oplot, cl14, cpcav, psym=8, color=0
plotsym, 8, 0.7, /fill
oplot, cl14, cpcav, psym=8, color=25

;# add legend
push, items, textoidl('Croston et al. `08 FR-I sample')
push, items, textoidl('Jet dominated')
linearr = replicate(-99,n_elements(items))
psyarr = replicate(-99,n_elements(items))
legend, items, linestyle=larr, psym=parr, box=0, charsize=0.7, /bottom, /right

;# plot jet dominated systems
plotsym, 0, 1.3
oplot, jetx, jety, psym=8, color=0
x = [1d-20, 7.8]
y = [1d-20, 0.27]
plotsym, 0, 1.3
oplot, x, y, psym=8, color=0

;# croston legend
x = [1d-20, 2.6]
y = [1d-20, 0.32]
plotsym, 8, 0.9, /fill
oplot, x, y, psym=8, color=0
plotsym, 8, 0.7, /fill
oplot, x, y, psym=8, color=25

;# close device
device, /close

END
