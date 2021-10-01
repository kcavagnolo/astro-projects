PRO pvl

!QUIET=1
dojet = 'n'
plotlines = 'y'
croston = 'n'

;# external bces fit params
birzana = 0.35                  ;# from the 08 paper
birzanb = 1.85
yxbcesa = 0.597
yxbcesb = 1.86
xybcesa = 1.08
xybcesb = 2.00
obcesa  = 0.749
obcesb  = 1.91
;#############
;#############
;from my bces fitting
;birzana = 0.746
;birzanb = 2.02
;############# bad d_l
;############# bad d_l
;yxbcesa = 0.575
;yxbcesb = 1.87
;xybcesa = 1.05
;xybcesb = 2.02
;obcesa  = 0.720                 ;# slope
;obcesb  = 1.92                  ;# intercept
;############# bad d_l
;############# bad d_l

;# open file to amend table
OPENW, DATLOG, "p14table.tex", /GET_LUN
printf, DATLOG, '#P1.4 in 10^38 e/s, then 4pv/tbuoy=P_cav(buoy) in 10^42 e/s'

;# read files
file1 = '../mc_ge/target.list'
file2 = 'radlum_nvss_withz.dat'
file3 = 'cavity_radio_powers.dat'
file4 = 'croston_pcav.dat'
readcol, file1, format='A,D,D,D,D,D,D,D,D,D,D,A,A,A', comment='#', $
         pname, lx, ps, pslo, pshi, pb, pblo, pbhi, pf, pflo, pfhi, grade, gtype, dom
;readcol, file2, format='A,A,A,F,F,F,F,F,F,F,A', comment='#', $
;         rname, obs, rtype, z, flux, ferr, power, perr, lum, lerr, surv

;####################3
;####################3
;####################3
;####################3
readcol, 'fixed_nvss.dat', format='A,A,F,F', comment='#', $
         rname, rtype, power, perr
flux = 0.0*power
;####################3
;####################3
;####################3
;####################3

readcol, file3, format='A,F,F,F,F,F,F,F', comment='#', $
         bname, bpcav, bplo, bphi, bl14, bl14err, bl3, bl3err
IF croston EQ 'y' THEN BEGIN
   readcol, file4, format='A,F,F,F,D,D,D,D,D,D', comment='#', $
            cname, cz, cd, ckt, cecav, ctcs, cpcav, cperr, cl14, cl14err
   cpcav = cpcav/1d42
   cperr = cperr/1d42
   cl14 = cl14/1d40
   cl14err = cl14err/1d40
ENDIF

;# stats on gE fluxes
minf = min(flux)
maxf = max(flux)
medf = median(flux,/even)
meaf = mean(flux)
stdf = stddev(flux)
print, FORMAT='(A-15,F10.4,A6)','Flux Min: ',minf,'[Jy]'
print, FORMAT='(A-15,F10.4,A6)','Flux Max: ',maxf,'[Jy]'
print, FORMAT='(A-15,F10.4,A6)','Flux Median: ',medf,'[Jy]'
print, FORMAT='(A-15,F10.4,A6)','Flux Mean: ',meaf,'[Jy]'
print, FORMAT='(A-15,F10.4,A6)','Flux StdDev: ',stdf,'[Jy]'

;# set plot defaults
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 2
!Y.THICK  = 2
!Z.THICK  = 2

;# open a log file for BCESLOG
OPENW, BCESLOG, "bces_pvl.dat", /GET_LUN

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
      print, 'WARNING: Oops, there is no ',pname[i],' in NVSS radio file ',file2
      GOTO, SKIP
   ENDIF
   IF gtype[i] EQ 'dw' THEN BEGIN
      GOTO, SKIP
      push, dwfx, power[ord]
      push, dwfxerr, perr[ord]
      push, dwfy, pb[i]
      push, dwfylo, pb[i]-pblo[i]
      push, dwfyhi, pbhi[i]-pb[i]
   ENDIF
   printf, DATLOG, FORMAT='(A, A, F10.4, A, F10.4, A, A, A, F10.2, A, F10.2, A, F10.2, A)', $
           pname[i], '$', power[ord]*10.0, '\pm', perr[ord]*10.0, '$', '&', '$', pb[i], '^{+', pb[i]-pblo[i], '}_{-', pbhi[i]-pb[i], '}$'
   IF (dom[i] NE 'jet') THEN BEGIN 
      IF ((grade[i] EQ 'A') OR (grade[i] EQ 'B')) THEN BEGIN
         push, fx, power[ord]
         push, fxerr, perr[ord]
         push, fy, pb[i]
         push, fylo, pblo[i]
         push, fyhi, pbhi[i]
      ENDIF
   ENDIF
   IF dom[i] EQ 'jet' THEN BEGIN
      push, jetx, power[ord]
      push, jety, pb[i]
      GOTO, SKIP
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
free_lun, DATLOG

;# scatter in prad and pcav for ges and bcgs
print, FORMAT='(A-20,F10.4)','Stddev log B08 P_cav:', stddev(alog10(bpcav))
print, FORMAT='(A-20,F10.4)','Stddev log B08 P_rad:', stddev(alog10(bl14))
print, FORMAT='(A-20,F10.4)','Stddev log gE P_cav:', stddev(alog10(fy))
print, FORMAT='(A-20,F10.4)','Stddev log gE P_rad:', stddev(alog10(fx))

;# convert to log-space
fyhi = [alog10(fyhi/fy), alog10((bphi+bpcav)/bpcav)]
fylo = [alog10(fy/fylo), alog10(bpcav/(bpcav-bplo))]
fy = alog10([fy,bpcav])
fxhi = [alog10((fx+fxerr)/fx), alog10((bl14+bl14err)/bl14)]
fxlo = [alog10(fx/(fx-fxerr)), alog10(bl14/(bl14-bl14err))]
fx = alog10([fx,bl14])
print, FORMAT='(A-15,F10.4)','Mean log P_rad:', mean(fx)
print, FORMAT='(A-15,F10.4)','Mean log P_cav:', mean(fy)

;# plot the log space dist
set_plot, 'PS'
device, filename='log_1400.eps', $
        /encapsulated, $
        /portrait, $
        set_font='Times-Roman', $
        bits=16
ytex = textoidl('log(P_{cav} 10^{42} erg s^{-1})')
xtex = textoidl('log(P_{1.4} 10^{40} erg s^{-1})')
xmin = -5
xmax = 5
ymin = -2
ymax = 5
plot, fx, fy, $
      /xsty, /ysty, $
      title = '4pV & buoyant ages', $
      ytitle = ytex, $
      xtitle = xtex, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      position= ASPECT(1.0), $
      psym = 1, $
      charsize = 1.0
oploterror, fx, fy, fxhi, fyhi, psym=1, /hibar, /nohat
oploterror, fx, fy, fxlo, fylo, psym=1, /lobar, /nohat
x = maken(-10,10,10)
y = obcesa*x + obcesb           ;# ortho
oplot, x, y, linestyle=1
y = yxbcesa*x + yxbcesb         ;# (Y|X)
oplot, x, y, linestyle=2
y = xybcesa*x + xybcesb         ;# (X|Y)
oplot, x, y, linestyle=3
device, /close

;# fitting
FOR j=0,n_elements(fylo)-1 DO $
   IF fylo[j] GT fyhi[j] THEN push, fyerr, fylo[j] ELSE push, fyerr, fyhi[j]
void, fxerr
FOR j=0,n_elements(fxlo)-1 DO $
   IF fxlo[j] GT fxhi[j] THEN push, fxerr, fxlo[j] ELSE push, fxerr, fxhi[j]

;# make a log
FOR j=0,n_elements(fx)-1 DO $
   printf, BCESLOG, FORMAT='(5F10.4)', fx[j], fxerr[j], fy[j], fyerr[j], 0
FREE_LUN, BCESLOG

;# fitexy
print, ''
print, '## Fitting routines ##'
print, ''
fitexy, fx, fy, A, B, X_SIG=fxerr, Y_SIG=fyerr, ABerr
print, 'FITEXY:'
print, FORMAT='(A-15,F10.4,A6,F10.4)','slope:', B, '+/-', ABerr[1]
print, FORMAT='(A-15,F10.4,A6,F10.4)','y-intercept:', A, '+/-', ABerr[0]
print, ''

;# bces
bc = bces(fx, fy, xerror=fxerr, yerror=fyerr, error=bcerr, bootstrap=5000)
print, 'IDL BCES:'
print, FORMAT='(A-15,F10.4,A6,F10.4)','slope:', bc[1], '+/-', bcerr[1]
print, FORMAT='(A-15,F10.4,A6,F10.4)','y-intercept:', bc[0], '+/- ', bcerr[0]
print, ''

;# mpfit
expr = 'P[0] + X*P[1]'
p0 = [1.D, 0.5D]
p = mpfitexpr(expr, fx, fy, fyerr, p0, perror=msig, /quiet)
print, 'MPFIT:'
print, FORMAT='(A-15,F10.4,A6,F10.4)', 'slope:', p[1], '+/-', msig[1]
print, FORMAT='(A-15,F10.4,A6,F10.4)', 'y-intercept:', p[0], '+/-', msig[0]
print, ''
print, '######################'
print, ''

;# calculate scatter
N = n_elements(fy)
sigma = sqrt(fxerr^2.+(obcesa*fyerr)^2.)
weights = N/(sigma^2.*total(sigma^(-2.)))
pred = weights*(fy-(fx*obcesa+obcesb))^2.
psum = total(pred)
scatter = sqrt(psum/(N-2.0))
junk1 = moment(fx,mdev=devfx)
junk2 = moment(fy,mdev=devfy)
print, FORMAT='(A-15,F10.4)','StdDev P_rad:', devfx
print, FORMAT='(A-15,F10.4)','StdDev P_cav:', devfy
print, FORMAT='(A-15,F10.4)','Corr Coeff:', correlate(fx,fy)
print, FORMAT='(A-15,F10.4)','Scatter:', scatter

;# plot
set_plot, 'PS'
loadct, 13, /silent
device, filename='pcav-lrad_1400.eps', $
        /color, $
        /encapsulated, $
        /portrait, $
        set_font='Times-Roman', $
        bits=16
ytex = textoidl('P_{cav} [10^{42} erg s^{-1}]')
xtex = textoidl('P_{1.4} [10^{40} erg s^{-1}]')
xmin = 1d-5
xmax = 1d5
ymin = 1d-2
ymax = 1d5
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

;# draw jetted box
;oplot, [0.2,0.2], [0.2,30], linestyle=0, psym=0, color=0, thick=2
;oplot, [30,30], [0.2,30], linestyle=0, psym=0, color=0, thick=2
;oplot, [0.2,30], [0.2,0.2], linestyle=0, psym=0, color=0, thick=2
;oplot, [0.2,30], [30,30], linestyle=0, psym=0, color=0, thick=2

;# lines of equality
IF plotlines EQ 'y' THEN BEGIN

   ;# external ortho best-fit bces relation
   px = maken(1d-50,1d50,10)
   py = (10^(obcesb))*(px^(obcesa))
   oplot, px, py, psym=0, linestyle=2, thick=4, color=0

   ;# Birzan best-fit line
   py = (10^birzanb)*(px^birzana)
   oplot, px, py, psym=0, linestyle=1, thick=4, color=250

   ;# Heinz and Grimm line
;   py = 6.2d37*(px/1d30)^(0.704)
;   oplot, px/1d40, py/1d42, psym=0, linestyle=3, thick=4, color=150

   ;# plot 6/7
;   py = 3d38*((px/1d28)^(6./7.))/1d42*1d7
;   px = px*151d6*1d7*(4*!PI)/1d40
;   oplot, px, py, psym=0, linestyle=4, thick=4, color=50

ENDIF 

;# plot upper-limits
IF n_elements(nx) EQ 1 THEN BEGIN
   push, nx, -1d20
   push, ny, -1d20
ENDIF
IF n_elements(nx) GT 1 THEN BEGIN
   plotsym, 6, 2.0

print, nx
print, ny

   oplot, nx, ny, psym=8, color=0
ENDIF ELSE print, '# WARNING: Not enough upper-limits to plot.'

;# plot grade D's
IF n_elements(gradedx) GT 1 THEN BEGIN
   plotsym, 0, 0.9, /fill
   oploterror, gradedx, gradedy, gradedxerr, gradedyhi, psym=8, /hibar, /nohat, color=0
   oploterror, gradedx, gradedy, gradedxerr, gradedylo, psym=8, /lobar, /nohat, color=0
   oplot, gradedx, gradedy, psym=8, color=0
ENDIF ELSE print, '# WARNING: Not enough grade-D to plot.'   

;# plot grade C's
IF n_elements(gradecx) GT 1 THEN BEGIN
   plotsym, 0, 0.9, /fill
   oploterror, gradecx, gradecy, gradecxerr, gradecyhi, psym=8, /hibar, /nohat, color=250
   oploterror, gradecx, gradecy, gradecxerr, gradecylo, psym=8, /lobar, /nohat, color=250
   oplot, gradecx, gradecy, psym=8, color=0
   plotsym, 0, 0.7, /fill
   oplot, gradecx, gradecy, psym=8, color=250
ENDIF ELSE print, '# WARNING: Not enough grade-C to plot.'   

;# plot grade B's
IF n_elements(gradebx) GT 1 THEN BEGIN
   plotsym, 0, 0.9, /fill
   oploterror, gradebx, gradeby, gradebxerr, gradebyhi, psym=8, /hibar, /nohat, color=50
   oploterror, gradebx, gradeby, gradebxerr, gradebylo, psym=8, /lobar, /nohat, color=50
   oplot, gradebx, gradeby, psym=8, color=0
   plotsym, 0, 0.7, /fill
   oplot, gradebx, gradeby, psym=8, color=50
ENDIF ELSE print, '# WARNING: Not enough grade-B to plot.'   

;# plot grade A's
IF n_elements(gradeax) GT 1 THEN BEGIN
   plotsym, 0, 0.9, /fill
   oploterror, gradeax, gradeay, gradeaxerr, gradeayhi, psym=8, /hibar, /nohat, color=150
   oploterror, gradeax, gradeay, gradeaxerr, gradeaylo, psym=8, /lobar, /nohat, color=150
   oplot, gradeax, gradeay, psym=8, color=0
   plotsym, 0, 0.7, /fill
   oplot, gradeax, gradeay, psym=8, color=150
ENDIF ELSE print, '# WARNING: Not enough grade-A to plot.'   

;# plot dwarfs
IF n_elements(dwfx) GT 1 THEN BEGIN
   plotsym, 8, 0.7, /fill
   oploterror, dwfx, dwfy, dwfxerr, dwfyhi, psym=8, /hibar, /nohat, color=0
   oploterror, dwfx, dwfy, dwfxerr, dwfylo, psym=8, /lobar, /nohat, color=0
   oplot, dwfx, dwfy, psym=8, color=0
ENDIF ELSE print, '# WARNING: Not enough dwarfs to plot.'

;# plot birzan's numbers
plotsym, 4, 0.9, /fill
oploterror, bl14, bpcav, bl14err, bphi, psym=8, /hibar, /nohat, color=225
oploterror, bl14, bpcav, bl14err, bplo, psym=8, /lobar, /nohat, color=225
oplot, bl14, bpcav, psym=8, color=0
plotsym, 4, 0.7, /fill
oplot, bl14, bpcav, psym=8, color=225

;# plot croston's numbers
IF croston EQ 'y' THEN BEGIN
   plotsym, 0, 0.5
   oplot, cl14, cpcav, psym=8, color=0
ENDIF

;# plot jet dominated systems
IF dojet EQ 'y' THEN BEGIN
   IF n_elements(jetx) GT 0 THEN BEGIN
      oplot, jetx, jety, psym=8, color=0
   ENDIF
   x = [1d-20, 140]
   y = [1d-20, 2.3d-1]
   oplot, x, y, psym=8, color=0
   push, items, textoidl('Poorly confined')
ENDIF

;# add legend
push, items, textoidl('B08 sample')
push, items, textoidl('Grade A')
push, items, textoidl('Grade B')
push, items, textoidl('Grade C')
linearr = replicate(-99,n_elements(items))
psyarr = replicate(-99,n_elements(items))
legend, items, linestyle=larr, psym=parr, box=0, charsize=0.7, /bottom, /right

;# raffertyn legend
x = [1d-20, 450]
y = [1d-20, 1.35d-1]
plotsym, 4, 0.9, /fill
oplot, x, y, psym=8, color=0
plotsym, 4, 0.7, /fill
oplot, x, y, psym=8, color=225

;# grade A legend
x = [1d-20, 1200]
y = [1d-20, 7.8d-2]
plotsym, 0, 0.9, /fill
oplot, x, y, psym=8, color=0
plotsym, 0, 0.7, /fill
oplot, x, y, psym=8, color=150

;# grade B legend
x = [1d-20, 1200]
y = [1d-20, 4.6d-2]
plotsym, 0, 0.9, /fill
oplot, x, y, psym=8, color=0
plotsym, 0, 0.7, /fill
oplot, x, y, psym=8, color=50

;# grade C legend
x = [1d-20, 1200]
y = [1d-20, 2.7d-2]
plotsym, 0, 0.9, /fill
oplot, x, y, psym=8, color=0
plotsym, 0, 0.7, /fill
oplot, x, y, psym=8, color=250

;# close device
device, /close

!QUIET=0
END
