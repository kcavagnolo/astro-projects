pro pfit, dat1, dat2

; ie:
; pfit,'dat/adj_r2500_nhfro_fefro_7-7.dat','dat/adj_r2500_nhfro_fefro_2-7.dat'
; pfit,'dat/adj_r2500-50_nhfro_fefro_7-7.dat','dat/adj_r2500-50_nhfro_fefro_2-7.dat'
; pfit,'dat/adj_r5000_nhfro_fefro_7-7.dat','dat/adj_r5000_nhfro_fefro_2-7.dat'
; pfit,'dat/adj_r5000-50_nhfro_fefro_7-7.dat','dat/adj_r5000-50_nhfro_fefro_2-7.dat'
; pfit,'dat/adj_robs-70_nhfro_fefro_7-7.dat','dat/adj_robs-70_nhfro_fefro_2-7.dat'
; pfit,'dat/adj_rmax-70_nhfro_fefro_7-7.dat','dat/adj_rmax-70_nhfro_fefro_2-7.dat'
; pfit,'dat/fak_control_nhfro_fefro_7-7.dat','dat/fak_control_nhfro_fefro_2-7.dat'
; pfit,'dat/fak_control_nhfro_fefree_7-7.dat','dat/fak_control_nhfro_fefree_2-7.dat'

; define limiters for what is used in the fit
plawfile = "none"               ; name of output file for power law indice values (alpha, beta)
output = "fak_control.ps"       ; name of output plots
suppress = "no"                 ; removes the output info about the fit params
logplot = "yes"                 ; should this be a log plot?
namelim = 8.0                   ; Tx ratio limit above which names are added to the plot
Tmin = 1.0                      ; lower range on plot
Tmax = 50.0                     ; upper range on plot

fidA = 0.80                     ; initial guesses of A in y = A*x^B
fidB = 1.10                     ; initial guesses of B ^in y = A*x^B
zlim = 0.0                      ; lower limit cut in redshift space
srclim = -1.                    ; lower limit percent of emission which is source
ratlim = 6.                     ; upper limit for ratio of Tx(0.7-7.0)/Tx(2.0-7.0)
txlim = Tmax                    ; upper limit to Tx
ssize = 0.5                     ; size of symbols

; restore the fit template and read some variables
restore,"../scripts/xspectemp_rin_normerr_src.sav"
half = read_ascii(dat1, template = xspectemp_rin_normerr_src)
two  = read_ascii(dat2, template = xspectemp_rin_normerr_src)

; store the hi and lo errors in variables
terrlo = two.tx - two.tlo
terrhi = two.thi - two.tx
herrlo = half.tx - half.tlo
herrhi = half.thi - half.tx

; test to see which deviation is larger, the hi or lo;
; store the larger of the two as the error value
sigh = fltarr(n_elements(terrlo))
sigt = sigh
FOR h = 0,n_elements(sigh)-1 DO BEGIN
    IF (herrlo[h] GT herrhi[h]) THEN sigh[h] = herrlo[h] ELSE sigh[h] = herrhi[h]
    IF (terrlo[h] GT terrhi[h]) THEN sigt[h] = terrlo[h] ELSE sigt[h] = terrhi[h]
ENDFOR

FOR i = 0,n_elements(two.obsid)-1 DO BEGIN
    rat  = two.tx[i]/half.tx[i]
    IF (two.z[i]   GE zlim AND $
        two.src[i] GE srclim AND $
        two.tx[i]  LT txlim AND $
        half.tx[i] LT txlim AND $
        rat        LT ratlim AND $
        1./rat     LT ratlim) THEN BEGIN
        IF (rat    GT namelim OR $
            1./rat GT namelim) THEN BEGIN
            name = two.cluster[i]
        ENDIF ELSE BEGIN
            name = ""
        ENDELSE

        ; build some arrays
        push, devt, sigt[i]
        push, devh, sigh[i]
        push, ttx, two.tx[i]
        push, htx, half.tx[i]
        push, names, name

    ENDIF
ENDFOR
    
; begin the power law fitting
coeffs = [fidA, fidB]
oldchi = 10000.
newchi = 1000.
con = 0
n = 0

WHILE (con lt 4.) DO BEGIN
    IF (n GT 0) THEN $
      void,err
    FOR j = 0,n_elements(devh)-1 DO BEGIN
        effvar = sqrt(devh[j]^2+((coeffs[0]*coeffs[1]*(ttx[j]^(coeffs[1]-1)))*devt[j])^2)
        push, err, effvar
    ENDFOR
    weights = 1/(err)^2.
    oldchi = newchi
    yfit = MPCURVEFIT(ttx, htx, weights, coeffs, sigma, function_name='plaw', CHISQ=chi, NFREE=nfree, DOF=dof, /autoderivative, /quiet)
    print,'iteration ', strcompress(n,/remove_all),':   A=', strcompress(coeffs[0]),' ; B=', strcompress(coeffs[1]),' ; ChiSq=', strcompress(chi)
    newchi = chi
    IF (newchi ge oldchi) THEN con++
    n++
ENDWHILE

; calculate the weighted average and associated error for bins of size
; 1keV to ease the look of the plot
binmin = 0.
binmax = 1.
m = 0
IF max(ttx) GT max(htx) THEN maxss = max(ttx) ELSE maxss = max(htx) 
FOR ss = 0,round(maxss) DO BEGIN
    IF m GT 0 THEN BEGIN
        IF (n_elements(twi) GT 0) THEN BEGIN
            void,txi
            void,twi
            void,hxi
            void,hwi
        ENDIF
    ENDIF
    FOR k = 0,n_elements(ttx)-1 DO BEGIN
        IF (ttx[k] GE binmin AND ttx[k] LE binmax) THEN BEGIN
            push, txi, ttx[k]
            push, hxi, htx[k]
            push, twi, (1./(sigt[k]^2.))
            push, hwi, (1./(sigh[k]^2.))
        ENDIF
    ENDFOR
    IF (n_elements(twi) GT 0) THEN BEGIN
        tnume = total(txi*twi)
        tdeno = total(twi)
        hnume = total(hxi*hwi)
        hdeno = total(hwi)

        ; build some arrays
        push, twav, (tnume/tdeno)
        push, tsigwav, (1./sqrt(tdeno))
        push, hwav, (hnume/hdeno)
        push, hsigwav, (1./sqrt(hdeno))
    ENDIF
    m++
    IF binmax LT 10. THEN binsize=1. ELSE binsize=10.
    binmin = binmax
    binmax = binmax+binsize
ENDFOR

; plot commands for making a postscript file
; define the labels for the plot axes
xtx = textoidl("kT_{S} [2.0/(1+z)-7.0] keV band")
ytx = textoidl("kT_{S} [0.7-7.0] keV observed band")

A    = strcompress(sigfig(coeffs[0],3),/remove_all)
B    = strcompress(sigfig(coeffs[1],3),/remove_all)
sigA = strcompress(sigfig(sigma[0],3),/remove_all)
sigB = strcompress(sigfig(sigma[1],3),/remove_all)
chi  = strcompress(sigfig(chi,3),/remove_all)
dof  = strcompress(sigfig(dof,3),/remove_all)
num  = strcompress((n_elements(ttx)),/remove_all)
z    = strcompress(sigfig(zlim,2),/remove_all)
src  = strcompress(sigfig(srclim,3),/remove_all)
tx   = strcompress(sigfig(txlim,3),/remove_all)
rat  = strcompress(sigfig(ratlim,2),/remove_all)
    
Apm  = textoidl(A+'\pm'+sigA)
Bpm  = textoidl(B+'\pm'+sigB)
equa = textoidl('Best Fit: kT_{S,[0.7-7.0]} = ('+Apm+')kT_{S,[2.0/(1+z)-7.0]}^{'+Bpm+'}')
csq  = textoidl('\chi^{2}: '+chi)
dof  = textoidl('DOF: '+dof)
num  = textoidl('Clusters in fit: '+num)
z    = textoidl('Redshift cut: '+z)
src  = textoidl('Source % cut: '+src)
tx   = textoidl('Upper T_{X} cut: '+tx)
rat  = textoidl('T_{X} ratio cut: '+rat)
    
; draw the legend
IF (suppress EQ "no") THEN BEGIN
    items = ["Line of Equality", equa, csq, dof, num, z, src, tx, rat]
    linearr = [2, 0, -99, -99, -99, -99, -99, -99, -99]
    psyarr  = [0, 0, -99, -99, -99, -99, -99, -99, -99]
    thiarr  = [0.8, 0.8, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6]
ENDIF ELSE BEGIN
    items = ["Line of Equality", equa]
    linearr = [2, 0]
    psyarr  = [0, 0]
    thiarr  = [0.8, 0.8]
ENDELSE

; make a hardcopy
set_plot, 'PS'
device, filename=output, /color
!fancy = 4
!p.font=0
!linetype = 0
!p.font=0
IF (logplot EQ "yes") THEN BEGIN
    plot, ttx, htx, /XLOG, /YLOG, $
          psym = 4, $
          symsize = ssize, $
          xtitle = xtx, $
          ytitle = ytx, $
          xstyle = 1, $
          ystyle = 1, $
          xrange = [Tmin,Tmax], $
          yrange = [Tmin,Tmax], $
          charsize = 1.0
ENDIF ELSE BEGIN
    plot, ttx, htx, $
          psym = 4, $
          symsize = ssize, $
          xtitle = xtx, $
          ytitle = ytx, $
          xstyle = 1, $
          ystyle = 1, $
          xrange = [Tmin,Tmax], $
          yrange = [Tmin,Tmax], $
          charsize = 1.0
ENDELSE
    
; overplot the line x=y
x = findgen(Tmax*5)+Tmin
y = x
oplot, x, y, linestyle=2, psym=0
    
; overplot the weighted average, errors, and bin demarcation
oploterror, twav, hwav, tsigwav, hsigwav, psym=3

; overplot a shaded region for the fit
IF (logplot EQ "no") THEN BEGIN
    top = (coeffs[0]+sigma[0])*x^(coeffs[1]+sigma[1])
    bot = (coeffs[0]-sigma[0])*x^(coeffs[1]-sigma[1])
    oband, x, top, bot, /line_fill, orien=-45
ENDIF

; overplot the fit line
x = findgen(Tmax*5)+Tmin
y = coeffs[0]*x^coeffs[1]
oplot, x, y, linestyle=0, psym=0

; add names to hot clusters
xyouts, ttx, htx, names, charthick=0.5, charsize=0.5, align=0.5

; draw the legend
legend, items, linestyle=linearr, psym=psyarr, box=0, spacing=1.0, thick=thiarr, charthick=0.6, charsize=0.6, /top, /left_legend

; close the device
device, /close

; print power law info to the dat file
IF plawfile NE "none" THEN BEGIN
    zz = 0
    openu, 1, plawfile, /append
    WHILE (zz LT n_elements(Aarr)-1) DO BEGIN
        printf, 1, output,'   ',zz,'   ',Aarr[zz],'   ',Barr[zz]
        zz++
    ENDWHILE
    printf, 1, ''
    close, 1
ENDIF

END
