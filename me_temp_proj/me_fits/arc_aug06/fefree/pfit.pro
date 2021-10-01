pro pfit, dat1, dat2, dat3

; ie:
; pfit, '0.5-9.5.fits', '2.0-9.5.fits', 'fin.fits'
; pfit, '0.5-7.0.fits', '2.0-7.0.fits', 'fin.fits'

; restore the fit template and read some variables
half = mrdfits(dat1, 1, thdr)
two  = mrdfits(dat2, 1, hhdr)
ref = mrdfits(dat3, 1, rhdr)

; store the hi and lo errors in variables
terrlo = two.tx - two.tlo
terrhi = two.thi - two.tx
herrlo = half.tx - half.tlo
herrhi = half.thi - half.tx

FOR i = 0,n_elements(ref.obsid)-1 DO BEGIN
    datat = [(two.thi[i]), (two.tx[i]), (two.tlo[i])]
    datah = [(half.thi[i]), (half.tx[i]), (half.tlo[i])]
    avgstd, datat, resultt, /quiet
    avgstd, datah, resulth, /quiet
    sigt = resultt[1]
    sigh = resulth[1]
    IF (sigt LE 1. AND $
        sigh LE 1. AND $
        two.tx[i] LE 10. AND $
        half.tx[i] LE 10.) THEN $
      IF (n_elements(ttx) EQ 0) THEN BEGIN
        devt = sigt
        devh = sigh
        ttx = two.tx[i]
        tlo = terrlo[i]
        thi = terrhi[i]
        htx = half.tx[i]
        hlo = herrlo[i]
        hhi = herrhi[i]
        used = strcompress(two.obsid[i])+' ('+strcompress(two.cluster[i],/remove_all)+', '+strcompress(two.z[i])+')'
    ENDIF ELSE BEGIN
        devt = [devt, sigt]
        devh = [devh, sigh]
        ttx = [ttx, two.tx[i]]
        tlo = [tlo, terrlo[i]]
        thi = [thi, terrhi[i]]
        htx = [htx, half.tx[i]]
        hlo = [hlo, herrlo[i]]
        hhi = [hhi, herrhi[i]]
        used = [used, strcompress(two.obsid[i])+' ('+strcompress(two.cluster[i],/remove_all)+', '+strcompress(two.z[i])+')']
    ENDELSE
ENDFOR

coeffs = [0.8, 1.10] ; initial guesses of [A,B] in y = A*x^B, use 0.8 and 1.10 from M/E paper
weights = devh
yfit = MPCURVEFIT(ttx, htx, weights, coeffs, sigma, function_name='plaw', CHISQ=chi, NFREE=nfree, DOF=dof)

print, ''
print, 'MPCURVEFIT Results : A =', strcompress(coeffs[0]),' ; sigA =', strcompress(sigma[0])
print, '                     B =', strcompress(coeffs[1]),' ; sigB =', strcompress(sigma[1])
print, '                     chisq =', strcompress(chi),' ; dof =', strcompress(dof),' ; nfree =', strcompress(nfree)
print, ''
print, 'Total Clusters: ', strcompress(n_elements(ref.obsid))
print, ''
print, 'Clusters in fit: ', strcompress(n_elements(used))
print, ''
print, 'Cluster obsids and names: ', used

FOR i=0,n_elements(ttx)-1 DO BEGIN
    IF (ttx[i] GT htx[i]) THEN $
      print, 'Tx(2.0) > Tx(0.5) for ', used[i]
ENDFOR

; plot commands for making a postscript file
; define the labels for the plot axes
xtx = textoidl("kT_{S} [2.0-7.0] keV band")
ytx = textoidl("kT_{S} [0.5-7.0] keV band")

A = strcompress(sigfig(coeffs[0],3),/remove_all)
B = strcompress(sigfig(coeffs[1],3),/remove_all)
sigA = strcompress(sigfig(sigma[0],3),/remove_all)
sigB = strcompress(sigfig(sigma[1],3),/remove_all)
chi = strcompress(sigfig(chi/dof,3),/remove_all)

equa = textoidl('Best Fit: kT_{S,[0.5-7.0]} = ('+A+'\pm'+sigA+')kT_{S,[2.0-7.0]}^{'+B+'\pm'+sigB+'}')
csq = textoidl('\chi_{red}^{2}: '+chi)

set_plot, 'PS'
device, filename = "sev_half_vs_two.eps", /encapsulated
!fancy = 4
!linetype = 0
plot, ttx, htx, /XLOG, /YLOG,$
      psym = 4, $
      xtitle = xtx, $
      ytitle = ytx, $
      xstyle = 9, $
      xrange = [1.,10.], $
      yrange = [1.,10.], $
      charsize = 0.8

; overplot the hi and lo errorbars in both directions
oploterror, ttx, htx, thi, hhi, psym=3, /hibar
oploterror, ttx, htx, tlo, hlo, psym=3, /lobar

; overplot the line x=y
x = findgen(20)
y = x
oplot, x, y, linestyle=2, psym=0

; overplot the fit line
x = findgen(20)
y = A*x^B
oplot, x, y, linestyle=0, psym=0

; draw the legend
items = ["Line of Equality", equa, csq]
linearr = [2, 0, -99]
psyarr = [0, 0, -99]
thiarr = [0.8, 0.8, 0.8]
legend, items, linestyle=linearr, psym=psyarr, box=0, spacing=0.4, thick=thiarr, charthick=0.8, charsize=0.8, /top, /left_legend

device, /close
set_plot, "X"


END
