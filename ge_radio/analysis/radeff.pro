PRO radeff

tbcorr = 0.65
file1 = 'jetted_pcav.dat'
readcol, file1, format='A,F,F,F,D,D,D,D,D,D', comment='#', $
         jname, jz, jd, jkt, jecav, jtcs, jpcav, jperr, jl14, jl14err
jpcav = jpcav/1d42/tbcorr
jperr = jperr/1d42/tbcorr
jl14 = jl14/1d40
jl14err = jl14err/1d40
file2 = 'croston_pcav.dat'
readcol, file2, format='A,F,F,F,D,D,D,D,D,D', comment='#', $
         cname, cz, cd, ckt, cecav, ctcs, cpcav, cperr, cl14, cl14err
cpcav = cpcav/1d42/tbcorr
cperr = cperr/1d42/tbcorr
cl14 = cl14/1d40
cl14err = cl14err/1d40

;# fiducial array
px = maken(1d-50,1d50,5)

;# plot sur bri prof and nuker fit to own file
set_plot, 'PS'
loadct, 13, /silent
device, filename='radeff.eps', $
        /color, $
        /encapsulated, $
        /portrait, $
        set_font='Times-Roman', $
        bits=16
ytex = textoidl('P_{jet} [10^{42} erg s^{-1}]')
xtex = textoidl('P_{1.4} [10^{40} erg s^{-1}]')
xmin = 1d-5
xmax = 1d5
ymin = 1d-5
ymax = 1d5
x = maken(xmin,xmax,2)
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

;# Willott relation w/ low-k
f = 1.0
py = 4.61d41*f^(3./2.)*(px/1d40)^(6./7.) ;# corrected to 1.4 GHz
oplot, px/1d40, py/1d42, psym=0, linestyle=0, thick=4, color=250
;py = 3d38*f^(3./2.)*((px/1d28)^(6./7.))
;oplot, (px*151d6*1d7*(4*!PI)/1d40), (py*1d7/1d42), psym=0, linestyle=0, thick=4, color=250
push, items, 'W99; k=1'
push, colors, 250
push, linearr, 0

;# Willott relation w/ high-k
gg = 2.0
fmin = 1.0
eta = 1.0
k = 1000.
fgeom = 1.4
flowe = 2.0
f = gg*fmin*eta*((1+k)*fgeom*flowe/eta)^(4./7.)
print, '## Using f = ',f,' e.g. f^3/2 = ',f^(3./2.)
py = 4.61d41*f^(3./2.)*(px/1d40)^(6./7.) ;# corrected to 1.4 GHz
;oplot, px/1d40, py/1d42, psym=0, linestyle=0, thick=4, color=50
;py = 3d38*f^(3./2.)*((px/1d28)^(6./7.)) ;# original relation
;oplot, (px*151d6*1d7*(4*!PI)/1d40), (py*1d7/1d42), psym=0, linestyle=0, thick=4, color=50
;push, items, 'W99; k=100'
;push, colors, 50
;push, linearr, 0

;# 1.4 GHz external ortho best-fit bces relation
a = 0.720
b = 1.92
py = (10^(b))*(px^(a))
oplot, px, py, psym=0, linestyle=2, thick=4, color=0
push, items, 'This work'
push, colors, 0
push, linearr, 2

;# plot +- 1 dex in eta
epint = 1.2879
py = (10^(b+epint))*(px^(a))
oplot, px, py, psym=0, linestyle=1, thick=2, color=0
py = (10^(b-epint))*(px^(a))
oplot, px, py, psym=0, linestyle=1, thick=2, color=0
push, items, textoidl('This work \eta \pm \epsilon_{int}')
push, colors, 0
push, linearr, 1

;# plot points for poorly confined
plotsym, 0, 0.7, thick=1.5
oplot, jl14, jpcav, psym=8, color=0
push, items, 'Poorly confined'
push, colors, -1
push, linearr, -1
x = [1d4, 1d-100]
y = [8d-5, 1d-100]
oplot, x, y, psym=8, color=0

;# c08 fr1
plotsym, 5, 0.7, thick=1.5
oplot, cl14, cpcav, psym=8, color=0
push, items, 'C08 FR-I'
push, colors, -1
push, linearr, -1
x = [1d4, 1d-100]
y = [4d-5, 1d-100]
oplot, x, y, psym=8, color=0

;# M87, CYGA, 3C388, 3C401, 4C55.16, A85, A2029, HERCA, PKS0745-191, PKS1404-267
;plotsym, 8, 0.7, /fill
;prad = [5.468, 9035.8, 165.3, 782.6, 2023.0, 1.1, 10.4, 4134.1, 87.9, 0.9]
;pcav = [6.0, 1328.0, 200.0, 650.0, 420.0, 37.0, 87.0, 310.0, 1700.0, 20.0]
;oplot, prad, pcav, psym=8, color=0

;# add legend
psyarr = replicate(0,n_elements(items))
legend, items, linestyle=linearr, colors=colors, $
        thick=4, box=0, charsize=0.7, number=1, $
        /bottom, /right

device,/close

END


;; ;# Birzan total radio relation
;; a = 0.48
;; b = 2.32
;; py = (10^(b))*(px^(a))
;; oplot, px, py, psym=0, linestyle=0, thick=4, color=0

;; ;# Birzan 327 MHz relation
;; a = 0.724                       ;# my bces fit
;; b = 1.43
;; py = (10^b)*(px^a)
;; oplot, px, py, psym=0, linestyle=2, thick=4, color=100
;; ;# Birzan break frequency corrected relation
;; vc = [8.4, 5.0, 1.4]
;; FOR i=0,n_elements(vc)-1 DO BEGIN
;;    py = 10^(2.12)*(px^(0.53)/vc[i]^(0.74))
;;    oplot, px, py, psym=0, linestyle=1, thick=4, color=0
;; ENDFOR

;; ;# 1.4 GHz external ortho best-fit bces relation
;; a = 12./17.
;; b = 1.92
;; py = (10^(b))*(px^(a))
;; oplot, px, py, psym=0, linestyle=0, thick=4, color=0
;; push, items, textoidl('Q \propto P_{radio}^{12/17}')
;; push, colors, 0
;; push, linearr, 0

;; ;# Merloni and Heinz relation
;; py = 10.^(11.9)*px^(0.81)
;; oplot, px/1d40, py/1d42, psym=0, linestyle=4, thick=4, color=250
;; push, items, 'Merloni & Heinz `07'
;; push, colors, 250
;; push, linearr, 4

;; ;# Heinz and Grimm relation
;; py = 6.2d37*(px/1d30)^(12./17.)
;; oplot, px/1d40, py/1d42, psym=0, linestyle=5, thick=4, color=0
;; push, items, 'Heinz & Grimm `05'
;; push, colors, 0
;; push, linearr, 5

;; ;# 200-400 GHz external ortho best-fit bces relation
;; a = 0.640
;; b = 1.55
;; py = (10^(b))*(px^(a))
;; oplot, px, py, psym=0, linestyle=2, thick=4, color=0
;; push, items, 'This work 200-400 MHz'
;; push, colors, 0
;; push, linearr, 2

;; ;# high-k guys, ms07, hyda, a262
;; plotsym, 0, 0.7, /fill
;; x = [2.3740, 447.02, 0.0274]
;; y = [6948.00, 1082.80, 9.68]
;; oplot, x, y, psym=8, color=0
