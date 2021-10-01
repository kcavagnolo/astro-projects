PRO spin

ecav = 6d60
pjet = 6d45
mbh  = 1.48d9

!P.MULTI  = [0,2,1,0]
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 2
!Y.THICK  = 2
!Z.THICK  = 2
set_plot, 'PS'
device, filename='espin.eps', $
        /encapsulated, $
        /portrait, $
        set_font='Helvetica'
j = maken(-1,1,10000)
espin = alog10(1.6d62*(mbh/1d9)*abs(j)^2.)
needj = sqrt(ecav/(1.6d62*(mbh/1d9)))
print, 'Need j: ', needj
ytex = textoidl('log E_{spin} [erg]')
xtex = textoidl('j')
xmin = -1.05
xmax = 1.05
ymin = 61.
ymax = 62.5
plot, [xmin,xmax], [ymin,ymax], $
      /nodata, $
      /xsty, /ysty, $
      xtitle = xtex, $
      ytitle = ytex, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      charsize = 1.2, $
      position = aspect(1.0)
oplot, j, espin, linestyle=0
oplot, [-100,100], replicate(alog10(ecav),2), linestyle=1
device, /close

;# pjet
set_plot, 'PS'
device, filename='pspin.eps', $
        /encapsulated, $
        /portrait, $
        set_font='Helvetica'
bd = [300., 1300., 6700.]
plop = [1,2,5]
delta = 2.5
j = [1.0, 0.9, 0.8, 0.7, 0.6, needj]
j = [-j,reverse(j)]
gamma = (0.002/(j-0.65)^2.)+(0.1/(j+0.95))+(0.002/(j-0.055)^2.)
beta = (-(3./2.)*j^3.)+(12.*j^2.)-(10.*j)+7.-gamma
alpha = delta*((3./2.)-j)
ytex = textoidl('log P_{jet} [erg s^{-1}]')
xtex = textoidl('j')
ymin = 43
ymax = 49.5
plot, [xmin,xmax], [ymin,ymax], $
      /nodata, $
      /xsty, /ysty, $
      xtitle = xtex, $
      ytitle = ytex, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      charsize = 1.2, $
      position = aspect(1.0)
FOR i=0,n_elements(bd)-1 DO BEGIN
   ljet = 2d47*alpha*beta^2.*(bd[i]/1d5)^2.*(mbh/1d9)^2.*j^2.
   ljet = alog10(ljet)
   oplot, j, ljet, psym=plop[i]
   IF i EQ 0 THEN push, items, textoidl('B_{d,5} = '+num2str(sigfig(bd[i],3))+' G') ELSE $
      push, items, textoidl(num2str(sigfig(bd[i],3))+' G')
ENDFOR
oplot, [-100,100], replicate(alog10(pjet),2), linestyle=1
legend, items, psym=plop, box=0, charsize=1.0, /top, /right, /fill
device, /close

;# b disk
j1 = maken(-1.0,-needj,10000)
gamma = (0.002/(j1-0.65)^2.)+(0.1/(j1+0.95))+(0.002/(j1-0.055)^2.)
beta = (-(3./2.)*j1^3.)+(12.*j1^2.)-(10.*j1)+7.-gamma
alpha = delta*((3./2.)-j1)
needb1 = sqrt(pjet/(2d47*alpha*beta^2.*(mbh/1d9)^2.*j1^2.))*1d5
;ord = where((needb1 LT 1d4) AND (needb1 GT 1d2))
;j1 = j1[ord]
;needb1 = needb1[ord]

j2 = maken(1.0,needj,1000)
gamma = (0.002/(j2-0.65)^2.)+(0.1/(j2+0.95))+(0.002/(j2-0.055)^2.)
beta = (-(3./2.)*j2^3.)+(12.*j2^2.)-(10.*j2)+7.-gamma
alpha = delta*((3./2.)-j2)
needb2 = sqrt(pjet/(2d47*alpha*beta^2.*(mbh/1d9)^2.*j2^2.))*1d5
;ord = where((needb2 LT 1d4) AND (needb2 GT 2d3))
;j2 = j2[ord]
;needb2 = needb2[ord]

plotsym, 0, 0.8, /fill
set_plot, 'PS'
device, filename='bspin.eps', $
        /encapsulated, $
        /portrait, $
        set_font='Helvetica'
ytex = textoidl('B_{d,5} [G]')
xtex = textoidl('j')
ymin = 0.1*min([needb1,needb2])
ymax = 1.05*max([needb1,needb2])
plot, j1, needb1, $
      /xsty, /ysty, $
      xtitle = xtex, $
      ytitle = ytex, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      charsize = 1.2, $
      position = aspect(1.0), $
      psym = 0
oplot, j2, needb2
device, /close

;# exact values
gamma = (0.002/(needj-0.65)^2.)+(0.1/(needj+0.95))+(0.002/(needj-0.055)^2.)
beta = (-(3./2.)*needj^3.)+(12.*needj^2.)-(10.*needj)+7.-gamma
alpha = delta*((3./2.)-needj)
needb = sqrt(pjet/(2d47*alpha*beta^2.*(mbh/1d9)^2.*needj^2.))*1d5
print, 'Need B(j) [G]: ', needb
needj = -needj
gamma = (0.002/(needj-0.65)^2.)+(0.1/(needj+0.95))+(0.002/(needj-0.055)^2.)
beta = (-(3./2.)*needj^3.)+(12.*needj^2.)-(10.*needj)+7.-gamma
alpha = delta*((3./2.)-needj)
needb = sqrt(pjet/(2d47*alpha*beta^2.*(mbh/1d9)^2.*needj^2.))*1d5
print, 'Need B(-j) [G]: ', needb

rg = 1.0
alpha = 0.5
mdot = 0.01
;b1 = 3d8*mbh^(-1./2.)*rg
b2 = 3.03d7*mdot^(2./5.)*alpha^(1./20.)*mbh^(-9./20.)*rg
b3 = 4.17d9*mdot^(2./5.)*alpha^(-9./20.)*mbh^(-9./20.)*rg^(13./10.)
b4 = 6.55d8*mdot^(1./2.)*alpha^(-1./2.)*mbh^(-1./2.)*rg^(5./4.)

print, b2, b3, b4

;# gamma is adiabatic index of accreting fluid
;# alpha is viscosity
;# beta is ratio of gas to mag pressure
;pmag = b^2./(8*!PI)
;beta = pgas/pmag                ;# typically 1--10
;gamma = (5.*beta+8.)/(6.+3.*beta)
;alpha = 0.55/(1.0+beta)         ;# typically 0.04--0.3
;alpha = 0.3
;beta = 10.
;gamma = (5.*beta+8.)/(6.+3.*beta)
;f = 0.5
;epsilon = (1./f)*((5./3.)-gamma)/(gamma-1.0)
;gae = sqrt(1.+(18.*alpha^2./(5.+2.*epsilon)^2.))
;c1 = (5.+2.*epsilon)*gae/(3.*alpha^2.)
;c2 = sqrt(2.*epsilon*(5.+2.*epsilon)*gae/(9.*alpha^2.))
;c3 = (c2^2./epsilon)
;r = R/(2.95*1d5*mbh)
;r = 10.0
;nemb = 6.55d8*alpha^(-1./2.)*sqrt(1.0-beta)*c1^(-1./2.)*c3^(1./4.)*mbh^(-1./2.)*mdot^(1./2.)*r^(-5./4.)
;b = 1d8*sqrt(mbh)
;print, b

END
