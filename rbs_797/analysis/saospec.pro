PRO saospec

!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 3
!Y.THICK  = 3
!Z.THICK  = 3
set_plot,'PS'
loadct, 39

;# options
mode   = 'lam'                  ;# 'lam' or 'nu'
dered  = 'yes'
z      = 0.354
abreak = 4000.
;alim   = [3750, 3950., 4050., 4250.] ;# Bruzual 1983 definition
alim   = [3850, 3950., 4000., 4100.] ;# Balogh et al 1999 definition
refmin = 3239.87011719
refinc = 5.2332701683
csize  = 1.5
lsize  = 0.8
psize  = 2.0
pcolor = 250
angstrom = STRING(197B)

;# read data
IF mode EQ 'lam' THEN BEGIN
   y = mrdfits('../data/sao/rhs30sao.ifits',0)
   out  = 'spectrum_lam.ps'
   fidloc = 4.8
   ymin = 0.0
   ymax = 5.0
   xtex = '\lambda_{rest} ['+angstrom+']'
   ytex = textoidl('F_{\lambda} [10^{-16} ergs cm^{-2} s^{-1}'+angstrom+'^{-1}]')
ENDIF ELSE BEGIN
   y = mrdfits('../data/sao/fnu.fits',0)
   y = y/1d-11
   out  = 'spectrum_nu.ps'
   fidloc = 3.8
   ymin = 0.0
   ymax = 4.0
   xtex = textoidl('\lambda_{rest} ['+angstrom+']')
   ytex = textoidl('F_{\nu} [10^{-11} ergs cm^{-2} s^{-1} Hz^{-1}]')
ENDELSE
x = findgen(n_elements(y))
x = (refmin+x*refinc)

;# lines I want to see
push, lines,  [3727.,   3933.7, 3968., 4340.5,    4372.,  4861.3,   5007., 4959.]
push, nlines, ['[OII]', 'CaII HK','', 'H\gamma', 'Fe I', 'H\beta', '[OIII]', '']

;# build opt line array
;push, lines,  [6562.8,     4861.3,   5007.,   4959.,    4363.,    3727.,   3869.,   6300.,  6548.,   6584.,   5755.]
;push, nlines, ['H\alpha', 'H\beta', '[OIII]', '[OIII]', '[OIII]', '[OII]', '[OII]', '[OI]', '[NII]', '[NII]', '[NII]']
;push, lines,  [1216.,      5876., 3889., 4472., 6678., 4340.5,    4102.,     4384., 5178., 5891.]
;push, nlines, ['Ly\alpha', 'HeI', 'HeI', 'HeI', 'HeI', 'H\gamma', 'H\delta', 'FeI', 'Mg',  'Na']
;push, lines,  [3835.,      3860.,      3933.7, 3968.5,         4190.,      4305.]
;push, nlines, ['BL!C3835', 'CN!C3860', '','CaII KH+H\epsilon', 'CN!C4190', 'G Band']
;push, lines,  [4686.,  6717.,  6731., 3868.,  3426.]
;push, nlines, ['HeII', 'SII', 'SII', 'NeIII', 'NeV']

;# build UV line array
;push, lines,  [977.,   991.,   1000.,  1035., 1085., 1218., 1240.]
;push, nlines, ['CIII', 'NIII', 'NeVI', 'OVI', 'NII', 'OV', 'NV']
;push, lines,  [1335., 1402.,1486., 1550., 1666.,  1750.,  1886.,   1909.,  2326., 2423.,  2470., 2800.,  2967.]
;push, nlines, ['CII', 'OIV', 'NIV', 'CIV', 'OIII', 'NIII', 'SiIII', 'CIII', 'CII', 'NeIV', 'OII', 'MgII', 'CI']

;# add skylines
skylines = [5577.]
nskylines = ['Sky']

;# take care of redshift
IF dered EQ 'yes' THEN BEGIN
   x = x/(1.0+z)
   skylines = skylines/(1.0+z)
   xmin = 3500
   xmax = 5500
ENDIF ELSE BEGIN
   lines = lines*(1.0+z)
   alim = alim*(1.0+z)
   abreak = abreak*(1.0+z)
ENDELSE
push, lines, skylines
push, nlines, nskylines

;# calc d4000
ord    = where(x GE alim[0] AND x LE alim[1])
d4blue = total(y[ord])
ord    = where(x GE alim[2] AND x LE alim[3])
d4red  = total(y[ord])
print, 'D4000 = ', d4red/d4blue

;# print some info
print, 'l01: ', alim[0]+((alim[1]-alim[0])/2.)
print, 'width1: ', alim[1]-alim[0]
print, 'l02: ', alim[2]+((alim[3]-alim[2])/2.)
print, 'width2: ', alim[3]-alim[2]

;# order lines by wavelength
ord = sort(lines)
lines = lines[ord]
nlines = nlines[ord]

;# plot stuff
device, filename=out, $
        /portrait, $
        set_font='Helvetica'
IF mode EQ 'lam' THEN BEGIN
   xtex = textoidl('\lambda_{rest} ['+angstrom+']')
   ytex = textoidl('F_{\lambda} [10^{-16} ergs cm^{-2} s^{-1}'+angstrom+'^{-1}]')
ENDIF ELSE BEGIN
   xtex = textoidl('\lambda_{rest} ['+angstrom+']')
   ytex = textoidl('F_{\nu} [10^{-11} ergs cm^{-2} s^{-1} Hz^{-1}]')
ENDELSE
plot, x, y, $
      xtitle = xtex, $
      ytitle = ytex, $
      /xsty, /ysty, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      position = aspect(1.0), $
      thick=2, $
      charsize = csize
FOR i=0,n_elements(lines)-1 DO BEGIN
   yloc = fidloc-0.2*i
   xyouts, lines[i], yloc, charsize=lsize, textoidl(nlines[i]), alignment=0.5
   x = replicate(lines[i],10)
   y = maken(-1d4,yloc,10)
   oplot, x, y, linestyle=1, thick=1
ENDFOR
device, /close
set_plot,'X'

END
