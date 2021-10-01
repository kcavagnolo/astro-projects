PRO age

readcol, 'dat/pv.dat', FORMAT='A,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F', $
         pvcluster, pv, pvmin, pvmax, pcav, pcavmin, pcavmax, $
         lx, lxmin, lxmax, mcool, mcoolmin, mcoolmax, lcool, $
         lcoolmin, lcoolmax, rcool, tcs, tfill, tbuoy
readcol, 'dat/pf_radio.dat', FORMAT='A,I,A,F,F,F,F',$
         radcluster, obsids, found, z, flux, ferr, lum, lumerr, comment='#'

ages = tbuoy
FOR i=0,n_elements(pvcluster)-1 DO BEGIN
    name = strcompress(pvcluster[i],/remove_all)
    get = where(radcluster EQ name)
    get = get[0]
    IF get EQ -1 THEN BEGIN
        print, 'No ',name,' in pf_radio.dat'
        GOTO,ERROR
    ENDIF
    IF ages[i] LE 0. THEN GOTO,ERROR
    push, rlum, lum[get]
    push, age, ages[i]
    IF found[get] EQ 'NF' THEN BEGIN
        push, lumll, lum[get]
        push, agell, ages[i]
    ENDIF
    print, format='(A-20,F10.2,F10.2,A6)',name,ages[i],lum[get],found[get]
ERROR:
ENDFOR

loadct, 13
set_plot, 'PS'
device, $
  filename = 'age.ps', $
  /color, $
  /encapsulated, $
  /portrait, $
  /helvetica
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 3
!Y.THICK  = 3
!Z.THICK  = 3

xmin = 0.8*min(age)
xmax = 1.2*max(age)
ymin = 0.8*min(rlum)
ymax = 1.2*max(rlum)
xtex = textoidl('Age 10^7 yr')
ytex = textoidl('L_{1.0MHz-5.0GHz} [10^{40} ergs sec^{-1}]')

plotsym, 0, 0.8, /fill
plot, age, rlum, $
      linestyle = 0, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax],$
      xtitle = xtex, $
      ytitle = ytex, $
      /xsty, /ysty, $
      /xlog, /ylog, $
      charsize = 0.8, $
      psym = 8
plotsym, 1, 2
oplot, agell, lumll, psym=8
device,/close
set_plot, "X"

END
