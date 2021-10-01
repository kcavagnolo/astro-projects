pro asca_chan

output = 'asch.eps'
csize = 1.0

; input files array
file = 'dat/culled_r2500_7-7.dat'
afile = 'dat/asca.dat'

restore,"ascadat.sav"
asca = read_ascii(afile,template=ascadat)
restore,"~/research/redux/scripts/xspectemp_rin_normerr_src.sav"
chan = read_ascii(file, template=xspectemp_rin_normerr_src)

atx    = asca.tx
alo    = asca.tlo
ahi    = asca.thi
aerrlo = atx-alo
aerrhi = ahi-atx
anames = asca.cluster

rawnh  = chan.nh
ctx    = chan.tx
ctlo   = chan.tlo
cthi   = chan.thi
cerrlo = ctx-ctlo
cerrhi = cthi-ctx
cnames = chan.cluster

;; empty arrays
void, ascatx
void, ascalo
void, ascahi
void, chantx
void, chanlo
void, chanhi
void, names
void, plname
void, ctemps
void, atemps

FOR i=0,n_elements(anames)-1 DO BEGIN
    ord = where(cnames EQ anames[i])
    IF ord GT 0 THEN BEGIN
        push, ascatx, atx[i]
        push, ascalo, aerrlo[i]
        push, ascahi, aerrhi[i]
        push, chantx, ctx[ord]
        push, chanlo, cerrlo[ord]
        push, chanhi, cerrhi[ord]
        push, nh, rawnh[ord]
        push, names, anames[i]
    ENDIF
ENDFOR
print, n_elements(names)
diff = chantx/ascatx
difflo = diff*sqrt((ascalo/ascatx)^2.+(chanlo/chantx)^2.)
diffhi = diff*sqrt((ascahi/ascatx)^2.+(chanhi/chantx)^2.)

;; grab clusters which have variations > 2-sigma
FOR i=0,n_elements(difflo)-1 DO BEGIN
    IF ((diff[i]-2*difflo[i] GE 1.0) OR $
        (diff[i]+2*diffhi[i] LE 1.0)) THEN BEGIN
        push, plname, names[i]
        push, ctemps, chantx[i]
        push, atemps, ascatx[i]
    ENDIF ELSE BEGIN
        push, plname, " "
        push, ctemps, 0.0
        push, atemps, 0.0
    ENDELSE
ENDFOR

;; plotting input
x    = chantx
xlo  = chanlo
xhi  = chanhi
y    = diff
ylo  = difflo
yhi  = diffhi
;y    = ascatx
;ylo  = ascalo
;yhi  = ascahi
xmin = min(x-xlo)-0.1*min(x-xlo)
xmax = max(x+xhi)+0.05*max(x+xhi)
ymin = min(y-ylo)-0.1*min(y-ylo)
ymax = max(y+yhi)+0.05*max(y+yhi)

;; plotting commands
xtex = textoidl('T_{Chandra} [keV]')
ytex = textoidl('T_{Chandra}/T_{ASCA}')
plotsym, 0, 0.5, /fill
odevice = !d.name
set_plot, 'PS'
device, filename = output, /encapsulated
!fancy = 4
!linetype = 0
!p.font = 0
plot, x, y, $
  psym = 8, $
  /xlog, /xsty, /ysty, $
  xran = [xmin,xmax], $
  yran = [ymin,ymax], $
  charsize = csize, $
  xtitle = xtex, $
  ytitle = ytex

;; plot errors
oploterror, x, y, xlo, ylo, /lobar, psym=3
oploterror, x, y, xhi, yhi, /hibar, psym=3
xov = findgen(100)
oplot, xov, replicate(1.0,n_elements(xov)), linestyle=2

;; plot names of hot clusters
;strreplace, plname, '_', ' '
;xyouts, x-0.1, y+0.1, plname, charthick=0.8, charsize=0.50, align=1.0

;; print names and temps
ord = where(plname NE " ")
a = plname[ord]
b = ctemps[ord]
c = atemps[ord]
FOR g = 0,n_elements(a)-1 DO BEGIN
    print, format='(A-20,F10.3,F10.3)', a[g],b[g],c[g]
ENDFOR
device,/close

device, filename = 'aschnh.eps', /encapsulated
!fancy = 4
!linetype = 0
!p.font = 0
plot, nh, y, $
  psym = 8, $
  /xlog, /xsty, /ysty, $
  xrange = [0.6,max(nh)+0.1*max(nh)], $
  yran = [ymin,ymax], $
  charsize = csize, $
  xtitle = textoidl('N_{H} [10^{20} cm^{-2}]'), $
  ytitle = ytex

;; plot errors
oploterror, nh, y, xlo, ylo, /lobar, psym=3
oploterror, nh, y, xhi, yhi, /hibar, psym=3
xov = findgen(100)
oplot, xov, replicate(1.0,n_elements(xov)), linestyle=2

set_plot, odevice

END
