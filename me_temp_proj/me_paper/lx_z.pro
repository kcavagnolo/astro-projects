pro lx_z

myhome = GETENV('HOME')
reflist = 'full_sample_L-50.list'
output = 'lx_z.eps'
plname = "no"
csize = 0.9
mpcm = double(3.0856e24)        ; Mpc to meters
norm = 1.d44                    ; normalization to 10**44

restore, myhome+"/research/redux/scripts/full_sample.sav"
ref = read_ascii(reflist, template = full_sample)

plz = maken(0.001,2,500)
flux  = 4*!pi*2.5e-15
flux1 = flux*10
flux2 = flux*100
flux3 = flux*1000
flux4 = flux*10000

FOR i=0,n_elements(plz)-1 DO BEGIN
    cosmology,plz[i],result,/silent
    pllx  = (flux*(result[2]*mpcm)^2.)/norm
    pllx1 = (flux1*(result[2]*mpcm)^2.)/norm
    pllx2 = (flux2*(result[2]*mpcm)^2.)/norm
    pllx3 = (flux3*(result[2]*mpcm)^2.)/norm
    pllx4 = (flux4*(result[2]*mpcm)^2.)/norm

    push, pllum, pllx
    push, pllum1, pllx1
    push, pllum2, pllx2
    push, pllum3, pllx3
    push, pllum4, pllx4

ENDFOR

FOR i=0,n_elements(ref.z)-1 DO BEGIN
    IF (ref.lx[i] gt 0.) THEN BEGIN
       myname = ref.name[i]
       lum = ref.lx[i]
       lumlo = ref.lxlo[i]
       lumhi = ref.lxhi[i]
       cosmology,ref.z[i],result,/silent
       fidlx = (flux1*(result[2]*mpcm)^2.)/norm
       IF (lum LT fidlx AND plname EQ "yes") THEN BEGIN
          name = ref.name[i]
       ENDIF else BEGIN
          name = ""
       ENDELSE

       ; build arrays
        check = STREGEX(myname, 'dag$', /FOLD_CASE)  
        IF check EQ -1 THEN BEGIN
           push,lx,lum
           push,lxlo,lum-lumlo
           push,lxhi,lumhi-lum
           push,redsh,ref.z[i]
           push,names,name
        ENDIF ELSE BEGIN
           push,r5lx,lum
           push,r5lxlo,lum-lumlo
           push,r5lxhi,lumhi-lum
           push,r5redsh,ref.z[i]
           push,r5names,name
        ENDELSE
    ENDIF
ENDFOR

xtx = textoidl("Redshift")
ytx = textoidl("L_{bol} [10^{44}ergs sec^{-1}]")

odevice = !d.name
set_plot, 'PS'
device, filename = output, $
        /encapsulated, $
        /portrait, $
        /helvetica
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 3
!Y.THICK  = 3
!Z.THICK  = 3
xmin = 0.3*min(redsh)
xmax = 1.05*max(redsh)
ymin = 0.4*min(lx)
ymax = 1.2*max(lx)

plotsym, 0, 0.75, /fill
plot, redsh, lx, $
      /ylog, $
      psym = 8, $
      xtitle = xtx, $
      ytitle = ytx, $
      /xsty, /ysty, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      charsize = csize
oplot, plz, pllum, psym=0, linestyle=1
oplot, plz, pllum1, psym=0, linestyle=1
oplot, plz, pllum2, psym=0, linestyle=1
oplot, plz, pllum3, psym=0, linestyle=1
oplot, plz, pllum4, psym=0, linestyle=1
oploterror, redsh, lx, lxlo, psym=8, /lobar
oploterror, redsh, lx, lxhi, psym=8, /hibar
xyouts, redsh, lx, names, charsize=0.5, align=0.5
plotsym, 3, 0.85
oplot, r5redsh, r5lx, psym=8
oploterror, r5redsh, r5lx, r5lxlo, psym=8, /lobar
oploterror, r5redsh, r5lx, r5lxhi, psym=8, /hibar

device, /close
set_plot, odevice    

END
