pro plot_plaws, dat

output = 'all_plaws.ps'
boe = 1.                       ; T-min
que = 20.                       ; T-max

; restore the fit template and read some variables
restore,"plaws_template.sav"
plaws = read_ascii(dat, template = plaws_template)

FOR i=0,n_elements(plaws.iter)-1 DO BEGIN
    IF (plaws.iter[i] EQ 0) THEN BEGIN
        IF (n_elements(Aarr) EQ 0) THEN BEGIN
            Aarr = plaws.A[i]
            Barr = plaws.B[i]
            runname = plaws.runname[i]
        ENDIF ELSE BEGIN
            Aarr = [Aarr,plaws.A[i]]
            Barr = [Barr,plaws.B[i]]
            runname = [runname,plaws.runname[i]]
        ENDELSE
    ENDIF
ENDFOR

; define the color array
ncl = n_elements(Aarr)
color = (fltarr(ncl)+0.5)*(!d.n_colors)

; set up empty plot
set_plot, 'PS'
device, filename = output, /color
!fancy = 4
!p.font=0
csize = 1.0
lgsize = 0.75*csize
lsize = 0.5
xrange=[boe,que]
yrange=[boe,que]
xtx = textoidl("kT_{[2.0-7.0]} keV")
ytx = textoidl("kT_{[0.7-7.0]} keV")
plot, xrange, yrange, /nodata, linestyle=0, xrange=xrange, yrange=yrange,$
      xtitle=xtx, ytitle=ytx, /xlog, /ylog, xsty=1, ysty=1, charsize=csize, thick=lsize

FOR i=0,n_elements(Aarr)-1 DO BEGIN
    A = Aarr[i]
    B = Barr[i]
    loadct,13
    plcolor = (((B-0.5)/(A-0.5))^2.)*(color[i]*sqrt((B-0.5)/(A-0.5)))
    IF (n_elements(legcolor) EQ 0) THEN BEGIN
        legcolor = plcolor
    ENDIF ELSE BEGIN
        legcolor = [legcolor, plcolor]
    ENDELSE
    symbols = [0,3,4,5,8,0,3,4,5,8,0,3,4,5,8,0,3,4,5,8,0,3,$
               4,5,8,0,3,4,5,8,0,3,4,5,8,0,3,4,5,8,0,3,4,5,$
               8,0,3,4,5,8,0,3,4,5,8,0,3,4,5,8,0,3,4,5,8,$
               0,3,4,5,8,0,3,4,5,8]
    x = findgen(que*5)+boe
    y = A*x^B
    oplot, x, y, psym=0, linestyle=0, color=plcolor, thick=1.5

ENDFOR

; draw the legend
equa = textoidl('Best Fit: kT_{[0.7-7.0]} = \alpha kT_{[2.0-7.0]}^{\beta}')
loe = textoidl('Line of equality')
me = textoidl('ME01 Prediction')
items = [equa,loe,me]
linearr = [-99,2,1]
psyarr = [-99,0,0]
legend, items, linestyle=linearr, psym=psyarr, charsize=lgsize, /top, /left_legend

; draw the legend
legend, textoidl(runname)+textoidl(' \alpha = ')+strcompress(sigfig(Aarr,3),/remove_all)+' ; '+textoidl('\beta = ')+strcompress(sigfig(Barr,3),/remove_all)+'   ', psym=intarr(ncl)+8, /usersym, color=legcolor, charsize=lgsize, spacing=0.6, /bottom, /right, /fill

; overplot the line x=y
red = REFORM([255,0,0],1,3)
TVLCT, red, 100
y = x
oplot, x, y, linestyle=2, psym=0, thick=3.0

; overplot the ME prediction
y = 0.81*y^(1.09)
oplot, x, y, linestyle = 1, thick=3.0, psym=0

device, /close

END


