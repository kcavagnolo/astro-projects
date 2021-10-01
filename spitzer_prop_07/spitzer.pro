PRO spitzer

type = 'flat'
csize = 1.0

loadct, 13
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 3
!Y.THICK  = 3
!Z.THICK  = 3

readcol, 'spitzer.table', $
         FORMAT='A,A,A,F,F,F,F,D,D,A,D,A,A,A,A,A', $
         cluster, ra, dec, z, tx, k0itpl, k0flat, $
         lx, lha, hatype, lrad, radtype, $
         irac, mips, irs, data, comment='#'
IF type EQ 'itpl' THEN k0=k0itpl ELSE k0=k0flat

OPENW, /GET_LUN, LOGLUN, 'out'
printf, LOGLUN, format='(A-20, A15, A15, A8, A8, A10, A10, A10, A10, A6, A10, A6, A6, A6, A6, A6)', $
        "#Name","RA","Dec","z","TX","K0itpl","K0flat","LX","LHa","Type","LRad","Type","IRAC","MIPS","IRS","Data?"
printf, LOGLUN, format='(A-20, A15, A15, A8, A8, A10, A10, A10, A10, A6, A10, A6, A6, A6, A6, A6)', $
        "#---","h:m:s","d:m:s","---","keV","keV cm^2","keV cm^2","ergs/s","ergs/s","---","ergs/s","---","---","---","---","---"
;ord = where((cluster EQ 'ABELL_0119') OR $
;            (cluster EQ 'ABELL_0401') OR $
;            (cluster EQ 'ABELL_2142') OR $
;            (cluster EQ 'ABELL_2063') OR $
;            (cluster EQ 'ABELL_1650') OR $
;            (cluster EQ 'ABELL_2107'))
;ord = where((z LT 0.09) AND (z GT 0.02) AND (lx GE 1d44) AND (lrad LE 1d40) AND (hatype EQ 'NF'))
for j=0,n_elements(dec)-1 do begin
    tdec = split(':',dec[j])
    push, ttdec, float(tdec[0])
endfor
;ord = where((ttra LE 4.) AND (ttra GE 3.) AND (hatype EQ 'UK'))
ord = where((ttdec LE 20.) AND (hatype NE 'F') AND (data EQ 'Y'))
;keys = reverse(sort(k0[ord]))
keys = reverse(sort(ra[ord]))
ord = ord[keys]
prevname = 'fjdhfd'
counter = 0
FOR i=0,n_elements(ord)-1 DO BEGIN
    temp = ord[i]
    IF cluster[temp] NE prevname THEN BEGIN
        counter++
        push, names, num2str(counter)+': '+cluster[temp]
        push, nums, '  '+num2str(counter)
        printf, LOGLUN, format='(A-20,A15,A15,F8.4, F8.2, F10.2, F10.2, E10.2, E10.2, A6, E10.2, A6, A6, A6, A6, A6)', $
          cluster[temp], ra[temp], dec[temp], z[temp], tx[temp], $
          k0itpl[temp], k0flat[temp], lx[temp], lha[temp], hatype[temp], $
          lrad[temp], radtype[temp], irac[temp], mips[temp], irs[temp], data[temp]
    ENDIF ELSE BEGIN
        push, names, ''
        push, nums, ''
    ENDELSE
    prevname = cluster[temp]
ENDFOR
FREE_LUN, LOGLUN

set_plot, 'PS'
device, $
  filename = 'new_obs_k0z.eps', $
  /color, $
  /encapsulated, $
  /portrait, $
  /helvetica
plotPosition = ASPECT(1.0)
plotsym, 0, 0.6, /fill
plot, z, k0, $
      linestyle = 0, $
      xrange = [0.01,1.0], $
      yrange = [1.,550],$
      xtitle = textoidl('Redshift'), $
      ytitle = textoidl('K_0 [keV cm^2]'), $
      /xsty, /ysty, $
      /xlog, /ylog, $
      charsize = csize, $
      POSITION = plotPosition, $
      psym = 8
plotsym, 8, 1.2, /fill
oplot, z[ord], k0[ord], psym=8, color=250
plotsym, 3, 0.7, /fill
oplot, z[ord], k0[ord], psym=8, color=200
names = str_replace(names,'_',' ')
names = str_replace(names,'ABELL','Abell')
goo = where(names NE '')
items = [names[goo]]
linearr = replicate(-99,n_elements(items))
psyarr = replicate(-99,n_elements(items))
legend, items, linestyle=linearr, psym=psyarr, charsize=csize, spacing=0.6, $
        /bottom, box=0, /right_legend
xyouts, z[ord], k0[ord], nums, charsize=csize, charthick=2
device,/close
void, names

lx = lx/1d44
lha = lha/1d40
lrad = lrad/1d42

FOR i=0,n_elements(cluster)-1 DO BEGIN
    name = strcompress(cluster[i],/remove_all)

    ;# LX
    IF lx[i] GT 0. THEN BEGIN
        IF irac[i] EQ 'Y' THEN BEGIN
            push, iraclx, lx[i]
            push, iraczlx, z[i]
        ENDIF
        IF mips[i] EQ 'Y' THEN BEGIN
            push, mipslx, lx[i]
            push, mipszlx, z[i]
        ENDIF
        IF irs[i] EQ 'Y' THEN BEGIN
            push, irslx, lx[i]
            push, irszlx, z[i]
        ENDIF
        push, alllx, lx[i]
        push, allzlx, z[i]
    ENDIF

    ;# K0
    IF k0[i] GT 0. THEN BEGIN
        IF irac[i] EQ 'Y' THEN BEGIN
            push, iraczk0, z[i]
            push, irack0, k0[i]
        ENDIF
        IF mips[i] EQ 'Y' THEN BEGIN
            push, mipszk0, z[i]
            push, mipsk0, k0[i]
        ENDIF
        IF irs[i] EQ 'Y' THEN BEGIN
            push, irszk0, z[i]
            push, irsk0, k0[i]
        ENDIF
        push, allzk0, z[i]
        push, allk0, k0[i]
        push, names, name
    ENDIF

    ;# LHa
    IF lha[i] GT 0. THEN BEGIN
        IF irac[i] EQ 'Y' THEN BEGIN
            push, iraczlha, z[i]
            push, iraclha, lha[i]
        ENDIF
        IF mips[i] EQ 'Y' THEN BEGIN
            push, mipszlha, z[i]
            push, mipslha, lha[i]
        ENDIF
        IF irs[i] EQ 'Y' THEN BEGIN
            push, irszlha, z[i]
            push, irslha, lha[i]
        ENDIF
        push, allzlha, z[i]
        push, alllha, lha[i]
        IF hatype[i] EQ 'NF' THEN BEGIN
            push, hanf, lha[i]
            push, zhanf, z[i]
        ENDIF
    ENDIF

    ;# Lradio
    IF lrad[i] GT 0. THEN BEGIN
        IF irac[i] EQ 'Y' THEN BEGIN
            push, iraczlrad, z[i]
            push, iraclrad, lrad[i]
        ENDIF
        IF mips[i] EQ 'Y' THEN BEGIN
            push, mipszlrad, z[i]
            push, mipslrad, lrad[i]
        ENDIF
        IF irs[i] EQ 'Y' THEN BEGIN
            push, irszlrad, z[i]
            push, irslrad, lrad[i]
        ENDIF
        push, allzlrad, z[i]
        push, alllrad, lrad[i]
        IF radtype[i] EQ 'NF' THEN BEGIN
            push, radnf, lrad[i]
            push, zradnf, z[i]
        ENDIF
    ENDIF

    ;# LHa-K0
    IF ((lha[i] GT 0.) AND (k0[i] GT 0.)) THEN BEGIN
;        IF (k0[i] LT 30. AND hatype[i] EQ 'NF') THEN print, name
        IF irac[i] EQ 'Y' THEN BEGIN
            push, irack0lha, k0[i]
            push, iraclhak0, lha[i]
        ENDIF
        IF mips[i] EQ 'Y' THEN BEGIN
            push, mipsk0lha, k0[i]
            push, mipslhak0, lha[i]
        ENDIF
        IF irs[i] EQ 'Y' THEN BEGIN
            push, irsk0lha, k0[i]
            push, irslhak0, lha[i]
        ENDIF
        push, allk0lha, k0[i]
        push, alllhak0, lha[i]
        IF hatype[i] EQ 'NF' THEN BEGIN
            push, k0lhanf, k0[i]
            push, lhak0nf, lha[i]
        ENDIF
    ENDIF

    ;# Lrad-K0
    IF ((lrad[i] GT 0.) AND (k0[i] GT 0.)) THEN BEGIN
        IF (k0[i] LT 40. AND lrad[i] GE 10.) THEN print, name
        IF irac[i] EQ 'Y' THEN BEGIN
            push, irack0lrad, k0[i]
            push, iraclradk0, lrad[i]
        ENDIF
        IF mips[i] EQ 'Y' THEN BEGIN
            push, mipsk0lrad, k0[i]
            push, mipslradk0, lrad[i]
        ENDIF
        IF irs[i] EQ 'Y' THEN BEGIN
            push, irsk0lrad, k0[i]
            push, irslradk0, lrad[i]
        ENDIF
        push, allk0lrad, k0[i]
        push, alllradk0, lrad[i]
        IF radtype[i] EQ 'NF' THEN BEGIN
            push, k0lradnf, k0[i]
            push, lradk0nf, lrad[i]
        ENDIF
    ENDIF
ENDFOR

set_plot, 'PS'
device, $
  filename = 'spitzer_lx.eps', $
  /color, $
  /encapsulated, $
  /portrait, $
  /helvetica
xmin = 0.8*min(allzlx)
xmax = 1.2*max(allzlx)
ymin = 0.8*min(alllx)
ymax = 1.2*max(alllx)
xtex = textoidl('Redshift')
ytex = textoidl('L_X [10^{44} ergs sec^{-1}]')
plotPosition = ASPECT(1.0)
plotsym, 0, 0.4, /fill
plot, allzlx, alllx, $
      linestyle = 0, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax],$
      xtitle = xtex, $
      ytitle = ytex, $
      /xsty, /ysty, $
      /xlog, /ylog, $
      charsize = csize, $
      POSITION = plotPosition, $
      psym = 8
plotsym, 8, 1.0, thick=2
oplot, mipszlx, mipslx, psym=8, color=250
plotsym, 0, 0.8, thick=2
oplot, iraczlx, iraclx, psym=8, color=50
plotsym, 4, 1.2, thick=2
oplot, irszlx, irslx, psym=8, color=150
num = strcompress(round(n_elements(alllx)),/remove_all)
mipsnum = strcompress(round(n_elements(mipslx)),/remove_all)
iracnum = strcompress(round(n_elements(iraclx)),/remove_all)
irsnum = strcompress(round(n_elements(irslx)),/remove_all)
items = [textoidl('Clusters: '+num),$
         textoidl('MIPS: '+mipsnum),$
         textoidl('IRAC: '+iracnum),$
         textoidl('IRS: '+irsnum)]
linearr = replicate(-99,n_elements(items))
psyarr = replicate(-99,n_elements(items))
legend, items, linestyle=linearr, psym=psyarr, charsize=csize, $
        /bottom, box=0, /right_legend
device,/close

ord = where((names EQ 'ABELL_0119') OR $
            (names EQ 'ABELL_0401') OR $
            (names EQ 'ABELL_2142') OR $
            (names EQ 'ABELL_2063') OR $
            (names EQ 'ABELL_1650') OR $
            (names EQ 'ABELL_2107'))
specx = allzk0[ord]
specy = allk0[ord]
set_plot, 'PS'
device, $
  filename = 'spitzer_k0.eps', $
  /color, $
  /encapsulated, $
  /portrait, $
  /helvetica
xmin = 0.8*min(allzk0)
xmax = 1.2*max(allzk0)
ymin = 0.8*min(allk0)
ymax = 1.2*max(allk0)
xtex = textoidl('Redshift')
ytex = textoidl('K_0 [keV cm^2]')
plotsym, 0, 0.4, /fill
plot, allzk0, allk0, $
      linestyle = 0, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax],$
      xtitle = xtex, $
      ytitle = ytex, $
      /xsty, /ysty, $
      /xlog, /ylog, $
      charsize = csize, $
      POSITION = plotPosition, $
      psym = 8
plotsym, 8, 1.0, thick=2
oplot, mipszk0, mipsk0, psym=8, color=250
plotsym, 0, 0.8, thick=2
oplot, iraczk0, irack0, psym=8, color=50
;plotsym, 4, 1.2, thick=2
;oplot, irszk0, irsk0, psym=8, color=150
plotsym, 8, 1.2, /fill
oplot, specx, specy, psym=8, color=250
plotsym, 3, 0.7, /fill
oplot, specx, specy, psym=8, color=200
num = strcompress(round(n_elements(allk0)),/remove_all)
mipsnum = strcompress(round(n_elements(mipsk0)),/remove_all)
iracnum = strcompress(round(n_elements(irack0)),/remove_all)
irsnum = strcompress(round(n_elements(irsk0)),/remove_all)
items = [textoidl('Clusters: '+num),$
         textoidl('MIPS: '+mipsnum),$
         textoidl('IRAC: '+iracnum)]
;         textoidl('IRS: '+irsnum)]
linearr = replicate(-99,n_elements(items))
psyarr = replicate(-99,n_elements(items))
legend, items, linestyle=linearr, psym=psyarr, charsize=csize, $
        /bottom, box=0, /right_legend
device,/close

set_plot, 'PS'
device, $
  filename = 'spitzer_lha.eps', $
  /color, $
  /encapsulated, $
  /portrait, $
  /helvetica
xmin = 0.8*min(allzlha)
xmax = 1.2*max(allzlha)
ymin = 0.6*min(alllha)
ymax = 1.2*max(alllha)
xtex = textoidl('Redshift')
ytex = textoidl('L_{H\alpha} [10^{40} ergs sec^{-1}]')
plotsym, 0, 0.4, /fill
plot, allzlha, alllha, $
      linestyle = 0, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax],$
      xtitle = xtex, $
      ytitle = ytex, $
      /xsty, /ysty, $
      /xlog, /ylog, $
      charsize = csize, $
      POSITION = plotPosition, $
      psym = 8
plotsym, 8, 1.0, thick=2
oplot, mipszlha, mipslha, psym=8, color=250
plotsym, 0, 0.8, thick=2
oplot, iraczlha, iraclha, psym=8, color=50
plotsym, 4, 1.2, thick=2
oplot, irszlha, irslha, psym=8, color=150
plotsym, 1, 2.0, thick=2
oplot, zhanf, hanf, psym=8
num = strcompress(round(n_elements(alllha)),/remove_all)
mipsnum = strcompress(round(n_elements(mipslha)),/remove_all)
iracnum = strcompress(round(n_elements(iraclha)),/remove_all)
irsnum = strcompress(round(n_elements(irslha)),/remove_all)
items = [textoidl('Clusters: '+num),$
         textoidl('MIPS: '+mipsnum),$
         textoidl('IRAC: '+iracnum),$
         textoidl('IRS: '+irsnum)]
linearr = replicate(-99,n_elements(items))
psyarr = replicate(-99,n_elements(items))
legend, items, linestyle=linearr, psym=psyarr, charsize=csize, $
        /top, box=0, /left
device,/close

set_plot, 'PS'
device, $
  filename = 'spitzer_lrad.eps', $
  /color, $
  /encapsulated, $
  /portrait, $
  /helvetica
xmin = 0.7*min(allzlrad)
xmax = 1.2*max(allzlrad)
ymin = 0.8*min(alllrad)
ymax = 1.2*max(alllrad)
xtex = textoidl('Redshift')
ytex = textoidl('L_{Radio} [10^{42} ergs sec^{-1}]')
plotsym, 0, 0.4, /fill
plot, allzlrad, alllrad, $
      linestyle = 0, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax],$
      xtitle = xtex, $
      ytitle = ytex, $
      /xsty, /ysty, $
      /xlog, /ylog, $
      charsize = csize, $
      POSITION = plotPosition, $
      psym = 8
plotsym, 8, 1.0, thick=2
oplot, mipszlrad, mipslrad, psym=8, color=250
plotsym, 0, 0.8, thick=2
oplot, iraczlrad, iraclrad, psym=8, color=50
plotsym, 4, 1.2, thick=2
oplot, irszlrad, irslrad, psym=8, color=150
plotsym, 1, 2.0, thick=2
oplot, zradnf, radnf, psym=8
num = strcompress(round(n_elements(alllrad)),/remove_all)
mipsnum = strcompress(round(n_elements(mipslrad)),/remove_all)
iracnum = strcompress(round(n_elements(iraclrad)),/remove_all)
irsnum = strcompress(round(n_elements(irslrad)),/remove_all)
items = [textoidl('Clusters: '+num),$
         textoidl('MIPS: '+mipsnum),$
         textoidl('IRAC: '+iracnum),$
         textoidl('IRS: '+irsnum)]
linearr = replicate(-99,n_elements(items))
psyarr = replicate(-99,n_elements(items))
legend, items, linestyle=linearr, psym=psyarr, charsize=csize, $
        /top, box=0, /left_legend
device,/close

set_plot, 'PS'
device, $
  filename = 'spitzer_hak0.eps', $
  /color, $
  /encapsulated, $
  /portrait, $
  /helvetica
xmin = 0.7*min(alllhak0)
xmax = 1.2*max(alllhak0)
ymin = 0.8*min(allk0lha)
ymax = 1.2*max(allk0lha)
ytex = textoidl('K_0 [keV cm^2]')
xtex = textoidl('L_{H\alpha} [10^{40} ergs sec^{-1}]')
plotsym, 0, 0.4, /fill
plot, alllhak0, allk0lha, $
      linestyle = 0, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax],$
      xtitle = xtex, $
      ytitle = ytex, $
      /xsty, /ysty, $
      /xlog, /ylog, $
      charsize = csize, $
      POSITION = plotPosition, $
      psym = 8
plotsym, 8, 1.0, thick=2
oplot, mipslhak0, mipsk0lha, psym=8, color=250
plotsym, 0, 0.8, thick=2
oplot, iraclhak0, irack0lha, psym=8, color=50
plotsym, 4, 1.2, thick=2
oplot, irslhak0, irsk0lha, psym=8, color=150
plotsym, 6, 2.0, thick=2
oplot, lhak0nf, k0lhanf, psym=8
num = strcompress(round(n_elements(alllhak0)),/remove_all)
mipsnum = strcompress(round(n_elements(mipslhak0)),/remove_all)
iracnum = strcompress(round(n_elements(iraclhak0)),/remove_all)
irsnum = strcompress(round(n_elements(irslhak0)),/remove_all)
items = [textoidl('Clusters: '+num),$
         textoidl('MIPS: '+mipsnum),$
         textoidl('IRAC: '+iracnum),$
         textoidl('IRS: '+irsnum)]
linearr = replicate(-99,n_elements(items))
psyarr = replicate(-99,n_elements(items))
legend, items, linestyle=linearr, psym=psyarr, charsize=csize, $
        /top, box=0, /right_legend
device,/close

set_plot, 'PS'
device, $
  filename = 'spitzer_radk0.eps', $
  /color, $
  /encapsulated, $
  /portrait, $
  /helvetica
xmin = 0.7*min(alllradk0)
xmax = 1.2*max(alllradk0)
ymin = 0.8*min(allk0lrad)
ymax = 1.2*max(allk0lrad)
ytex = textoidl('K_0 [keV cm^2]')
xtex = textoidl('L_{Radio} [10^{42} ergs sec^{-1}]')
plotsym, 0, 0.4, /fill
plot, alllradk0, allk0lrad, $
      linestyle = 0, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax],$
      xtitle = xtex, $
      ytitle = ytex, $
      /xsty, /ysty, $
      /xlog, /ylog, $
      charsize = csize, $
      POSITION = plotPosition, $
      psym = 8
plotsym, 8, 1.0, thick=2
oplot, mipslradk0, mipsk0lrad, psym=8, color=250
plotsym, 0, 0.8, thick=2
oplot, iraclradk0, irack0lrad, psym=8, color=50
plotsym, 4, 1.2, thick=2
oplot, irslradk0, irsk0lrad, psym=8, color=150
plotsym, 6, 2.0, thick=2
oplot, lradk0nf, k0lradnf, psym=8
num = strcompress(round(n_elements(alllradk0)),/remove_all)
mipsnum = strcompress(round(n_elements(mipslradk0)),/remove_all)
iracnum = strcompress(round(n_elements(iraclradk0)),/remove_all)
irsnum = strcompress(round(n_elements(irslradk0)),/remove_all)
items = [textoidl('Clusters: '+num),$
         textoidl('MIPS: '+mipsnum),$
         textoidl('IRAC: '+iracnum),$
         textoidl('IRS: '+irsnum)]
linearr = replicate(-99,n_elements(items))
psyarr = replicate(-99,n_elements(items))
legend, items, linestyle=linearr, psym=psyarr, charsize=csize, $
        /top, box=0, /right_legend
device,/close

set_plot, "X"

END
