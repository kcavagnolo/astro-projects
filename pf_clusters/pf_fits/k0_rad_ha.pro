pro k0_rad_ha, indata

;# options
!QUIET = 1                      ;# stop the screen vomit, =0 keeps it
k0val  = 'fit'                  ;# type of data to use: 'obs' or 'fit'
ktype  = 'flat'                 ;# type of K to use: 'flat' or 'itpl'
model  = 'nonzero'              ;# type of model to use: 'zero' or 'nonzero'
csize  = 0.8                    ;# size of characters in plots
psize  = 0.6                    ;# size of symbols in plots
raddata = 'dat/pf_radio.dat'    ;# file containing radio data
hadata  = 'dat/pf_halum.dat'    ;# file containing h-alpha data

;# read in data files
restore,'~/research/redux/scripts/s_tabletemplate.sav'
restore,'~/research/redux/scripts/s_resultstemplate.sav'
readcol, indata,FORMAT='A,A,F,F,F,F,F,F,F,F,F,A,F,A,F,A',$
         cluster,obsids,x,y,rmax,mincts,z,nh,tx,fe,lbol,chip,eobs,diff,robs,loc
readcol, raddata, FORMAT='A,A,A,F,F,F,F',$
         radcluster, radobsids, radfound, radz, radflux, radferr, radlum, radlumerr, comment='#'
readcol, hadata, FORMAT='A,A,A,F,F,F,F',$
         hacluster, haobsids, hafound, haz, haflux, haferr, halum, halumerr, comment='#'

;# get values specific to model
IF (ktype EQ 'flat') THEN BEGIN
    IF (model EQ 'nonzero') THEN ind = 2 ELSE ind = 3
ENDIF
IF (ktype EQ 'itpl') THEN BEGIN
    IF (model EQ 'nonzero') THEN ind = 0 ELSE ind = 1
ENDIF

;# begin looping
FOR i = 0,n_elements(obsids)-1 DO BEGIN
    obs = strcompress(obsids[i],/remove_all)
    name = strcompress(cluster[i],/remove_all)
    ord = where(cluster EQ name)
    IF n_elements(ord) NE 1 THEN BEGIN
        temp = obsids[ord]
        FOR j=1,n_elements(temp)-1 DO BEGIN
            obs = obs+'_'+strcompress(temp[j],/remove_all)
        ENDFOR
    ENDIF

    ;# get data values
    IF k0val EQ 'obs' THEN $
      file = '~/research/pf_clusters/pf_fits/tables/'+obs+'_table.dat' ELSE $
      file = '~/research/pf_clusters/pf_fits/s_results/'+obs+'_results.log'
    check = findfile(file,count=count)
    IF (count NE 1) THEN GOTO,ERROR

    ;# check for radio data
    radord = where(radcluster EQ name)
    radord = radord[0]
    IF radord EQ -1 THEN GOTO,ERROR

    ;# check for halpha data
    haord = where(hacluster EQ name)
    haord = haord[0]
    IF haord EQ -1 THEN GOTO,ERROR

    ;# get data
    IF k0val EQ 'obs' THEN BEGIN
        data = read_ascii(file, template = s_tabletemplate)
        IF (ktype EQ 'flat') THEN k = data.k_flat ELSE k = data.k
        ;# remember, they're reversed
        num = n_elements(k)
        k = k[num-1]
        kerr = data.k_err
        kerr = kerr[num-1]
    ENDIF ELSE BEGIN
        data = read_ascii(file, template = s_resultstemplate)
        k = data.k0
        k = k[ind]
        kerr = data.k0err
        kerr = kerr[ind]
    ENDELSE

    ;# build arrays by data type
    IF ((hafound[haord] EQ 'NF') AND (radfound[radord] EQ 'NF')) THEN BEGIN
        push, radno, radlum[radord]
        push, hano, halum[haord]
        push, kno, k
    ENDIF ELSE IF (hafound[haord] EQ 'NF') THEN BEGIN
        push, radnoha, radlum[radord]
        push, hanoha, halum[haord]
        push, knoha, k
    ENDIF ELSE IF (radfound[radord] EQ 'NF') THEN BEGIN
        push, radnorad, radlum[radord]
        push, hanorad, halum[haord]
        push, knorad, k
    ENDIF ELSE BEGIN
        push, radf, radlum[radord]
        push, haf, halum[haord]
        push, kf, k
    ENDELSE
    push, allrad, radlum[radord]
    push, allha, halum[haord]
    push, allk, k
ERROR:
ENDFOR

x = allrad
y = allha
z = allk
xtex = textoidl('L_{radio} [ergs s^{-1}]')
ytex = textoidl('L_{H\alpha} [ergs s^{-1}]')
ztex = textoidl('K_0 [keV cm^2]')
set_plot, 'PS'
loadct, 13, /silent
device, filename='k0_rad_ha.eps', $
        /color, $
        /encapsulated, $
        /landscape, $
        /helvetica
!P.MULTI = [0,2,2,0]
!FANCY    = 4
!P.FONT   = 0
!X.OMARGIN = [15,0]
!Y.OMARGIN = [0,0]
xmin = 0.5*min(x)
xmax = 1.5*max(x)
ymin = 0.5*min(y)
ymax = 1.5*max(y)
zmin = 0.5*min(z)
zmax = 1.5*max(z)

; Set the 3D coordinate space with axes.
SURFACE, dist(5), $
         /NODATA, $
         /SAVE, $
         XRANGE=[xmin,xmax], $
         YRANGE=[ymin,ymax], $
         ZRANGE=[zmin,zmax], $
         /XSTY, /YSTY, /ZSTY, $
         /XLOG, /YLOG, /ZLOG, $
         xtitle = textoidl('L_{radio}'), $
         ytitle = textoidl('L_{H\alpha}'), $
         ztitle = textoidl('K_0'), $
         CHARSIZE = 1.0, $
         XTICKLEN = 1, YTICKLEN=1, $
         XGRIDSTYLE=1, YGRIDSTYLE=1, $
         AX = 60
AXIS, XAXIS=1, /T3D, CHARSIZE=0.001
AXIS, YAXIS=1, /T3D, CHARSIZE=0.001

;# Found both
!X.THICK  = 3
!Y.THICK  = 3
!Z.THICK  = 3
plotsym, 0, psize, /fill
zcolors = BYTSCL(z, TOP=!D.N_COLORS-2)
PLOTS, x, y, z, PSYM=8, color=zcolors, /T3D
FOR j=0,n_elements(z)-1 DO PLOTS, [x(j), x(j)], [y(j), y(j)], [zmin, z(j)], color=zcolors(j), /T3D

plot, x, y, $
      /nodata, $
      XRANGE=[xmin,xmax], $
      YRANGE=[ymin,ymax], $
      /XSTY, /YSTY, $
      /XLOG, /YLOG, $
      xtitle = xtex, $
      ytitle = ytex, $
      CHARSIZE = csize
plotsym, 0, psize, /fill
OPLOT, radf, haf, PSYM=8
plotsym, 3, psize
OPLOT, radnoha, hanoha, PSYM=8
plotsym, 4, psize
OPLOT, radnorad, hanorad, PSYM=8
plotsym, 8, psize
OPLOT, radno, hano, PSYM=8

plot, x, z, $
      /nodata, $
      XRANGE=[xmin,xmax], $
      YRANGE=[zmin,zmax], $
      /XSTY, /YSTY, $
      /XLOG, /YLOG, $
      xtitle = xtex, $
      ytitle = ztex, $
      CHARSIZE = csize
plotsym, 0, psize, /fill
OPLOT, radf, kf, PSYM=8
plotsym, 3, psize
OPLOT, radnoha, knoha, PSYM=8
plotsym, 4, psize
OPLOT, radnorad, knorad, PSYM=8
plotsym, 8, psize
OPLOT, radno, kno, PSYM=8

; draw the legend
items = [textoidl('Circle: Radio and H\alpha'),$
         textoidl('Star: No H\alpha'), $
         textoidl('Triangle: No Radio'), $
         textoidl('Square: No Radio and No H\alpha')]
linearr = replicate(-99,n_elements(items))
psyarr  = replicate(-99,n_elements(items))
legend, items, linestyle=linearr, psym=psyarr, /top, box=0, /right_legend, charsize=csize, spacing=0.1

plot, y, z, $
      /nodata, $
      XRANGE=[ymin,ymax], $
      YRANGE=[zmin,zmax], $
      /XSTY, /YSTY, $
      /XLOG, /YLOG, $
      xtitle = ytex, $
      ytitle = ztex, $
      CHARSIZE = csize
plotsym, 0, psize, /fill
OPLOT, haf, kf, PSYM=8
plotsym, 3, psize
OPLOT, hanoha, knoha, PSYM=8
plotsym, 4, psize
OPLOT, hanorad, knorad, PSYM=8
plotsym, 8, psize
OPLOT, hano, kno, PSYM=8

device, /close
set_plot, 'X'

END
