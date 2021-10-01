PRO ftx_color

myhome = GETENV('HOME')
mkleg    = 'no'
dofilter = 'no'
ploterr  = 'no'
xtype    = 'tx'
pltype   = 'full'
pretty   = 'yes'
sig      = 2.0
tfcut    = 0.
upper    = 5.
csize    = 0.9
space    = 0.6
psize    = 0.9

!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 3
!Y.THICK  = 3
!Z.THICK  = 3 

;# input files array
push, pstitle, 'R!D2500-CORE'
push, pstitle, 'R!D5000-CORE'
push, files, myhome+'/research/me_temp_proj/me_fits/dat/culled_r2500-50_7-7.dat'
push, files, myhome+'/research/me_temp_proj/me_fits/dat/culled_r2500-50_2-7.dat'
push, files, myhome+'/research/me_temp_proj/me_fits/dat/culled_r5000-50_7-7.dat'
push, files, myhome+'/research/me_temp_proj/me_fits/dat/culled_r5000-50_2-7.dat'
coolfile = myhome+'/research/me_temp_proj/me_fits/dat/inner50_cash.dat'

IF xtype EQ "tx" THEN BEGIN
    output = 'ftx_tx_color.eps'
ENDIF ELSE IF xtype EQ "err" THEN BEGIN
    output = 'ftx_err_color.eps'
ENDIF ELSE IF xtype EQ "errz" THEN BEGIN
    output = 'ftx_errz_color.eps'
ENDIF ELSE IF xtype EQ "z" THEN BEGIN
    output = 'ftx_z_color.eps'
ENDIF ELSE IF xtype EQ "nh" THEN BEGIN
    output = 'ftx_nh_color.eps'
ENDIF ELSE IF xtype EQ "src" THEN BEGIN
    output = 'ftx_src_color.eps'
ENDIF ELSE IF xtype EQ "lx" THEN BEGIN
    output = 'ftx_lx_color.eps'
    restore, "/home/cavagnolo/research/redux/scripts/full_sample.sav"
    reflist = 'full_sample_L-50.list'
    ref = read_ascii(reflist, template = full_sample)
    lnames = strcompress(lfits.cluster,/remove_all)
    lbol = lfits.lx
    lo = lfits.lxlo
    hi = lfits.lxhi
    lbollo = lx-lo
    lbolhi = hi-lx
ENDIF

IF pltype EQ "full" THEN BEGIN
    namecut = 10000
    plname = "no"
ENDIF ELSE IF pltype EQ "hi" THEN BEGIN
    namecut = 1.5
    plname = "yes"
ENDIF ELSE IF pltype EQ "lo" THEN BEGIN
    namecut = 0.85
    plname = "yes"
ENDIF
IF (xtype EQ "err" OR xtype EQ "errz") THEN BEGIN
    namecut = 10000
    plname = "no"
ENDIF

i = 0
m = 0
FOR j = 0,n_elements(pstitle)-1 DO BEGIN
   void, plcolor
   void, plsym
   void, items
   void, dy
   void, names
   void, x
   void, xlo
   void, xhi

    restore,myhome+'/research/redux/scripts/xspectemp_rin_normerr_src.sav'
    full = read_ascii(files[i], template = xspectemp_rin_normerr_src)
    hard = read_ascii(files[i+1], template = xspectemp_rin_normerr_src)
    cool = read_ascii(coolfile, template = xspectemp_rin_normerr_src)

    check = full.obsid/hard.obsid
    uhoh = where(check NE 1)
    IF uhoh NE -1 THEN BEGIN
        print,'## ERROR: OUT OF ORDER FILE: '+dat1+' '+dat2
        exit
    ENDIF

    fulltx = full.tx
    fulllo = fulltx - full.tlo
    fullhi = full.thi - fulltx
    hardtx = hard.tx
    hardlo = hard.tx - hard.tlo
    hardhi = hard.thi - hard.tx
    name = full.cluster
    obsids = full.obsid
    z = full.z
    nh = full.nh
    src = full.src

    IF xtype EQ "tx" THEN BEGIN
        x = fulltx
        xhi = fullhi
        xlo = fulllo
    ENDIF
    IF xtype EQ "err" THEN x = fulltx
    IF xtype EQ "errz" THEN x = z
    IF xtype EQ "z" THEN x = z
    IF xtype EQ "nh" THEN x = nh
    IF xtype EQ "src" THEN x = src/100.
    IF xtype EQ "lx" THEN BEGIN
        FOR jj = 0,n_elements(fulltx)-1 DO BEGIN
            tname = strcompress(name[jj],/remove_all)
            get = where(strmatch(lnames,tname,/fold_case)EQ 1)
            get = get[0]
            push, x, lbol[get]
            push, xhi, lbolhi[get]
            push, xlo, lbollo[get]
        ENDFOR
    ENDIF
    y = hard.tx/fulltx
    yhi = hard.tx/fulltx*(sqrt((hardhi/hard.tx)^2.+(fullhi/fulltx)^2.))
    ylo = hard.tx/fulltx*(sqrt((hardlo/hard.tx)^2.+(fulllo/fulltx)^2.))

    ;# an ad-hoc filter
    IF dofilter EQ "yes" THEN BEGIN
        ord = where(src GT 90.)
        x = x[ord]
        y = y[ord]
        ylo = ylo[ord]
        yhi = yhi[ord]
        name = name[ord]
        obsids = obsids[ord]
        fulltx = fulltx[ord]
        fulllo = fulllo[ord]
        fullhi = fullhi[ord]
    ENDIF

    ;# build color arrays
    red = REFORM([255,0,0],1,3)
    TVLCT, red, 100
    blue = REFORM([0,0,255],1,3)
    TVLCT, blue, 200

    ;# define CC/NCC
    nblack = 0
    ngreen = 0
    nblue = 0
    nred = 0
    cc = 0
    ncc = 0
    numcc = 0
    numncc = 0
    coolobs = cool.obsid
    coolname = cool.cluster
    FOR ss=0,n_elements(x)-1 DO BEGIN
        obs = obsids[ss]
        tname = name[ss]
        mine = where(coolname EQ tname)
        mine = mine[0]
        cooltx = cool.tx[mine]
        cooltlo = cool.tlo[mine]
        coolthi = cool.thi[mine]
        coollo = cooltx - cooltlo
        coolhi = coolthi - cooltx
        dec = cooltx/fulltx[ss]
        dechi = cooltx/fulltx[ss]*(sqrt((coolhi/cooltx)^2.+(fullhi[ss]/fulltx[ss])^2.))
        declo = cooltx/fulltx[ss]*(sqrt((coollo/cooltx)^2.+(fulllo[ss]/fulltx[ss])^2.))
        IF ((y[ss]-ylo[ss] GE 1.1) AND (dec+sig*dechi LT 1.)) THEN BEGIN
           push, plcolor, 0     ;# black stars
           push, plsym, 3
           cc++
           nblack++
           IF y[ss]-ylo[ss] GT tfcut THEN numcc++
        ENDIF ELSE IF (y[ss]-ylo[ss] GE 1.1) THEN BEGIN
           push, plcolor, 150   ;# green squares
;           push, plcolor, 75   ;# green squares
           push, plsym, 4
           ncc++
           ngreen++
           IF y[ss]-ylo[ss] GT tfcut THEN numncc++
        ENDIF ELSE IF (dec+sig*dechi) LT 1. THEN BEGIN
           push, plcolor, 50    ;#blue color points
;           push, plcolor, 25    ;#blue color points
           push, plsym, 5
           cc++
           nblue++
           IF y[ss]-ylo[ss] GT tfcut THEN numcc++
        ENDIF ELSE BEGIN
           push, plcolor, 250   ;#red color points
;           push, plcolor, 100   ;#red color points
           push, plsym, 8
           ncc++
           nred++
           IF y[ss]-ylo[ss] GT tfcut THEN numncc++
        ENDELSE
     ENDFOR
    
    ;# load color tables and populate
    void,uppery
    void,upperx

    IF xtype EQ "tx" THEN BEGIN
        FOR jj=0,n_elements(y)-1 DO BEGIN
            IF y[jj]+yhi[jj] GT upper THEN BEGIN
                yhi[jj]=0.
                push,uppery,y[jj]
                push,upperx,x[jj]
            ENDIF
        ENDFOR
    ENDIF
    IF (xtype EQ "err" OR xtype EQ "errz") THEN BEGIN
        FOR f=0,n_elements(yhi)-1 DO BEGIN
            IF yhi[f] GT ylo[f] THEN push,dy,yhi[f] ELSE push,dy,ylo[f]
        ENDFOR
        y = (dy/y)*100.
    ENDIF

    IF (m NE 0) THEN void,names
    FOR ii=0,n_elements(fulltx)-1 DO BEGIN
        IF pltype EQ "hi" THEN BEGIN
            IF (y[ii] GT namecut) THEN $
              push, names, name[ii] ELSE $
              push, names, ''
        ENDIF ELSE IF pltype EQ "lo" THEN BEGIN
            IF (y[ii] LT namecut) THEN $
              push, names, name[ii] ELSE $
              push, names, ''
        ENDIF ELSE push,names,''
    ENDFOR
    plotsym, 0, 0.001, /fill
    ymax = 1.05*max(y)
    ymin = 0.95*min(y)
    xmax = 20.0
    xmin = 2.0
    IF n_elements(pstitle) GT 1 THEN BEGIN
        IF (m EQ 0) THEN BEGIN
            set_plot,'PS'
            device, filename = output, $
                    /color, $
                    /encapsulated, $
                    /portrait, $
                    set_font='Times-Roman'
            loadct, 39, /silent
            multiplot,[1,2]
        ENDIF
        IF xtype EQ "tx"   THEN xtex = textoidl('T_{0.7-7.0} [keV]')
        IF xtype EQ "err"  THEN xtex = textoidl('T_{0.7-7.0} [keV]')
        IF xtype EQ "errz" THEN xtex = textoidl('Redshift')
        IF xtype EQ "lx"   THEN xtex = textoidl('L_{bol.} [10^{45} ergs sec^{-1}]')
        IF xtype EQ "z"    THEN xtex = textoidl('Redshift')
        IF xtype EQ "nh"   THEN xtex = textoidl('N_{H} [10^{20} cm^{-2}]')
        IF xtype EQ "src"  THEN xtex = textoidl('Source/(Source+Background)')
        ytex = textoidl('T_{HBR} = T_{2.0-7.0}/T_{0.7-7.0}')
        IF (xtype EQ "err" OR xtype EQ "errz") THEN ytex = textoidl('Percentage Error')
        IF (m NE 0) THEN multiplot
        IF ((xtype EQ "nh") OR $
            (xtype EQ "lx") OR $
            (xtype EQ "z")  OR $
            (xtype EQ "tx")) THEN BEGIN
            plot, x, y, $
                  /xlog, $
                  /xsty, /ysty, $
                  psym = 8, $
                  xrange = [xmin,xmax], $
                  yrange = [ymin,ymax], $
                  symsize = 0.5*psize, $
                  charsize = csize
        ENDIF ELSE BEGIN
            plot, x, y, $
                  /xsty, /ysty, $
                  psym = 8, $
                  xrange = [xmin,xmax], $
                  yrange = [ymin,ymax], $
                  symsize = 0.5*psize, $
                  charsize = csize
        ENDELSE
    ENDIF ELSE BEGIN
        set_plot,'PS'
        device, filename = output, $
                /color, $
                /encapsulated, $
                /portrait, $
                set_font='Times-Roman'
        loadct, 39, /silent
        IF xtype EQ "tx"   THEN xtex = textoidl('T_{0.7-7.0} [keV]')
        IF xtype EQ "err"  THEN xtex = textoidl('T_{0.7-7.0} [keV]')
        IF xtype EQ "errz" THEN xtex = textoidl('Redshift')
        IF xtype EQ "lx"   THEN xtex = textoidl('L_{bol.} [10^{45} ergs sec^{-1}]')
        IF xtype EQ "z"    THEN xtex = textoidl('Redshift')
        IF xtype EQ "nh"   THEN xtex = textoidl('N_{H} [10^{20} cm^{-2}]')
        IF xtype EQ "src"  THEN xtex = textoidl('Source/(Source+Background)')
        ytex = textoidl('T_{HBR} = T_{2.0-7.0}/T_{0.7-7.0}')
        IF (xtype EQ "err" OR xtype EQ "errz") THEN ytex = textoidl('Percentage Error')
        IF ((xtype EQ "nh") OR $
            (xtype EQ "lx") OR $
            (xtype EQ "tx") OR $
            (xtype EQ "z")) THEN BEGIN
            plot, x, y, $
                  /xlog, $
                  /xsty, /ysty, $
                  psym = 8, $
                  xrange = [xmin,xmax], $
                  yrange = [ymin,ymax], $
                  symsize = psize, $
                  xtitle = xtex, $
                  ytitle = ytex, $
                  charsize = csize
        ENDIF ELSE BEGIN
            plot, x, y, $
                  /xsty, /ysty, $
                  psym = 8, $
                  xrange = [xmin,xmax], $
                  yrange = [ymin,ymax], $
                  xtitle = xtex, $
                  ytitle = ytex, $
                  symsize = psize, $
                  charsize = csize
        ENDELSE
    ENDELSE

    IF (xtype NE "err" AND xtype NE "errz") THEN BEGIN
        ;# overplot the line y=1
        oplot,findgen(xmax*1000)+xmin,replicate(1,1000), linestyle=2
        ;# overplot the errors
        IF ploterr EQ 'yes' THEN BEGIN
            oploterror, x, y, ylo, psym=8, /lobar, /nohat
            oploterror, x, y, yhi, psym=8, /hibar, /nohat
        ENDIF
    ENDIF

    ;# overplot color points
    FOR ff=0,n_elements(x)-1 DO BEGIN
        xt = [x[ff],0]
        yt = [y[ff],0]
        pt = plcolor[ff]
        ps = plsym[ff]
        plotsym, ps, 0.75*psize, /fill        
        oplot, xt, yt, color=pt, psym=8
    ENDFOR
    !psym = 0

    ;# draw a legend
    IF n_elements(pstitle) GT 1 THEN $
      items = [pstitle[j], $
               textoidl('Black \equiv T_{HBR} > 1.1 + Cool Core'), $
               textoidl('Green \equiv T_{HBR} > 1.1 + Non-cool core'), $
               textoidl('Blue \equiv Cool Core'), $
               textoidl('Red \equiv Non-Cool Core')] $
    ELSE items = [textoidl('Black \equiv T_{HBR} > 1.1 + Cool Core'), $
                  textoidl('Green \equiv T_{HBR} > 1.1 + Non-cool core'), $
                  textoidl('Blue \equiv Cool Core'), $
                  textoidl('Red \equiv Non-Cool Core')]
    IF pretty EQ "no" THEN BEGIN
        push, items, pstitle[j]
        push, items, 'Total :'+num2str(n_elements(x))
        push, items, 'CC: '+num2str(cc)
        push, items, 'NCC: '+num2str(ncc)
        push, items, textoidl('CC T_{HBR} > '+num2str(tfcut,2)+': '+num2str(numcc))
        push, items, textoidl('NCC T_{HBR} > '+num2str(tfcut,2)+': '+num2str(numncc))
    ENDIF ELSE IF n_elements(pstitle) GT 1 THEN items = [pstitle[j]]    
    linearr = replicate(-99,n_elements(items))
    psyarr = replicate(-99,n_elements(items))
    IF (mkleg EQ 'yes' OR n_elements(pstitle) GT 1) THEN $
      legend, items, linestyle=linearr, psym=psyarr, charsize=csize, $
              /top, box=0, /right_legend

    ;# write out names on the plot
    IF plname EQ "yes" THEN BEGIN
        xyouts, x, y, names, charsize=0.5
        names = names[where(names NE '')]
        items = [names]
        linearr = replicate(-99,n_elements(items))
        psyarr = replicate(-99,n_elements(items))
        legend, items, linestyle=linearr, psym=psyarr, charsize=0.3, $
                spacing=space, /top, box=0, /left_legend
    ENDIF
    i = i+2
    m++
print, 'Black: ',nblack
print, 'Green: ',ngreen
print, 'Blue: ', nblue
print, 'Red: ', nred
print, 'Total: ',nblack+ngreen+nblue+nred

ENDFOR

IF n_elements(pstitle) GT 1 THEN BEGIN
    ;# horz label
    xyouts, 0.52, 0.12, xtex, /normal, charsize=csize
    ;# vert label
    xyouts, 0.13, 0.46, ytex, /normal, orientation=90, charsize=csize
ENDIF
device, /close

END
