PRO GALLERY, dat1

;####################
;####################

myhome  = GETENV('HOME')
tempfitfile = myhome+'/research/pf_clusters/pf_fits/dat/pf_temp_profs.dat'
finalps = 'test.ps'
datadir = '/mnt/DROBO'
redir   = 'reprocessed'
mkps    = 'no'
mkwww   = 'yes'
bin     = '10pix'
ktype   = 'flat'
model   = 'nonzero'
psize   = 0.4
csize   = 1.0
pthick  = 2
ncol    = 3
nrow    = 3
keVdyne = 1.62d-9
htime   = 13.46673
wwwdir  = '/mnt/DROBO/accept'
imgout  = '.png'

;####################
;####################

;# read necessary templates
restore,myhome+'/research/redux/scripts/xspectemp_rin_normerr_src.sav'
restore,myhome+'/research/redux/scripts/s_resultstemplate.sav'
restore,myhome+'/research/redux/scripts/s_tabletemplate.sav'
restore,myhome+'/research/redux/scripts/tcooltemplate.sav'
readcol,dat1,FORMAT='A,A,F,F,F,F,F,F,F,F,F,A,F,A,F,A', comment='#', $
        cluster,obsids,xs,ys,rmaxs,minctss,zs,nhs,txs,fes,$
        lbols,chips,eobss,diffs,robss,locs
fitfile = read_ascii(tempfitfile,template=xspectemp_rin_normerr_src)

;# get values specific to model
IF (ktype EQ 'flat') THEN BEGIN
    IF (model EQ 'nonzero') THEN ind = 2 ELSE ind = 3
ENDIF
IF (ktype EQ 'itpl') THEN BEGIN
    IF (model EQ 'nonzero') THEN ind = 0 ELSE ind = 1
ENDIF

;# format names for plotting
cname = strcompress(cluster,/remove_all)
cname = str_replace(cname,'ABELL_00','Abell ')
cname = str_replace(cname,'ABELL_0','Abell ')
cname = str_replace(cname,'ABELL_','Abell ')
cname = str_replace(cname,'CENTAURUS','Centaurus')
cname = str_replace(cname,'CYGNUS','Cygnus')
cname = str_replace(cname,'HERCULES','Hercules')
cname = str_replace(cname,'HYDRA','Hydra')
cname = str_replace(cname,'OPHIUCHUS','Ophiuchus')
cname = str_replace(cname,'RBS_00','RBS')
cname = str_replace(cname,'RBS_0','RBS')
cname = str_replace(cname,'SERSIC','Sersic')
cname = str_replace(cname,'ZWICKY','Zwicky')
cname = str_replace(cname,'ZWCL','Zwicky')
cname = str_replace(cname,'_',' ')

;# start looping
counter = n_elements(obsids)
FOR i = 0,n_elements(obsids)-1 DO BEGIN
    obsid = strcompress(obsids[i],/remove_all)
    name = strcompress(cluster[i],/remove_all)
    ord = where(cluster EQ name)
    multi = 'no'
    void, myobs
    push, myobs, obsid
    IF n_elements(ord) EQ 1 THEN obsid = obsid ELSE BEGIN
        temp = obsids[ord]
        FOR j=1,n_elements(temp)-1 DO BEGIN
            obsid = obsid+'_'+strcompress(temp[j],/remove_all)
            push, myobs, strcompress(temp[j],/remove_all)
        ENDFOR
        multi = 'yes'
    ENDELSE

    ;# check for file existance
    ;# table.dat
    file1 = myhome+'/research/pf_clusters/pf_fits/tables/'+obsid+'_table.dat'
    check = findfile(file1,count=count1)
    IF (count1 NE 1) THEN BEGIN
       print, "## ERROR: Missing ",file1
       GOTO,ERROR
    ENDIF

    ;# results.log
    file2 = myhome+'/research/pf_clusters/pf_fits/s_results/'+obsid+'_results.log'
    check = findfile(file2,count=count2)
    IF (count2 NE 1) THEN BEGIN
       print, "## ERROR: Missing ",file2
       GOTO,ERROR
    ENDIF

    ;# tcool profile
    file3 = myhome+'/research/pf_clusters/pf_fits/tables/'+obsid+'_tcool.dat'
    check = findfile(file3,count=count3)
    IF (count3 NE 1) THEN BEGIN
       print, "## ERROR: Missing ",file3
       GOTO,ERROR
    ENDIF

    ;# tx profile
    ord = where(fitfile.obsid EQ obsid)
    IF ord[0] EQ -1 THEN BEGIN
       print, "## ERROR: Missing Tx for",obsid
       GOTO, ERROR
    ENDIF

    ;# sbr file
    IF multi EQ 'no' THEN $
      sbr = datadir+'/'+obsid+'/'+redir+'/'+obsid+'_sbprof_'+bin+'.fits' $
    ELSE $
      sbr = datadir+'/merged/'+name+'/'+obsid+'_sbprof_'+bin+'.fits'
    check = findfile(sbr,count=count)
    IF (count NE 1) THEN BEGIN
       print, "## ERROR: Missing ",sbr
       GOTO, ERROR
    ENDIF

    ;# read in data
    dataobs = read_ascii(file1, template = s_tabletemplate)
    datafit = read_ascii(file2, template = s_resultstemplate)
    datatc  = read_ascii(file3, template = tcooltemplate)

    ;# build arrays of data
    rmean = ((dataobs.rin_mpc + dataobs.rout_mpc)/2.)*1000.
    rmlo  = rmean-(dataobs.rin_mpc*1000.)
    rmhi  = (dataobs.rout_mpc*1000.)-rmean
    nelec = dataobs.n_elec
    nelecerr = dataobs.sigma_ne
    IF (ktype EQ 'flat') THEN BEGIN
        k = dataobs.k_flat
        p = 2.*dataobs.p_flat*keVdyne
    ENDIF ELSE BEGIN
        k = dataobs.k
        p = 2.*dataobs.p*keVdyne
    ENDELSE
    kerr = dataobs.k_err
    perr = dataobs.p_err*keVdyne
    mgrav = dataobs.mgas
    merr = dataobs.merr
    rtc  = ((datatc.rin + datatc.rout)/2.)*1000.
    tc   = datatc.tc32
    tcerr= datatc.tc32err

    ;# get fit values
    krmin     = datafit.rmin[0]*1000.
    krmax     = datafit.rmax[0]*1000.
    k0        = datafit.k0[ind]
    k0err     = datafit.k0err[ind]
    k100      = datafit.k100[ind]
    k100err   = datafit.k100err[ind]
    alpha     = datafit.plaw[ind]
    alphaerr  = datafit.plawerr[ind]
    pk0       = datafit.k0[ind+1]
    pk0err    = datafit.k0err[ind+1]
    pk100     = datafit.k100[ind+1]
    pk100err  = datafit.k100err[ind+1]
    palpha    = datafit.plaw[ind+1]
    palphaerr = datafit.plawerr[ind+1]

    ;# get Tx values
    z = zs[i]
    ftx = txs[i]
    cosmology, z, result, /silent
    r500  = rdelta(500, z, ftx, /silent)*1000.
    age   = htime-result[6]
    rin   = fitfile.rin[where(fitfile.obsid EQ obsid)]
    rout  = fitfile.rout[where(fitfile.obsid EQ obsid)]
    rtx   = ((rin+rout)/2.)*60.*result[4]
    rtxlo = rtx-(rin*60.)*result[4]
    rtxhi = ((rout*60.)*result[4])-rtx
    tx    = fitfile.tx[where(fitfile.obsid EQ obsid)]
    txhi  = fitfile.thi[where(fitfile.obsid EQ obsid)]
    txlo  = fitfile.tlo[where(fitfile.obsid EQ obsid)]
    tlo   = tx-txlo
    thi   = txhi-tx
    fe    = fitfile.fe[where(fitfile.obsid EQ obsid)]
    felo  = fitfile.felo[where(fitfile.obsid EQ obsid)]
    fehi  = fitfile.fehi[where(fitfile.obsid EQ obsid)]
    felo  = fe-felo
    fehi  = fehi-fe

    ;# surf bri
    print, '## STATUS: Working on ',name,'. ',num2str(counter),' remaining...'
    fits   = mrdfits(sbr,1,/silent)
    rsbr   = fits.rmid*0.492*result[4]
    rslo   = rsbr-(fits.rin*0.492*result[4])
    rshi   = (fits.rout*0.492*result[4])-rsbr
    exp    = fits.exposure
    surbri = fits.sur_bri/exp/(0.492^2.)
    sbrerr = fits.sur_bri_err/exp/(0.492^2.)

    ;# adhoc filter
    ord    = where(rsbr LT 1000)
    rsbr   = rsbr[ord]
    rslo   = rslo[ord]
    rshi   = rshi[ord]
    surbri = surbri[ord]
    sbrerr = sbrerr[ord]

    ;# get rid of the 'NaN' entries and negatives
    ord      = where((nelec EQ nelec) AND (nelec GT 0.))
    rne      = rmean[ord]
    rnelo    = rmlo[ord]
    rnehi    = rmhi[ord]
    nelec    = nelec[ord]
    nelecerr = nelecerr[ord]

    ;# get rid of the 'NaN' entries and negatives
    ord  = where((k EQ k) AND (k GT 0.)); AND (rmean GE krmin) AND (rmean LE krmax))
    rk   = rmean[ord]
    rklo = rmlo[ord]
    rkhi = rmhi[ord]
    k    = k[ord]
    kerr = kerr[ord]

    ;# calc implied suppression factor
    rfc   = rk/r500
    rfclo = rklo/r500
    rfchi = rkhi/r500
    fc    = 62.5*rk^2./k^3.
    fcerr = fc*3.*(kerr/k)

    ;# get rid of the 'NaN' entries and negatives
    ord  = where((p EQ p) AND (p GT 0.))
    rp   = rmean[ord]
    rplo = rmlo[ord]
    rphi = rmhi[ord]
    p    = p[ord]
    perr = perr[ord]

    ;# get rid of the 'NaN' entries and negatives
    ord   = where((mgrav EQ mgrav) AND (mgrav GT 0.))
    rmg   = rmean[ord]
    rmglo = rmlo[ord]
    rmghi = rmhi[ord]
    mgrav  = mgrav[ord]
    merr  = merr[ord]

    ;# get rid of the 'NaN' entries and negatives
    ord   = where((tc EQ tc) AND (tc GT 0.))
    rtc   = rtc[ord]
    rtclo = reverse(rmlo[ord])
    rtchi = reverse(rmhi[ord])
    tc    = tc[ord]
    tcerr = tcerr[ord]

    ;# build the pointer arrays
    colors = maken(50,250,9)
    rs = [ptr_new(rtx), $
          ptr_new(rtx), $
          ptr_new(rsbr), $
          ptr_new(rne), $
          ptr_new(rp), $
          ptr_new(rk), $
          ptr_new(rtc), $
          ptr_new(rmg), $
          ptr_new(rfc)]
    rlo = [ptr_new(rtxlo), $
           ptr_new(rtxlo), $
           ptr_new(rslo), $
           ptr_new(rnelo), $
           ptr_new(rplo), $
           ptr_new(rklo), $
           ptr_new(rtclo), $
           ptr_new(rmglo), $
           ptr_new(rfclo)]
    rhi = [ptr_new(rtxhi), $
           ptr_new(rtxhi), $
           ptr_new(rshi), $
           ptr_new(rnehi), $
           ptr_new(rphi), $
           ptr_new(rkhi), $
           ptr_new(rtchi), $
           ptr_new(rmghi), $
           ptr_new(rfchi)]
    ys = [ptr_new(tx), $
          ptr_new(fe), $
          ptr_new(surbri), $
          ptr_new(nelec), $
          ptr_new(p), $
          ptr_new(k), $
          ptr_new(tc), $
          ptr_new(mgrav), $
          ptr_new(fc)]
    void, terr
    void, feerr
    FOR h=0,n_elements(tlo)-1 DO IF tlo[h] GT thi[h] THEN push, terr, tlo[h] ELSE push, terr, thi[h]
    FOR h=0,n_elements(felo)-1 DO IF felo[h] GT fehi[h] THEN push, feerr, felo[h] ELSE push, feerr, fehi[h]
    yerrs = [ptr_new(terr), $
             ptr_new(feerr), $
             ptr_new(sbrerr), $
             ptr_new(nelecerr), $
             ptr_new(perr), $
             ptr_new(kerr), $
             ptr_new(tcerr), $
             ptr_new(merr), $
             ptr_new(fcerr)]
    ytexs = [textoidl('Temperature [keV]'),$
             textoidl('Metallicity [Z/Z'+sunsymbol()+']'), $
             textoidl('Surface Brightness [cts s^{-1} arcsec^{-2}]'), $
             textoidl('Electron Density [cm^{-3}]'),$
             textoidl('Pressure [dyne cm^{-2}]'),$
             textoidl('Entropy [keV cm^2]'),$
             textoidl('Cooling Time [Gyr]'), $
             textoidl('Gravitating Mass [M'+sunsymbol()+']'), $
             textoidl('Implied conduction suppression factor, f_c')]

    ;# Device settings
    set_plot, 'PS'
    loadct, 39, /silent
    !FANCY    = 4
    !LINETYPE = 0
    !P.FONT   = 0
    !X.THICK  = 1
    !Y.THICK  = 1
    !Z.THICK  = 1
    !X.TICKLEN = 0.04
    !X.TICKLEN = 0.04

    ;# make a gallery of plots
    IF (mkps EQ 'yes') THEN BEGIN
        !P.MULTI  = [0,ncol,nrow,0]
        push, list, name+'_gallery.eps'
        device, filename=name+'_gallery.eps', $
                /encapsulated, $
                /color, $
                /portrait, $
                set_font='Times-Roman', $
                bits=16
        FOR j=0,n_elements(ys)-1 DO BEGIN
            pcolor = colors[j]
            x = *rs[j]
            y = *ys[j]
            xerrlo = *rlo[j]
            xerrhi = *rhi[j]
            yerr   = *yerrs[j]
            yerrlo = *yerrs[j]
            yerrhi = *yerrs[j]
            xtex = textoidl('R [kpc]')
            ytex = ytexs[j]
            xmin = 0.8*min(x)
            xmax = 1.2*max(x)
            ny   = y[where(y GT 0.)]
            ymin = 0.8*min(ny)
            ymax = 1.2*max(ny)
            yticks = LOGLEVELS([ymin,ymax],/fine)
            ynticks = N_Elements(yticks)
            xticks = LOGLEVELS([xmin,xmax],/fine)
            xnticks = N_Elements(xticks)
            IF ((j EQ 0) OR (j EQ 1)) THEN $ 
               plot, x, y, $
                     /nodata, $
                     XRANGE=[xmin,xmax], $
                     YRANGE=[ymin,ymax], $
                     /XLOG, /XSTY, /YSTY, $
;                     title = cname[i], $
                     xtitle = xtex, $
                     ytitle = ytex, $
                     xticks = xnticks-1, $
                     xtickv = xticks, $
                     CHARSIZE = csize $
            ELSE $
               plot, x, y, $
                     /nodata, $
                     XRANGE = [xmin,xmax], $
                     YRANGE = [ymin,ymax], $
                     /XSTY, /YSTY, $
                     /XLOG, /YLOG, $
;                     title  = cname[i], $
                     xtitle = xtex, $
                     ytitle = ytex, $
                     yticks = ynticks-1, $
                     ytickv = yticks, $
                     xticks = xnticks-1, $
                     xtickv = xticks, $
                     CHARSIZE = csize
            IF j EQ 5 THEN BEGIN
                kfit = k0+k100*(x/100)^alpha
                oplot, x, kfit, psym=0, linestyle=2, thick=4, color=0
                pkfit = pk0+pk100*(x/100)^palpha
                oplot, x, pkfit, psym=0, linestyle=3, thick=4, color=0
                items = [textoidl('K_0 = '+num2str(k0)+' \pm '+num2str(k0err)), $
                         textoidl('K_{100} = '+num2str(k100)+' \pm '+num2str(k100err)), $
                         textoidl('\alpha = '+num2str(alpha)+' \pm '+num2str(alphaerr))]
                linearr = replicate(-99,n_elements(items))
                psyarr = replicate(-99,n_elements(items))
                legend, items, linestyle=larr, psym=parr, box=0, charsize=0.5, /top, /left, /fill
            ENDIF
            IF j EQ 6 THEN BEGIN
                dx = maken(0.01,100000,10)
                oplot, dx, replicate(age,n_elements(dx)), psym=0, linestyle=2, thick=4, color=0
            ENDIF
            plotsym, 0, psize, /fill
            oploterror, x, y, xerrlo, yerrlo, psym=8, /lobar
            oploterror, x, y, xerrhi, yerrhi, psym=8, /hibar
            oplot, x, y, PSYM=8, color=0
            plotsym, 0, psize-0.2*psize, /fill
            oplot, x, y, PSYM=8, color=pcolor
        ENDFOR
        device, /close
    ENDIF

    ;# make individual plots for web pages
    IF (mkwww EQ 'yes') THEN BEGIN
        mypsize = 1.0
        !P.MULTI  = [0,0,0,0]
        wwws = ['tx','fe','surbri','density','pressure','entropy','tcool','mgrav','fc']
        FOR j=0,n_elements(ys)-1 DO BEGIN
           outfile = obsid+'_'+wwws[j]+'.ps'
           device, filename = outfile, $
                   /color, $
                   /portrait, $
                   set_font='Times-Roman', $
                   bits=16
           pcolor = colors[j]
           x = *rs[j]
           y = *ys[j]
           xerrlo = *rlo[j]
           xerrhi = *rhi[j]
           yerr   = *yerrs[j]
           yerrlo = *yerrs[j]
           yerrhi = *yerrs[j]
           xtex = textoidl('R [kpc]')
           ytex = ytexs[j]
           xmin = 0.8*min(x)
           xmax = 1.2*max(x)
           ord  = where(y GT 0.,count)
           IF count LE 0 THEN BEGIN
              ymin = 0.01
              ymax = 1.2*max(yerr)
           ENDIF ELSE BEGIN
              ny = y[ord]
              ymin = 0.8*min(ny)
              ymax = 1.2*max(ny)
           ENDELSE
           yticks = LOGLEVELS([ymin,ymax],/fine)
           ynticks = N_Elements(yticks)
           xticks = LOGLEVELS([xmin,xmax],/fine)
           xnticks = N_Elements(xticks)
           IF wwws[j] EQ 'tx' THEN $
              plot, x, y, $
                    /nodata, $
                    XRANGE=[xmin,xmax], $
                    YRANGE=[ymin,ymax], $
                    /XLOG, /XSTY, /YSTY, $
                    title = cname[i], $
                    xtitle = xtex, $
                    ytitle = ytex, $
                    xticks = xnticks-1, $
                    xtickv = xticks, $
                    CHARSIZE = csize $
           ELSE $
              plot, x, y, $
                    /nodata, $
                    XRANGE=[xmin,xmax], $
                    YRANGE=[ymin,ymax], $
                    /XSTY, /YSTY, $
                    /XLOG, /YLOG, $
                    title = cname[i], $
                    xtitle = xtex, $
                    ytitle = ytex, $
                    yticks = ynticks-1, $
                    ytickv = yticks, $
                    xticks = xnticks-1, $
                    xtickv = xticks, $
                    CHARSIZE = csize
           IF wwws[j] EQ 'entropy' THEN BEGIN
              kfit = k0+k100*(x/100)^alpha
              oplot, x, kfit, psym=0, linestyle=2, thick=4, color=0
              pkfit = pk0+pk100*(x/100)^palpha
              oplot, x, pkfit, psym=0, linestyle=3, thick=4, color=0
           ENDIF
           IF wwws[j] EQ 'tcool' THEN BEGIN
              dx = maken(0.01,100000,10)
              oplot, dx, replicate(age,n_elements(dx)), psym=0, linestyle=2, thick=4, color=0
           ENDIF
           plotsym, 0, mypsize, /fill
           oploterror, x, y, xerrlo, yerrlo, psym=8, /lobar
           oploterror, x, y, xerrhi, yerrhi, psym=8, /hibar
           oplot, x, y, PSYM=8, color=0
           plotsym, 0, mypsize-0.2*mypsize, /fill
           oplot, x, y, PSYM=8, color=pcolor
           device, /close
           print, '## Converting '+outfile+' to '+imgout
           SPAWN, 'convert -scale 120% '+outfile+' temp'+imgout
           FOR zz=0,n_elements(myobs)-1 DO BEGIN
              tobs = myobs[zz]
              tdir = wwwdir+'/'+wwws[j]+'/'
              SPAWN, 'rm -f '+tdir+tobs+'_'+wwws[j]+imgout
              SPAWN, 'cp temp'+imgout+' '+tdir+tobs+'_'+wwws[j]+imgout
           ENDFOR
           SPAWN, 'rm -f temp'+imgout+' '+outfile
        ENDFOR
     ENDIF
ERROR:
counter--
ENDFOR
set_plot, 'X'
IF (mkps EQ 'yes') THEN BEGIN
   SPAWN, 'ls -1 *gallery.eps > list'
   SPAWN, 'cat list | pscat 1 '+finalps
   SPAWN, 'rm -f list'
   SPAWN, 'rm -f *gallery.eps'
ENDIF
END
