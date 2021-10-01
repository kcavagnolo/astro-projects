PRO GALLERY

;####################
;####################

myhome  = GETENV('HOME')
tabdir    = 'data/'
txfitfile = myhome+'/research/iras09/data/tx_2.5K/proj_1T_nhfro.dat'
dtxfitfile = myhome+'/research/iras09/data/tx_2.5K/deproj_1T_nhfro.dat'
fefitfile = myhome+'/research/iras09/data/tx_5K/proj_1T_nhfro.dat'
vikfile = myhome+'/research/iras09/analysis/vik06fit'
outfile = 'iras09_nhfro.eps'
datadir = '/mnt/DROBO'
redir   = 'reprocessed'
bin     = '2pix'
ktype   = 'flat'
model   = 'nonzero'
psize   = 0.5
csize   = 2.0
ncol    = 2
nrow    = 4
xsize   = 8.5
ysize   = 11.0
keVdyne = 1.60217733d-9
htime   = 13.46673
z       = 0.4418
cavheat = 3.41d44
lcool   = 1.61d45

;####################
;####################

;# read necessary templates
dat1 = myhome+'/research/iras09/info/iras09.list'
restore,myhome+'/research/redux/scripts/xspectemp_rin_normerr_src.sav'
restore,myhome+'/research/redux/scripts/s_resultstemplate.sav'
restore,myhome+'/research/redux/scripts/s_tabletemplate.sav'
restore,myhome+'/research/redux/scripts/tcooltemplate.sav'
readcol,dat1,FORMAT='A,A,F,F,F,F,F,F,F,F,F,A,F,A,F,A', comment='#', $
        cluster,obsids,xs,ys,rmaxs,minctss,zs,nhs,txs,fes,$
        lbols,chips,eobss,diffs,robss,locs
txfile = read_ascii(txfitfile,template=xspectemp_rin_normerr_src)
dtxfile = read_ascii(dtxfitfile,template=xspectemp_rin_normerr_src)
fefile = read_ascii(fefitfile,template=xspectemp_rin_normerr_src)

;# get values specific to model
IF (ktype EQ 'flat') THEN BEGIN
    IF (model EQ 'nonzero') THEN ind = 2 ELSE ind = 3
ENDIF
IF (ktype EQ 'itpl') THEN BEGIN
    IF (model EQ 'nonzero') THEN ind = 0 ELSE ind = 1
ENDIF

;# format names for plotting
cname = strcompress(cluster,/remove_all)
cname = str_replace(cname,'IRAS_00','IRAS')
cname = str_replace(cname,'IRAS_0','IRAS')
cname = str_replace(cname,'_',' ')

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
!P.MULTI  = [0,ncol,nrow,0]
device, filename=outfile, $
        /inches, $
        xsize=xsize, $
        ysize=ysize, $
        /color, $
        /portrait, $
        set_font='Times-Roman', $
        bits=16

;# start looping
counter = n_elements(obsids)
prevname = 'jdfdjf'
FOR i = 0,n_elements(obsids)-1 DO BEGIN
   obsid = strcompress(obsids[i],/remove_all)
   name = strcompress(cluster[i],/remove_all)
   fidnh = nhs[i]
   IF name EQ prevname THEN GOTO,SKIPOBS
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
    file1 = myhome+'/research/iras09/'+tabdir+'/'+obsid+'_table.dat'
    check = findfile(file1,count=count1)
    IF (count1 NE 1) THEN BEGIN
       print, "## ERROR: Missing ",file1
       GOTO,ERROR
    ENDIF

    ;# results.log
    file2 = myhome+'/research/iras09/'+tabdir+'/'+obsid+'_results.log'
    check = findfile(file2,count=count2)
    IF (count2 NE 1) THEN BEGIN
       print, "## ERROR: Missing ",file2
       GOTO,ERROR
    ENDIF

    ;# tcool profile
    file3 = myhome+'/research/iras09/'+tabdir+'/'+obsid+'_tcool.dat'
    check = findfile(file3,count=count3)
    IF (count3 NE 1) THEN BEGIN
       print, "## ERROR: Missing ",file3
       GOTO,ERROR
    ENDIF

    ;# tx profile
    ord = where(txfile.obsid EQ obsid)
    IF ord[0] EQ -1 THEN BEGIN
       print, "## ERROR: Missing Tx for ",obsid
       GOTO, ERROR
    ENDIF
    ord = where(dtxfile.obsid EQ obsid)
    IF ord[0] EQ -1 THEN BEGIN
       print, "## ERROR: Missing Deproj Tx for ",obsid
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
    print, 'Using ', sbr

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
    mgas = dataobs.mgas
    merr = dataobs.merr
    rtcin = datatc.rin
    rtcout = datatc.rout
    rtc  = ((rtcin + rtcout)/2.)*1000.
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
    cosmology, z, result, /silent
    age = htime-result[6]

    ;# get r for tc=age
    tempx = maken(min(rtc),max(rtc),3.2d4)
    temptc = interpol(tc,rtc,tempx)
    atc = 0.
    ind = 0
    WHILE (atc LE age) DO BEGIN
       rcool = tempx[ind]
       atc = temptc[ind]
       ind++
    ENDWHILE
    print, 'Age of Universe at z=',num2str(z),': ',age, ' Gyr'
    print, 'Cooling radius: ',rcool,' kpc'

    ;# finish with spec values
    ;# projected
    rin   = txfile.rin[where(txfile.obsid EQ obsid)]
    rout  = txfile.rout[where(txfile.obsid EQ obsid)]
    rtx   = ((rin+rout)/2.)*60.*result[4]
    rtxlo = rtx-(rin*60.)*result[4]
    rtxhi = ((rout*60.)*result[4])-rtx
    nh    = txfile.nh[where(txfile.obsid EQ obsid)]
    nhlo  = txfile.nlo[where(txfile.obsid EQ obsid)]
    nhhi  = txfile.nhi[where(txfile.obsid EQ obsid)]
    nhlo  = nh-nhlo
    nhhi  = nhhi-nh
    tx    = txfile.tx[where(txfile.obsid EQ obsid)]
    txhi  = txfile.thi[where(txfile.obsid EQ obsid)]
    txlo  = txfile.tlo[where(txfile.obsid EQ obsid)]
    tlo   = tx-txlo
    thi   = txhi-tx
    ;# deprojected
    rin   = dtxfile.rin[where(dtxfile.obsid EQ obsid)]
    rout  = dtxfile.rout[where(dtxfile.obsid EQ obsid)]
    rdtx   = ((rin+rout)/2.)*60.*result[4]
    rdtxlo = rdtx-(rin*60.)*result[4]
    rdtxhi = ((rout*60.)*result[4])-rdtx
    dtx    = dtxfile.tx[where(dtxfile.obsid EQ obsid)]
    dtxhi  = dtxfile.thi[where(dtxfile.obsid EQ obsid)]
    dtxlo  = dtxfile.tlo[where(dtxfile.obsid EQ obsid)]
    dtlo   = dtx-dtxlo
    dthi   = dtxhi-dtx
    ;# fe file
    rin   = fefile.rin[where(fefile.obsid EQ obsid)]
    rout  = fefile.rout[where(fefile.obsid EQ obsid)]
    rfe   = ((rin+rout)/2.)*60.*result[4]
    rfelo = rfe-(rin*60.)*result[4]
    rfehi = ((rout*60.)*result[4])-rfe
    fe    = fefile.fe[where(fefile.obsid EQ obsid)]
    felo  = fefile.felo[where(fefile.obsid EQ obsid)]
    fehi  = fefile.fehi[where(fefile.obsid EQ obsid)]
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

    ;# get rid of the 'NaN' entries and negatives
    ord  = where((p EQ p) AND (p GT 0.))
    rp   = rmean[ord]
    rplo = rmlo[ord]
    rphi = rmhi[ord]
    p    = p[ord]
    perr = perr[ord]

    ;# get rid of the 'NaN' entries and negatives
    ord   = where((mgas EQ mgas) AND (mgas GT 0.))
    rmg   = rmean[ord]
    rmglo = rmlo[ord]
    rmghi = rmhi[ord]
    mgas  = mgas[ord]
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
    ids = ['tx', $
           'fe', $
;           'nh', $
           'sbr', $
           'ne', $
           'p', $
           'k', $
           'tc', $
           'm']

    rs = [ptr_new(rtx), $
          ptr_new(rfe), $
;          ptr_new(rtx), $
          ptr_new(rsbr), $
          ptr_new(rne), $
          ptr_new(rp), $
          ptr_new(rk), $
          ptr_new(rtc)]
;          ptr_new(rmg)]
    rlo = [ptr_new(rtxlo), $
           ptr_new(rfelo), $
;           ptr_new(rtxlo), $
           ptr_new(rslo), $
           ptr_new(rnelo), $
           ptr_new(rplo), $
           ptr_new(rklo), $
           ptr_new(rtclo)]
;           ptr_new(rmglo)]
    rhi = [ptr_new(rtxhi), $
           ptr_new(rfehi), $
;           ptr_new(rtxhi), $
           ptr_new(rshi), $
           ptr_new(rnehi), $
           ptr_new(rphi), $
           ptr_new(rkhi), $
           ptr_new(rtchi)]
;           ptr_new(rmghi)]
    ys = [ptr_new(tx), $
          ptr_new(fe), $
;          ptr_new(nh), $
          ptr_new(surbri), $
          ptr_new(nelec), $
          ptr_new(p), $
          ptr_new(k), $
          ptr_new(tc)]
;          ptr_new(mgas)]
    void, terr
    void, feerr
    void, nherr
    FOR h=0,n_elements(tlo)-1 DO IF tlo[h] GT thi[h] THEN push, terr, tlo[h] ELSE push, terr, thi[h]
    FOR h=0,n_elements(felo)-1 DO IF felo[h] GT fehi[h] THEN push, feerr, felo[h] ELSE push, feerr, fehi[h]
    FOR h=0,n_elements(nhlo)-1 DO IF nhlo[h] GT nhhi[h] THEN push, nherr, nhlo[h] ELSE push, nherr, nhhi[h]
    yerrs = [ptr_new(terr), $
             ptr_new(feerr), $
;             ptr_new(nherr), $
             ptr_new(sbrerr), $
             ptr_new(nelecerr), $
             ptr_new(perr), $
             ptr_new(kerr), $
             ptr_new(tcerr)]
;             ptr_new(merr)]
    ytexs = [textoidl('kT_X [keV]'), $
             textoidl('Z [Z'+sunsymbol()+']'), $
;             textoidl('Column Density [10^{20} cm^{-2}]'), $
             textoidl('SB [ct s^{-1} arcsec^{-2}]'), $
             textoidl('n_e [cm^{-3}]'), $
             textoidl('P [erg cm^{-3}]'), $
             textoidl('K [keV cm^2]'), $
             textoidl('t_{cool} [Gyr]')]
;             textoidl('M_{gas}(< r) [M'+sunsymbol()+']')]
    FOR j=0,n_elements(ys)-1 DO BEGIN
       id = ids[j]
       pcolor = colors[j]
       x = *rs[j]
       y = *ys[j]
       xerrlo = *rlo[j]
       xerrhi = *rhi[j]
       yerrlo = *yerrs[j]
       yerrhi = *yerrs[j]
       xmin = 0.8*min(x-xerrlo)
       IF (xmin LE 0.) THEN BEGIN
          print, 'Warning: xmin <= 0: ',xmin
          xmin = 0.8*min(x)
          print, 'Resetting xmin: ',xmin
       ENDIF
       xmax = 1.2*max(x+xerrhi)
       ymin = 0.8*min(y-yerrlo)
       ymax = 1.1*max(y+yerrhi)
       ord = where(y-yerrlo LE 0, count)
       IF (count GT 0) THEN BEGIN
          xul = x[ord]
          yul = y[ord]+yerrhi[ord]
          y[ord] = y[ord]+yerrhi[ord]
          yerrlo[ord] = 0
          yerrhi[ord] = 0
       ENDIF
       xtex = textoidl('R [kpc]')
       ytex = ytexs[j]
       xticks = loglevels([xmin,xmax],/fine)
       xnticks = n_elements(xticks)
       yticks = loglevels([ymin,ymax],/fine)
       ynticks = n_elements(yticks)
       IF ((id EQ 'tx') OR (id EQ 'fe') OR (id EQ 'nh')) THEN BEGIN
          ymin = 0.0
          plot, x, y, $
                /nodata, $
                XRANGE=[xmin,xmax], $
                YRANGE=[ymin,ymax], $
                /XLOG, /XSTY, /YSTY, $
                xtitle = xtex, $
                ytitle = ytex, $
;                xticks = xnticks-1, $
;                xtickv = xticks, $
                CHARSIZE = csize
       ENDIF ELSE BEGIN
          ord = where(y-yerrlo GT 0.)
          ymin = 0.9*min(y[ord]-yerrlo[ord])
          ymax = 1.1*max(y+yerrhi)
          IF ((id EQ 'sbr') OR (id EQ 'p') OR (id EQ 'ne')) THEN ymax = 1.75*max(y+yerrhi)
          IF (id EQ 'sbr') THEN ymin = 1d-9
          IF (id EQ 'ne') THEN ymin = 1d-4
          IF (id EQ 'tc') THEN yticks[4] = age
          IF ((id EQ 'p') OR (id EQ 'ne')) THEN xticks[3] = 60.
          plot, x, y, $
                /nodata, $
                XRANGE = [xmin,xmax], $
                YRANGE = [ymin,ymax], $
                /XSTY, /YSTY, $
                /XLOG, /YLOG, $
                xtitle = xtex, $
                ytitle = ytex, $
;                yticks = ynticks-1, $
;                ytickv = yticks, $
;                xticks = xnticks-1, $
;                xtickv = xticks, $
                CHARSIZE = csize
       ENDELSE
       IF ((id EQ 'tx') OR (id EQ 'p') OR (id EQ 'ne')) THEN BEGIN
          dy = maken(1d-20,1d20,1d4)
          oplot, replicate(60.,n_elements(dy)), dy, psym=0, linestyle=2, thick=2, color=0
       ENDIF
       IF (id EQ 'tc') THEN BEGIN
          dx = maken(1d-2,1d5,1d6)
          oplot, dx, replicate(age,n_elements(dx)), psym=0, linestyle=2, thick=2, color=0
       ENDIF
;;        IF id EQ 'sbr' THEN BEGIN
;;           dx = maken(1d-2,1d5,1d6)
;;           a = [1.63d-3, 0.59, 34.4, 1.35d-8]
;;           dy = (a[0]*(1.0+(dx/a[2])^2.)^(-3.0*a[1]+0.5))+a[3]
;;           oplot, dx, dy, psym=0, linestyle=2, thick=2, color=0
;;        ENDIF
;;        IF id EQ 'nh' THEN BEGIN
;;           dx = maken(1d-2,1d5,1d6)
;;           oplot, dx, replicate(fidnh,n_elements(dx)), psym=0, linestyle=2, thick=2, color=0
;;        ENDIF
;;        IF id EQ 'tx' THEN BEGIN
;;           readcol,vikfile,FORMAT='A,F,F', comment='#', par,val,sig
;;           tmin = val[0]
;;           t0 = val[1]
;;           rt = val[2]
;;           rcool = val[3]
;;           acool = val[4]
;;           ap = val[5]
;;           b = val[6]
;;           c = val[7]
;;           dx = maken(1d-2,1d5,1d6)
;;           num1  = ((dx/rcool)^acool)+(tmin/t0)
;;           den1  = 1.+((dx/rcool)^acool)
;;           num2  = (dx/rt)^(-ap)
;;           den2  = (1.+(dx/rt)^b)^(c/b)
;;           term1 = num1/den1
;;           term2 = num2/den2
;;           dy   = t0*term1*term2
;;           oplot, dx, dy, psym=0, linestyle=2, thick=2, color=0
;;        ENDIF
       IF (count GT 0) THEN BEGIN
          plotsym, 1, 2.0
          oplot, xul, yul, psym=8
       ENDIF
       plotsym, 0, psize, /fill
       oploterror, x, y, xerrlo, yerrlo, psym=8, /lobar
       oploterror, x, y, xerrhi, yerrhi, psym=8, /hibar
       oplot, x, y, PSYM=8, color=0
       plotsym, 0, psize-0.2*psize, /fill
       oplot, x, y, PSYM=8, color=pcolor
       IF (id EQ 'tx') THEN BEGIN
          plotsym, 4, psize, /fill
          oploterror, rdtx, dtx, rdtxlo, dtlo, psym=8, /lobar
          oploterror, rdtx, dtx, rdtxhi, dthi, psym=8, /hibar
          oplot, rdtx, dtx, PSYM=8, color=0
          plotsym, 4, psize-0.2*psize, /fill
          oplot, rdtx, dtx, PSYM=8, color=250
       ENDIF
;;        IF (id EQ 'k') THEN BEGIN
;;           dumx = maken(5,1000,1000)
;;           kfit = k0+k100*(dumx/100)^(alpha)
;;           oplot, dumx, kfit, psym=0, linestyle=2, thick=2, color=0
;;           items = [textoidl('K_0 = '+num2str(k0,3)+' \pm '+num2str(k0err,2)+' keV cm^2'), $
;;                    textoidl('K_{100} = '+num2str(k100,3)+' \pm '+num2str(k100err,2)+' keV cm^2'), $
;;                    textoidl('\alpha = '+num2str(alpha,3)+' \pm '+num2str(alphaerr,2))]
;;           linearr = replicate(-99,n_elements(items))
;;           psyarr = replicate(-99,n_elements(items))
;;           legend, items, linestyle=larr, psym=parr, box=0, charsize=1.0, /top, /left, /fill
;;        ENDIF
    ENDFOR

dat1 = '/mnt/DROBO/10445/reprocessed/10445_fluxprof_sbprof_2pix.dat'
readcol, dat1, FORMAT='F,F,F,F,F,D,D,D,D', comment='#', $
         ann, cell, rin, rout, rmid, flux, ferr, lum, lerr
lerr = 3*lerr
cosmology, z, result, /silent
r   = rmid*0.492*result[4]
rlo = r-(rin*0.492*result[4])
rhi = (rout*0.492*result[4])-r
flo = ferr
fhi = ferr
llo = lerr
lhi = lerr
dum = 0.0
dumerr = 0.0
FOR i=0,n_elements(lum)-1 DO BEGIN
   dum = dum+lum[i]
   dumerr = dum*sqrt((lerr[i]/lum[i])^2.+(dumerr/dum)^2.)
   push, tlum, dum
   push, tlumerr, dumerr
ENDFOR
tlum = tlum*0.855
x = r
xerrlo = rlo
xerrhi = rhi
y = tlum
yerrlo = tlumerr
yerrhi = tlumerr
ymin = 0.8*min(tlum)
ymax = 1.2*max(tlum)
ord = where(r LE 197.064)
ymax = 1.2*max(tlum[ord])
yticks = [6d43, cavheat, lcool, 3d45]
ynticks = n_elements(yticks)
plot, x, y, $
      /nodata, $
      XRANGE = [xmin,xmax], $
      YRANGE = [ymin,ymax], $
      /XSTY, /YSTY, $
      /XLOG, /YLOG, $
      xtitle = textoidl('R [kpc]'), $
      ytitle = textoidl('L_X (r < R) [erg s^{-1}]'), $
      yticks = ynticks-1, $
      ytickv = yticks, $
;      xticks = xnticks-1, $
;      xtickv = xticks, $
      CHARSIZE = csize
oplot, maken(1d-100,1d100,10), replicate(lcool,10), linestyle=2, thick=2
oplot, maken(1d-100,1d100,10), replicate(cavheat,10), linestyle=3, thick=2
plotsym, 0, psize, /fill
oploterror, x, y, xerrlo, yerrlo, psym=8, /lobar
oploterror, x, y, xerrhi, yerrhi, psym=8, /hibar
oplot, x, y, PSYM=8, color=0
plotsym, 0, psize-0.2*psize, /fill
j = n_elements(colors)-1
oplot, x, y, PSYM=8, color=colors[j]
device, /close

ERROR:
SKIPOBS:
    prevname = name
    counter--
ENDFOR
set_plot, 'X'
END
