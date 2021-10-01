PRO turb_kunz, dat1

;####################
;####################

myhome  = GETENV('HOME')
tempfitfile = myhome+'/research/pf_clusters/pf_fits/dat/pf_temp_profs.dat'
finalps = 'turb_final.ps'
datadir = '/mnt/DROBO'
redir   = 'reprocessed'
mkps    = 'yes'
mkwww   = 'no'
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

;# Device settings
set_plot, 'PS'
loadct, 39, /silent
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 1
!Y.THICK  = 1
!Z.THICK  = 1

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

    ;# tx profile
    ord = where(fitfile.obsid EQ obsid)
    IF ord[0] EQ -1 THEN BEGIN
       print, "## ERROR: Missing Tx for",obsid
       GOTO, ERROR
    ENDIF

    ;# read in data
    dataobs = read_ascii(file1, template = s_tabletemplate)
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

    ;# get rid of the 'NaN' entries and negatives
    ord      = where((nelec EQ nelec) AND (nelec GT 0.))
    rne      = rmean[ord]
    rnelo    = rmlo[ord]
    rnehi    = rmhi[ord]
    nelec    = nelec[ord]
    nelecerr = nelecerr[ord]

    ;# calculate turbulent model profs
    psi = 0.75
    talpha = 1.0
    trne = reverse(rne)
    tnelec = reverse(nelec)
    txint = interpol(tx, rtx, trne)
    bturb = 11.0*psi^(-1./2.)*sqrt(tnelec/0.1)*(txint/2.0)^(3./4.)
    uturb = 70.0*psi^(-1./2.)*(txint/2.0)^(3./4.)
    mturb = 0.18*psi^(-1./2.)*(txint/2.0)^(1./4.)
    lturb = 0.2*talpha*psi^(-3./2.)*(tnelec/0.1)^(-1.0)*(txint/2.0)^(7./4.)
    kturb = 3d27*psi^(-2.0)*(tnelec/0.1)^(-1.0)*(txint/2.0)^(5./2.)
    rturb = trne
    turb = [ptr_new(bturb), $
            ptr_new(uturb), $
            ptr_new(lturb), $
            ptr_new(kturb)]
    turbtex = [textoidl('B [\muG]'),$
               textoidl('U_{rms} [km s^{-1}]'),$
               textoidl('L [kpc]'),$
               textoidl('\kappa_{turb} [cm^2 s^{-1}]')]

    ;# turbulence
    !P.MULTI  = [0,2,2,0]
    device, filename=name+'_turb.ps', $
            /color, $
            /portrait, $
            set_font='Times-Roman', $
            bits=16
    FOR j=0,n_elements(turb)-1 DO BEGIN
       x = rturb
       y = *turb[j]
       xtex = textoidl('R [kpc]')
       ytex = turbtex[j]
       xmin = 0.8*min(x)
       xmax = 1.2*max(x)
       ny   = y[where(y GT 0.)]
       ymin = 0.8*min(ny)
       ymax = 1.2*max(ny)
       yticks = LOGLEVELS([ymin,ymax],/fine)
       ynticks = N_Elements(yticks)
       xticks = LOGLEVELS([xmin,xmax],/fine)
       xnticks = N_Elements(xticks)
       plot, x, y, $
             /nodata, $
             XRANGE=[xmin,xmax], $
             YRANGE=[ymin,ymax], $
             /YLOG, /XLOG, /XSTY, /YSTY, $
             xtitle = xtex, $
             ytitle = ytex, $
             CHARSIZE = csize
       plotsym, 0, psize, /fill
       oplot, x, y, PSYM=8, color=0
       plotsym, 0, psize-0.2*psize, /fill
       oplot, x, y, PSYM=8, color=pcolor
    ENDFOR
    device, /close

ERROR:
counter--
ENDFOR
set_plot, 'X'
IF (mkps EQ 'yes') THEN BEGIN
   SPAWN, 'ls -1 *turb.ps > list'
   SPAWN, 'cat list | pscat 1 '+finalps
   SPAWN, 'rm -f list'
   SPAWN, 'rm -f *turb.ps'
ENDIF
END
