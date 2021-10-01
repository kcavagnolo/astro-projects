PRO cooltime, dat1

;# NAME:
;#
;# PURPOSE:
;#
;# EXPLANATION:
;#
;# CALLING SEQUENCE:
;#
;# INPUTS:
;#          
;# OUTPUTS:
;#
;# MODIFICATION HISTORY:
;#
;######################
;######################
;#      OPTIONS       #
;######################
;######################

myhome = GETENV('HOME')
xspecd = GETENV('HEADAS')
txfile  = myhome+'/research/rbs_797/data/tx_2.5K/proj_1T_nhfro.dat'
;txfile  = myhome+'/research/iras09/data/tx_2.5K/proj_1T_nhfro.dat'
;txfile = myhome+'/research/pf_clusters/pf_fits/dat/pf_temp_profs.dat'
tcbin  = 0.15                   ;# binning factor for tcool histogram **LOG SPACE**
csize  = 0.8                    ;# size of characters in plots
hcolor = 100                    ;# color to fill histograms
rsplo  = 0.01
rsphi  = 100.0
fluxlo = rsplo
fluxhi = rsphi
h0     = 70.
omegam = 0.3
omegal = 0.7
xcmd   = '/usr/local/headas/x86_64-unknown-linux-gnu/bin/xspec11'
keVerg = 1.60217646d-9
secGyr = 3.1556926d16

;######################
;######################

;# read necessary templates
restore, '~/research/redux/scripts/xspectemp_rin_normerr_src.sav'
restore, "~/research/redux/scripts/s_tabletemplate.sav"
readcol, dat1, FORMAT='A,A,F,F,F,F,F,F,F,F,F,A,F,A,F,A', comment='#', $
         cluster, obsids, xs, ys, rmaxs, minctss, zs, nhs,$
         txs, fes, lbols, chips, eobss, diffs, robss, locs
fitfile = read_ascii(txfile,template=xspectemp_rin_normerr_src)

;# start looping
FOR i = 0,n_elements(obsids)-1 DO BEGIN
    obsid = strcompress(obsids[i],/remove_all)
    name = strcompress(cluster[i],/remove_all)
    ord = where(cluster EQ name)
    IF n_elements(ord) EQ 1 THEN obsid = obsid ELSE BEGIN
        temp = obsids[ord]
        FOR j=1,n_elements(temp)-1 DO BEGIN
            obsid = obsid+'_'+strcompress(temp[j],/remove_all)
        ENDFOR
    ENDELSE

    ;# table.dat
    file1 = '~/research/pf_clusters/pf_fits/tables/'+obsid+'_table.dat'
    check = findfile(file1,count=count1)
    IF (count1 NE 1) THEN GOTO,ERROR
    data = read_ascii(file1, template = s_tabletemplate)

    ;# tx profile
    ord = where(fitfile.obsid EQ obsid)
    IF ord[0] EQ -1 THEN GOTO, ERROR

    ;# build arrays of data
    rinmpc   = reverse(data.rin_mpc)
    routmpc  = reverse(data.rout_mpc)
    nelec    = reverse(data.n_elec)
    nelecerr = reverse(data.sigma_ne)
    rmean    = ((rinmpc+routmpc)/2.)*1000.

    ;# get Tx values
    z = zs[i]
    cosmology, z, result, hubcon=h0, cosdens=omegal, matdens=omegam, /silent
    rin  = fitfile.rin[where(fitfile.obsid EQ obsid)]
    rout = fitfile.rout[where(fitfile.obsid EQ obsid)]
    rtx  = (((rin+rout)/2.)*60.)*result[4]
    nh   = fitfile.nh[where(fitfile.obsid EQ obsid)]
    nhlo = fitfile.nlo[where(fitfile.obsid EQ obsid)]
    nhhi = fitfile.nhi[where(fitfile.obsid EQ obsid)]
    tx   = fitfile.tx[where(fitfile.obsid EQ obsid)]
    thi  = fitfile.thi[where(fitfile.obsid EQ obsid)]
    tlo  = fitfile.tlo[where(fitfile.obsid EQ obsid)]
    fe   = fitfile.fe[where(fitfile.obsid EQ obsid)]
    felo = fitfile.felo[where(fitfile.obsid EQ obsid)]
    fehi = fitfile.fehi[where(fitfile.obsid EQ obsid)]
    norm   = fitfile.norm[where(fitfile.obsid EQ obsid)]
    normlo = fitfile.normlo[where(fitfile.obsid EQ obsid)]
    normhi = fitfile.normhi[where(fitfile.obsid EQ obsid)]

    ;# fill err array
    void, tx_err
    void, fe_err
    void, norm_err
    void, nh_err
    void, lambda
    FOR ss=0,n_elements(tx)-1 DO BEGIN
        IF (tx[ss]-tlo[ss] GT thi[ss]-tx[ss]) THEN $
          push, tx_err, tx[ss]-tlo[ss] ELSE $
          push, tx_err, thi[ss]-tx[ss]
        IF (fe[ss]-felo[ss] GT fehi[ss]-fe[ss]) THEN $
          push, fe_err, fe[ss]-felo[ss] ELSE $
          push, fe_err, fehi[ss]-fe[ss]
        IF (norm[ss]-normlo[ss] GT normhi[ss]-norm[ss]) THEN $
          push, norm_err, norm[ss]-normlo[ss] ELSE $
          push, norm_err, normhi[ss]-norm[ss]
        IF (nh[ss]-nhlo[ss] GT nhhi[ss]-nh[ss]) THEN $
          push, nh_err, nh[ss]-nhlo[ss] ELSE $
          push, nh_err, nhhi[ss]-nh[ss]
    ENDFOR

    ;# interpolate tx to dens grid
    temp1 = interpol(tx, rtx, rmean)
    temp2 = interpol(tx_err, rtx, rmean)
    wcentral = where((rmean GE 0.0) AND (rmean LE min(rtx)),nwcentral)

    IF (tx[0] LT tx[1] AND nwcentral GT 0) THEN BEGIN
       ;# the central error should be that of the last data point
       wouter = where(rmean GT min(rtx))
       wmintx = where(rtx EQ min(rtx))
       xc     = findgen(nwcentral)+1.0
       xc     = xc/xc
       t_central    = xc * tx(wmintx[0])
       terr_central = xc * tx_err(wmintx[0])

       ;# remember indices are 'flipped' (inner is outer)
       ;# a vector where the central pts=tx(rmin)
       txitpl = [t_central, temp1(wouter)]
       txerr  = [terr_central, temp2(wouter)]
    ENDIF ELSE BEGIN
       txitpl = temp1
       txerr  = temp2
    ENDELSE

    ;# build a model for each annulus to get lambda(T)
    FOR j=0,n_elements(tx)-1 DO BEGIN
        OPENW, XLUN, 'temp.xcm', /GET_LUN
        mtx = tx[j]
        mfe = fe[j]
        mnr = 1.0
        mz  = 0.
        printf, XLUN, 'chatter 0 0'
        printf, XLUN, 'query yes'
        printf, XLUN, 'cosmo '+$
                strcompress(h0,/remove_all)+' '+$
                strcompress(omegam,/remove_all)+' '+$
                strcompress(omegal,/remove_all)
        printf, XLUN, 'model mekal & '+$
                strcompress(mtx,/remove_all)+' & 1.0 & '+$
                strcompress(mfe,/remove_all)+',0 & '+$
                strcompress(mz,/remove_all)+' & 0 & '+$
                strcompress(mnr,/remove_all)
        printf, XLUN, 'dummyrsp '+$
                strcompress(rsplo,/remove_all)+' '+$
                strcompress(rsphi,/remove_all)
        printf, XLUN, 'flux '+$
                strcompress(fluxlo,/remove_all)+' '+$
                strcompress(fluxhi,/remove_all)
        printf, XLUN, 'tclout flux'
        printf, XLUN, 'log temp.log'
        printf, XLUN, 'puts $xspec_tclout'
        printf, XLUN, 'log none'
        printf, XLUN, 'exit'
        CLOSE, XLUN
        FREE_LUN, XLUN
        SPAWN, xcmd+' - temp.xcm'
        xout = rd_tfile('temp.log',/auto,/nocomment)
        push, lambda, 1d-14*xout(0,1)
    ENDFOR

    ;# Voigt and Fabian 2004 MNRAS 347, 1130
    ;ttcool = (3.6*txitpl*keVerg)/(nelec*lamitpl)/secGyr
    ;ftcool = (5.4*txitpl*keVerg)/(nelec*lamitpl)/secGyr

    ;# calculate cooling times using equation:
    ;# tc = energy available/energy radiated
    ;# tc = (alpha*nkT)/emissivity
    ;# tc ~ kT/lambda*n_elec
    lamitpl = interpol(lambda, rtx, rmean)
    ftcool = (((5./2.)*(2.3/1.2)*txitpl*keVerg)/((nelec/1.2)*lamitpl))/secGyr
    ttcool = (((3./2.)*(2.3/1.2)*txitpl*keVerg)/((nelec/1.2)*lamitpl))/secGyr
    ftcoolerr = ftcool*sqrt((txerr/txitpl)^2.+(nelecerr/nelec)^2.)
    ttcoolerr = ttcool*sqrt((txerr/txitpl)^2.+(nelecerr/nelec)^2.)
    push, allftc, ftcool[0]
    push, allttc, ttcool[0]

;    rint  = maken(0.25*min(rmean),max(rmean),100000)
;    ftint = interpol(ftcool,rmean,rint)
;    ttint = interpol(ttcool,rmean,rint)
;    ord   = where((rint GT 9.99) AND (rint LT 10.01))
;    ord   = ord[0]
;    push, ftenkpc, ftint[ord]
;    push, ttenkpc, ttint[ord]
;    OPENW, OUTLUN, obsid+'_tcool_itpl.dat', /GET_LUN

    OPENW, OUTLUN, obsid+'_tcool.dat', /GET_LUN
    printf, OUTLUN, format='(11A15)','#rin','rout','nelec','ne_err','tx','tx_err','lambda','5/2tc','tc_err','3/2tc','tc_err'
    printf, OUTLUN, format='(11A15)','#Mpc','Mpc','cm^-3','cm^-3','keV','keV','e cm^3/s','Gyr','Gyr','Gyr','Gyr'
    outarray = [ [rinmpc], [routmpc], $
                 [nelec], [nelecerr], $
                 [txitpl], [txerr], [lamitpl], $
                 [ftcool], [ftcoolerr], $
                 [ttcool], [ttcoolerr] ]
    outarray = transpose(outarray)
    printf, OUTLUN, format='(11E15.5)', outarray
    CLOSE, OUTLUN
    FREE_LUN, OUTLUN

ERROR:
ENDFOR

;# hisotgram of tcool dist
;; set_plot, 'PS'
;; device, filename='tclamhist.eps', $
;;         /color, $
;;         /encapsulated, $
;;         /portrait, $
;;         /helvetica
;; !FANCY    = 4
;; !LINETYPE = 0
;; !P.FONT   = 0
;; !X.THICK  = 3
;; !Y.THICK  = 3
;; !Z.THICK  = 3
;; histoplot, alog10(allftc), tcbin, $
;;            /drawlines, /colorfill, fillcolor = hcolor, $
;;            xtitle = textoidl('log Central Cooling Time [Gyr]'), $
;;            ytitle = textoidl('Number of Clusters'), $
;; ;           position= ASPECT(1.0), $
;;            charsize= csize
;; device, /close

;; ;# hisotgram of tcool dist
;; set_plot, 'PS'
;; device, filename='tclamhist_10kpc.eps', $
;;         /color, $
;;         /encapsulated, $
;;         /portrait, $
;;         /helvetica
;; !FANCY    = 4
;; !LINETYPE = 0
;; !P.FONT   = 0
;; !X.THICK  = 3
;; !Y.THICK  = 3
;; !Z.THICK  = 3
;; histoplot, alog10(ftenkpc), tcbin, $
;;            /drawlines, /colorfill, fillcolor = hcolor, $
;;            xtitle = textoidl('log Cooling Time at 10 kpc [Gyr]'), $
;;            ytitle = textoidl('Number of Clusters'), $
;; ;           position= ASPECT(1.0), $
;;            charsize= csize
;; device, /close

SPAWN, 'rm -f temp.xcm temp.log xautosav.xcm'

END
