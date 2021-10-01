PRO itpl_flat

showme = 'no'
myhome = GETENV('HOME')
restore,myhome+'/research/redux/scripts/s_resultstemplate.sav'
restore,myhome+'/research/redux/scripts/s_tabletemplate.sav'
restore,myhome+'/research/redux/scripts/xspectemp_rin_normerr_src.sav'        
readcol,'../pf_info/done.list',FORMAT='A,A,F,F,F,F,F,F,F,F,F,A,F,A,F,A', comment='#', $
        cluster,obsids,x,y,rmax,mincts,z,nh,tx,fe,lbol,chip,eobs,diff,robs,loc
fitfile = read_ascii('dat/pf_temp_profs.dat',template=xspectemp_rin_normerr_src)

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

   ;# check for file existance
   file1 = myhome+'/research/pf_clusters/pf_fits/tables/'+obsid+'_table.dat'
   check = findfile(file1,count=count1)
   IF (count1 NE 1) THEN GOTO,ERROR
   file2 = myhome+'/research/pf_clusters/pf_fits/s_results/'+obsid+'_results.log'
   check = findfile(file2,count=count2)
   IF (count2 NE 1) THEN GOTO,ERROR
   ord  = where(fitfile.obsid EQ obsid)
   IF ord[0] EQ -1 THEN GOTO,ERROR

   ;# read in data
   dataobs = read_ascii(file1, template = s_tabletemplate)
   datafit = read_ascii(file2, template = s_resultstemplate)
   tx   = fitfile.tx[where(fitfile.obsid EQ obsid)]
   txhi = fitfile.thi[where(fitfile.obsid EQ obsid)]
   txlo = fitfile.tlo[where(fitfile.obsid EQ obsid)]
   ttlo  = tx-txlo
   tthi  = txhi-tx

   ;# get central entropy value, either obs or fit
   num = n_elements(dataobs.k)
   push, names, name
   push, obs, obsid
   push, obsitpl, dataobs.k[num-1]
   push, obsitplerr,  dataobs.k_err[num-1]
   push, obsflat, dataobs.k_flat[num-1]
   push, obsflaterr,  dataobs.k_err[num-1]
   push, fititpl, datafit.k0[0]
   push, fititplerr, datafit.k0err[0]
   push, fitflat, datafit.k0[2]
   push, fitflaterr, datafit.k0err[2]
   push, tx0, tx[0]
   push, tlo, ttlo[0]
   push, thi, tthi[0]
   IF tx[0] lt 1. THEN print, FORMAT='(A-20,A10,A10,F10.2,A4,F5.2,F5.2)','TX(0) < 1.0: ',name, obsid, tx[0], '+/-', ttlo[0], tthi[0]
   IF tlo[0] GT thi[0] THEN push, tx0err, tlo[0] ELSE push, tx0err, thi[0]
    
ERROR:
ENDFOR

;# make array for overplotting
ox = maken(0.001,10000,10)
oy = ox

;# build the ratio array
ratio = fitflat/fititpl
ratioerr = (fitflat/fititpl)*sqrt((fitflaterr/fitflat)^2+(fititplerr/fititpl)^2.)

ord = where(ratio LT 1)
print,obs[ord]

;# print out oddities
IF showme EQ 'yes' THEN BEGIN
   ord = where((ratio-ratioerr GT 1.) OR (ratio+ratioerr LT 1.)) 
   FOR i=0,n_elements(ord)-1 DO BEGIN
      t = ord[i]
      print, FORMAT='(A-20,A20,A10,A10,A10)',"Name","Obsid","K0 flat","K0 itpl","Ratio"
      print, FORMAT='(A-20,A20,F10.2,F10.2,F10.2)',names[t],obs[t],fitflat[t],fititpl[t],ratio[t]
      tord  = where(fitfile.cluster EQ names[t])
      rin  = fitfile.rin[tord]
      rout = fitfile.rout[tord]
      r    = (rin+rout)/2.
      tx   = fitfile.tx[tord]
      print, 'Tdiff: ',max(tx)/min(tx)
      txhi = fitfile.thi[tord]
      txlo = fitfile.tlo[tord]
      ttlo = tx-txlo
      tthi = txhi-tx
      plotsym, 0, /fill
      xmin = min(rin)
      xmax = max(rout)
      ymin = 0.8*min(txlo)
      ymax = 1.2*max(txhi)
      plot, r, tx, psym=8, $
            /xsty, /ysty, $
            xrange=[xmin,xmax], $
            yrange=[ymin,ymax], $
            xtitle = 'R [arcmin]', $
            ytitle = 'Tx [keV]', $
            title  = names[t]+' '+obs[t]
      oploterror, r, tx, ttlo, psym=8, /lobar, /nohat
      oploterror, r, tx, tthi, psym=8, /hibar, /nohat
      yesno = ''
      read, 'Press enter to continue', yesno
   ENDFOR
ENDIF

;# filter for neg. ratio and inf
ord = WHERE(FINITE(ratio, /INFINITY),count)
IF count NE 0 THEN ratio[ord] = 0.
ord = where(ratio GT 0.)
ratio = ratio[ord]
ratioerr = ratioerr[ord]
names = names[ord]
tx0 = tx0[ord]
tx0err = tx0err[ord]
tlo = tlo[ord]
thi = thi[ord]

set_plot, 'PS'
loadct, 13, /silent
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 3
!Y.THICK  = 3
!Z.THICK  = 3
fnames  = ['itplflat_rat.eps','itpl_flat.eps', 'obsk_fitk_itpl.eps', 'obsk_fitk_flat.eps']
titles  = ['',$
           textoidl('Fit (\DeltaT_{center}=0) vs. Fit (\DeltaT_{center}=0)'), $
           textoidl('(\DeltaT_{center}=0) Obs vs. Fit'), $
           textoidl('(\DeltaT_{center}=0) Obs vs. Fit')]
xtitles = [textoidl('K_0(\DeltaT_{center}=0) [keV cm^2]'), $
           textoidl('K_0(\DeltaT_{center}=0)[keV cm^2]'), $
           textoidl('K_0 Obs [keV cm^2]'), $
           textoidl('K_0 Obs [keV cm^2]')]
ytitles = [textoidl('K_0(\DeltaT_{center}=0) / K_0(\DeltaT_{center}\neq0)'), $
           textoidl('K_0(\DeltaT_{center}=0) [keV cm^2]'), $
           textoidl('K_0 Fit [keV cm^2]'), $
           textoidl('K_0 Fit [keV cm^2]')]
x = [ptr_new(fitflat), ptr_new(fititpl),ptr_new(obsitpl),ptr_new(obsflat)]
y = [ptr_new(ratio), ptr_new(fitflat),ptr_new(fititpl),ptr_new(fitflat)]
xerr = [ptr_new(tx0err),ptr_new(fititplerr),ptr_new(obsitplerr),ptr_new(obsflaterr)]
yerr = [ptr_new(ratioerr), ptr_new(fitflaterr),ptr_new(fititplerr),ptr_new(fitflaterr)]

FOR jj=0,n_elements(fnames)-1 DO BEGIN
   px = *x[jj]
   py = *y[jj]
   pxerr = *xerr[jj]
   pyerr = *yerr[jj]
   device, filename=fnames[jj], $
;           /color, $
           /encapsulated, $
           /portrait, $
           /helvetica
   plotsym, 0, 0.6, /fill
   IF jj EQ 0 THEN BEGIN
      xmin = 0.8*min(px)
      xmax = 1.2*max(px)
      ymin = 0.8*min(py)
      ymax = 1.1*max(py)
      plot, px, py, $
            xtitle = xtitles[jj], $
            ytitle = ytitles[jj], $
            xran = [xmin,xmax], $
            yran = [ymin,ymax], $
            /xsty, /ysty, $
            /XLOG, $
            psym = 8, $
            charsize = 1.0
      oploterror, px, py, tlo, pyerr, psym=8, /lobar, /nohat
      oploterror, px, py, thi, pyerr, psym=8, /hibar, /nohat
      ord = where((py-pyerr GT 1.) OR (py+pyerr LT 1.))
      yr = py[ord]
      xr = px[ord]
      yrerr = pyerr[ord]
      xrlo  = tlo[ord]
      xrhi  = thi[ord]
;      oploterror, xr, yr, xrlo, yrerr, psym=8, /lobar, color=250, /nohat
;      oploterror, xr, yr, xrhi, yrerr, psym=8, /hibar, color=250, /nohat
;      oplot, xr, yr, psym=8, color=250
      plotsym, 8, 1.0, /fill
      oploterror, xr, yr, xrlo, yrerr, psym=8, /lobar, /nohat
      oploterror, xr, yr, xrhi, yrerr, psym=8, /hibar, /nohat
      oplot, xr, yr, psym=8
   ENDIF ELSE BEGIN
      xmin = 1.
      xmax = 1.2*max(px)
      ymin = xmin
      ymax = xmax
      plot, px, py, $
            title = titles[jj], $
            xtitle = xtitles[jj], $
            ytitle = ytitles[jj], $
            xran = [xmin,xmax], $
            yran = [ymin,ymax], $
            /xsty, /ysty, $
            /XLOG, /YLOG, $
            psym = 8, $
            charsize = 1.0
      oploterror, px, py, pxerr, pyerr, psym=8, /nohat
   ENDELSE
   IF jj NE 0 THEN oplot, ox, oy, linestyle=2 ELSE oplot, ox, replicate(1.,n_elements(ox)), linestyle=2
   device,/close
ENDFOR
END
