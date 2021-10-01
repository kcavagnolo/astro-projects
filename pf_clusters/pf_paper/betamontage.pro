PRO betamontage

redir = 'reprocessed'
bin = '10pix'

set_plot, 'PS'
device, $
   filename = 'beta.eps', $
   /encapsulated, $
   /portrait, $
   /helvetica, $
   bits = 16
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
multiplot,[3,6], $
;          /doxaxis, $
          /doyaxis, $
;          gap = 0.015, $
          xgap = 0.015, $
          mxtitle = textoidl('R_{mid} [arcsec]'), $
          mytitle = textoidl('Surface Brightness [cts arcsec^{-2} s^{-1}]')

readcol, '../pf_fits/dat/betafits.dat', $
         format='(A,A,F,F,F,F,F,F,F,F,F,F,F,F,F)', $
         comment='#', $
         bcluster, bobsid, $
         s01, s01err, s02, s02err, $
         rc1, rc1err, rc2, rc2err, $
         beta1, beta1err, beta2, beta2err, $
         dof, chi

readcol, '../pf_info/bfits.list', $
         FORMAT='A,A,F,F,F,F,F,F,F,F,F,A,F,A,F,A', $
         comment='#', $
         cluster,obsids,xs,ys,rmaxs,minctss,zs,nhs,txs,fes,$
         lbols,chips,eobss,diffs,robss,locs

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
FOR i = 0,n_elements(obsids)-1 DO BEGIN
   obsid = strcompress(obsids[i],/remove_all)
   name = strcompress(cluster[i],/remove_all)
   datadir = strcompress(locs[i],/remove_all)
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
   IF multi EQ 'no' THEN BEGIN
      sbrfile = datadir+'/'+obsid+'/'+redir+'/'+obsid+'_sbprof_'+bin+'.fits'
      excor   = datadir+'/'+obsid+'/'+redir+'/'+obsid+'_expcorr_'+bin+'.fits'
   ENDIF ELSE BEGIN
      sbrfile = datadir+'/merged/'+name+'/'+obsid+'_sbprof_'+bin+'.fits'
      excor   = datadir+'/merged/'+name+'/'+obsid+'_expcorr_'+bin+'.fits'
   ENDELSE
   check  = findfile(excor,count=count)
   IF (count NE 1) THEN BEGIN
      excor = 'nihil'
      print, "## WARNING WARNING: No exposure correction for ",obsid
   ENDIF
   check = findfile(sbrfile,count=count)
   IF (count NE 1) THEN GOTO, ERROR

   ;# surf bri
   fits   = mrdfits(sbrfile,1)
   rsbr   = fits.rmid*0.492
   exp    = fits.exposure
   sbr    = fits.sur_bri/exp/(0.492^2.)
   sbrerr = fits.sur_bri_err/exp/(0.492^2.)

   ;# exposure correction
   IF excor NE 'nihil' THEN BEGIN
      expprof = mrdfits(excor,1,/silent)
      expcorr = expprof.sur_bri
      expcorrerr = expprof.sur_bri_err
      expcorrerr = expcorrerr/max(expcorr)
      expcorr = expcorr/max(expcorr)
      sbr = sbr/expcorr
      sbrerr = sbrerr/expcorr
   ENDIF

   ;# beta data
   ord = where(bcluster EQ name)
   s1 = s01[ord]
   r1 = rc1[ord]
   b1 = beta1[ord]
   s2 = s02[ord]
   r2 = rc2[ord]
   b2 = beta2[ord]
   IF r2 EQ 0 THEN $
      y = (s1[0]*(1.+(rsbr/r1[0])^2.)^(-3.*b1[0]+0.5)) $
   ELSE $
      y = (s1[0]*(1.+(rsbr/r1[0])^2.)^(-3.*b1[0]+0.5))+(s2[0]*(1.+(rsbr/r2[0])^2.)^(-3.*b2[0]+0.5))
   yran = sbr[where(sbr GT 0)]
   ymin = 0.9*min(yran)
   ymax = 1.1*max(sbr+sbrerr)
   xmin = 1
   xmax = 1d3
   yticks = loglevels([ymin,ymax],coarse=4)
   ynticks = N_Elements(yticks)
   xticks = loglevels([xmin,xmax],coarse=4)
   xnticks = N_Elements(xticks)
   plotsym, 0, 0.25, /fill
   plot, rsbr, sbr, $
         xrange = [xmin,xmax], $
         yrange = [ymin,ymax],$
         /xsty, /ysty, $
         /xlog, /ylog, $
         psym = 8, $
         charsize = 0.4, $
;         ytickformat='(A1)', $
;         xtickformat='(A1)', $
;         xtickformat='exponent', $
         ytickformat='exponent', $
;         xticks = xnticks-1, $
;         xtickv = xticks, $
         yticks = ynticks-1, $
         ytickv = yticks
   oploterror, rsbr, sbr, sbrerr, psym=8, /nohat
   oplot, rsbr, y
   items = [cname[i]]
   larr = replicate(-99,n_elements(items))
   parr = replicate(-99,n_elements(items))
   legend, items, linestyle=larr, psym=parr, box=0, charsize=0.6, /bottom, /left, /fill
   multiplot

ERROR:
ENDFOR
device,/close
set_plot, "X"
END
