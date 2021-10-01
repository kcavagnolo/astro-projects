PRO k0z, dat1

;# ie:
;# splots, 'reference.list'
k0val = 'fit'
ktype = 'flat'
model = 'nonzero'
readcol, dat1, FORMAT='A,A,F,F,F,F,F,F,F,F,F,A,F,A,F,A', comment='#', $
         cluster, obsids, x, y, rmax, mincts, z, nh, tx, fe, lbol, chip, eobs, diff, robs, loc
restore,'~/research/redux/scripts/s_tabletemplate.sav'
restore,'~/research/redux/scripts/s_resultstemplate.sav'

;## get values specific to model
IF (ktype EQ 'flat') THEN BEGIN
    IF (model EQ 'nonzero') THEN ind = 2 ELSE ind = 3
ENDIF
IF (ktype EQ 'itpl') THEN BEGIN
    IF (model EQ 'nonzero') THEN ind = 0 ELSE ind = 1
ENDIF

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
   push, obsid, obs
   push, zs, z[i]
   push, clusters, cluster[i]
   push, txs, tx[i]
ENDFOR
z = zs

;# sort by tx
s = sort(tx)
obs_or = obsid[s]
cluster = clusters[s]
tx_or = txs[s]
z_or = zs[s]

;# define the color array
color = maken(20,250,n_elements(obsid))

FOR i = 0,n_elements(obs_or)-1 DO BEGIN
    obs = strcompress(obs_or[i],/remove_all)
    file1 = '~/research/pf_clusters/pf_fits/tables/'+obs+'_table.dat'
    file2 = '~/research/pf_clusters/pf_fits/s_results/'+obs+'_results.log'
    check = findfile(file1,count=count)
    IF (count NE 1) THEN GOTO,ERROR
    check = findfile(file2,count=count)
    IF (count NE 1) THEN GOTO,ERROR
    obsdata = read_ascii(file1, template = s_tabletemplate)
    fitdata = read_ascii(file2, template = s_resultstemplate)
    push, red, z_or[i]

    IF k0val EQ 'obs' THEN BEGIN
        IF (ktype EQ 'flat') THEN k = obsdata.k_flat ELSE k = obsdata.k
        ;## remember, they're reversed
        num = n_elements(k)
        push, allk, k[num-1]
        kerr = obsdata.k_err
        push, allkerr, kerr[num-1]
    ENDIF ELSE BEGIN
        k = fitdata.k0
        push, allk, k[ind]
        kerr = fitdata.k0err
        push, allkerr, kerr[ind]
    ENDELSE
    push, allk100, fitdata.k100[ind]
    push, allk100err, fitdata.k100err[ind]
    push, legcolor, color[i]

ERROR:
;#IF (count NE 1) THEN BEGIN
;#    print, '## ERROR: ',file,' does not exist, moving on...'
;#ENDIF
ENDFOR

;# set up empty plot
xmin = 0.7*min(red)
xmax = 1.1*max(red)
ord = where(allk-allkerr GT 0.)
ymin = 0.9*min(allk[ord]-allkerr[ord])
ymax = 1.1*max(allk[ord]+allkerr[ord])
set_plot, 'PS'
loadct, 39, /silent
device, filename = 'k0z.eps', $
        /color, $
        /encapsulated, $
        /helvetica
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 3
!Y.THICK  = 3
!Z.THICK  = 3
plot, [xmin,xmax], [ymin,ymax], $
  /nodata, $
  xrange = [xmin,xmax], $
  yrange = [ymin,ymax], $
  xtitle = textoidl('Redshift'), $
  ytitle = textoidl('K_0 [keV cm^{2}]'), $
  /xsty, /ysty, $
  /ylog, /xlog, $
  POSITION = ASPECT(1.0), $
  charsize = 0.8
plotsym, 0, 0.8, /fill
;oploterror, red, allk, allkerr, psym=8, /nohat
oplot, red, allk, psym=8, color=0
plotsym, 0, 0.6, /fill
plots, red, allk, psym=8, color=legcolor
device,/close

;# set up empty plot
xmin = 0.7*min(red)
xmax = 1.1*max(red)
ck = (allk100-allk100err)[where(allk100-allk100err GT 0.)]
ymin = 0.9*min(ck)
ymax = 1.1*max(allk100+allk100err)
set_plot, 'PS'
loadct, 39, /silent
device, filename = 'k100z.eps', $
        /color, $
        /encapsulated, $
        /helvetica
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 3
!Y.THICK  = 3
!Z.THICK  = 3
plot, [xmin,xmax], [ymin,ymax], $
  /nodata, $
  xrange = [xmin,xmax], $
  yrange = [ymin,ymax], $
  xtitle = textoidl('Redshift'), $
  ytitle = textoidl('K_{100} [keV cm^{2}]'), $
  /xsty, /ysty, $
  /ylog, /xlog, $
  POSITION = ASPECT(1.0), $
  charsize = 0.8
plotsym, 0, 0.8, /fill
oploterror, red, allk100, allk100err, psym=8, /nohat
oplot, red, allk100, psym=8, color=0
plotsym, 0, 0.6, /fill
plots, red, allk100, psym=8, color=legcolor
device,/close
set_plot,'X'

END
