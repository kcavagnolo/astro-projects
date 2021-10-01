pro allsbr, reffile

!quiet  = 1
myhome  = GETENV('HOME')
fakprof = myhome+'/research/pf_clusters/pf_fits/fakmarx/z1_tx1prof.fits'
redir   = 'reprocessed'
bin     = '5pix'
plnames = 'no'
smooth  = 'no'
virial  = 'no'
norm    = 'yes'
arcnorm = 'yes'
amin = 9.99
amax = 10.00

IF virial EQ 'yes' THEN BEGIN
   IF norm EQ 'no' THEN BEGIN
      xran = [0.001,1]
      yran = [1d-8,1d-2]
   ENDIF ELSE BEGIN
      xran = [0.001,1]
      yran = [1d-2,10.0]
   ENDELSE
   nmin = 0.0199
   nmax = 0.0201
   !xtitle = textoidl('R/R_{virial}')
   !ytitle = textoidl('Surface Brightness [cts sec^{-1} arcsec^{-2}]')
ENDIF ELSE BEGIN
   IF norm EQ 'no' THEN BEGIN
      xran = [1,100]
      yran = [1d-8,1d-2]
   ENDIF ELSE BEGIN
      xran = [1,100]
      yran = [0.1,10.0]
   ENDELSE
   nbin = 1
   !xtitle = textoidl('R_{mid} [arcsec]')
   !ytitle = textoidl('Normalized Surface Brightness')
ENDELSE

;# restore data strucs
restore,myhome+'/research/redux/scripts/tcooltemplate.sav'
readcol, reffile, FORMAT='A,A,F,F,F,F,F,F,F,F,F,A,F,A,F,A', comment='#', $
         names, obsids, xs, ys, rmaxs, minctss, zs, nhs,$
         txs, fes, lbols, chips, eobss, diffs, robss, locs

;# read fak data
fakfits   = mrdfits(fakprof,1,/silent)
fakrmid   = fakfits.rmid*0.492
faksurbri = fakfits.sur_bri
;faksurbri = smooth(faksurbri,4)
faksurbri = faksurbri/4.9981
rint      = maken(min(fakrmid),max(fakrmid),1000000)
sbrint    = interpol(faksurbri,fakrmid,rint)
;ord       = where((rint GT 2.455) AND (rint LT 2.465))
ord       = where((rint GT 1.225) AND (rint LT 1.235))
ord       = ord[0]
fak0      = sbrint[ord]

;# open a log
GET_LUN, LOGLUN
OPENW, LOGLUN, 'err_plotsb.log'

;# plot the results
loadct, 13, /silent
set_plot, 'PS'
device, filename = 'allsbr.eps', $
        /color, $
        /encapsulated, $
        /portrait, $
        /helvetica
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 3
!Y.THICK  = 3
!Z.THICK  = 3
plot, xran, yran, $
      /nodata, $
      xran = xran, $
      yran = yran, $
      /xlog, /ylog, $
      /xsty, /ysty, $
      charsize = 0.8

FOR i = 0,n_elements(obsids)-1 DO BEGIN
   multi = 'no'
   obsid = strcompress(obsids[i],/remove_all)
   name = strcompress(names[i],/remove_all)
   z = zs[i]
   tx = txs[i]
   datadir = strcompress(locs[i],/remove_all)
   ord = where(names EQ name)
   IF n_elements(ord) EQ 1 THEN obsid = obsid ELSE BEGIN
      temp = obsids[ord]
      FOR j=1,n_elements(temp)-1 DO BEGIN
         obsid = obsid+'_'+strcompress(temp[j],/remove_all)
      ENDFOR
      multi = 'yes'
   ENDELSE
   IF multi EQ 'no' THEN $
      sbr = datadir+'/'+obsid+'/'+redir+'/'+obsid+'_sbprof_'+bin+'.fits' $
   ELSE $
      sbr = datadir+'/merged/'+name+'/'+obsid+'_sbprof_'+bin+'.fits'

   ;# check for file existance
   check = findfile(sbr,count=count)
   IF (count NE 1) THEN GOTO, err

   ;# set colors for diff redshift bins
   IF z LE 0.05 THEN color = 50
   IF ((z LE 0.3) AND (z GT 0.05)) THEN color = 150
   IF z GT 0.3 THEN color = 250

   ;# sbr data
   fits   = mrdfits(sbr,1,/silent)
   rmid   = fits.rmid*0.492
   exp    = fits.exposure
   surbri = fits.sur_bri/exp/(0.492^2.)
   IF smooth EQ 'yes' THEN surbri = smooth(surbri,2)
   sbrerr = fits.sur_bri_err/exp/(0.492^2.)
   IF virial EQ 'yes' THEN BEGIN
      cosmology, z, result, /silent
      as2kpc = result[4]
      rv     = rdelta(180,z,tx,/silent)*1000.
      rmid   = fits.rmid*0.492*as2kpc
      rmid   = rmid/rv
   ENDIF

   ;# normalize sbr
   IF norm EQ 'yes' THEN BEGIN
      IF virial EQ 'yes' THEN BEGIN
         rint   = maken(min(rmid),max(rmid),100000)
         sbrint = interpol(surbri,rmid,rint)
         ord    = where((rint GT nmin) AND (rint LT nmax))
         ord    = ord[0]
         surbri = surbri/sbrint[ord]
      ENDIF ELSE IF arcnorm EQ 'yes' THEN BEGIN
         rint   = maken(min(rmid),max(rmid),100000)
         sbrint = interpol(surbri,rmid,rint)
         ord    = where((rint GT amin) AND (rint LT amax))
         ord    = ord[0]
         surbri = surbri/sbrint[ord]
      ENDIF ELSE BEGIN
         surbri = surbri/surbri[nbin]
      ENDELSE
   ENDIF

   ;# track sbr above a certain level
   coco = 'n'
   IF max(surbri) GT fak0 THEN BEGIN
      ord = where(surbri EQ max(surbri))
      IF plnames EQ 'yes' THEN xyouts, rmid[ord], surbri[ord], name, charsize=0.3, alignment=0.45
      print, format='(A-20,A20,F10.4)',name,obsid,z
      coco = 'y'
   ENDIF

   ;# overplot the data
   IF coco EQ 'n' THEN BEGIN
      oplot, rmid, surbri, psym=0
      plotsym, 0, 0.25, /fill
      oplot, rmid, surbri, psym=8
   ENDIF ELSE BEGIN
      oplot, rmid, surbri, psym=0, color=150, linestyle=2
      plotsym, 0, 0.5, /fill
      oplot, rmid, surbri, psym=8, color=150
   ENDELSE

err:
   IF count NE 1 THEN printf, LOGLUN, 'No ',sbr
ENDFOR

;# overplot the idealized power-law from MARX
IF virial NE 'yes' THEN BEGIN
   oplot, fakrmid, faksurbri, psym=0, linestyle=0, thick=4, color=250
ENDIF

;# redraw axes to make clean lines
plot, xran, yran, $
      /noerase, $
      /nodata, $
      xran = xran, $
      yran = yran, $
      /xlog, /ylog, $
      /xsty, /ysty, $
      charsize = 0.8

;# shut it all down
close, LOGLUN
device, /close
set_plot, 'X'
!quiet = 0

END
