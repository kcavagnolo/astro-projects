PRO ha, input

;# options
dotc    = 'no'                  ;# shall we plot ha versus tcool too? only works for ktype=flat & nonzero
tcfile  = 'dat/t0_fits.dat'     ;# name of file with cooling profile fits
k0val   = 'fit'                 ;# type of data to use: 'obs' or 'fit'
space   = 'lum'                 ;# plot luminosity (lum) or flux?
ktype   = 'flat'                ;# type of fit to use, itpl or flat
model   = 'nonzero'             ;# type of K0 value, zero or nonzero
zfilter = 'no'                  ;# make a cut in redshift space
zmax    = 0.2                   ;# max redshift to cut at
zmin    = 0.0                   ;# min redshift to cut at
mini    = 'yes'                 ;# make the figure sqaure and small
plodd   = 'yes'                 ;# plot the blue boxes?
plokt   = 'yes'                 ;# plot the kthreshold line?
csize   = 1.0                   ;# size of characters
psize   = 0.8                   ;# size of plot symbols
pthick  = 1                     ;# thickness of arrows
cmMpc   = 3.08d24               ;# 1 Mpc = 3.08x10**24 cm

;# load all the templates needed to read data
restore,'~/research/redux/scripts/xspectemp_rin_normerr_src.sav'
restore,'~/research/redux/scripts/s_tabletemplate.sav'
restore,'~/research/redux/scripts/s_resultstemplate.sav'
readcol, '../pf_info/alldone.list', FORMAT='A,A,F,F,F,F,F,F,F,F,F,A,F,A,F,A', comment='#',$
         rclusters,robsids,rxs,rys,rrmaxs,rminctss,rzs,rnhs,rtxs,rfes,rlbols,rchips,reobss,rdiffs,rrobss,rlocs
readcol, input, FORMAT='A,A,D,D,F,F,A,F,A', comment='#', $
         cluster, obsids, ha, haerr, nii, niierr, notes, z, lines
IF ((dotc EQ 'yes') AND (ktype EQ 'flat') AND (model EQ 'nonzero')) THEN usetc = 'yes' ELSE usetc = 'no'
IF usetc EQ 'yes' THEN BEGIN
   check = findfile(tcfile,count=count)
   IF (count NE 1) THEN BEGIN
      print, '## ERROR: Missing ',tcfile
      exit
   ENDIF
   readcol, tcfile, FORMAT='A,A,A,F,F,I,F,F,F,F,F,F,F,F', comment='#', $
            tcclusters,tcobsids,tcmodes,tcrmins,tcrmaxs,tcanns,t0s,t0errs,$
            t100s,t100errs,tcas,tcaerrs,tcchisqs,tcprobs
ENDIF

;# open log file
OPENW, /get_lun, LOGLUN, 'pf_halum.dat'
printf, LOGLUN, format='(A-20,A20,A10,A10,4A12)', '#Name','Obsid','Type','z','HaFlux','err','HaLum','err'
printf, LOGLUN, format='(A-20,A20,A10,A10,4A12)', '#---','---','---','---','1d-15','1d-15','1d40','1d40'

;# get values specific to model
IF (ktype EQ 'flat') THEN BEGIN
   IF (model EQ 'nonzero') THEN ind = 2 ELSE ind = 3
ENDIF
IF (ktype EQ 'itpl') THEN BEGIN
   IF (model EQ 'nonzero') THEN ind = 0 ELSE ind = 1
ENDIF

;# begin looping
zf1 = 0
zf2 = 0
oldname = 'jgsdfkjsf'
total = 0
nha = 0
yha = 0
FOR i = 0,n_elements(obsids)-1 DO BEGIN
   k0zero = 'F'
   obsid = strcompress(obsids[i],/remove_all)
   name = strcompress(cluster[i],/remove_all)
   IF name EQ oldname THEN GOTO,ERROR
   ord = where(cluster EQ name)
   IF n_elements(ord) EQ 1 THEN obsid = obsid ELSE BEGIN
      temp = strcompress(obsids[ord],/remove_all)
      obsid = strjoin(temp,'_',/single)
   ENDELSE
   IF obsid LT 0. THEN print, name

   ;# get rid of 'unknowns'
   IF (lines[i] EQ 'uk') THEN GOTO,ERROR

   ;# check for file existance
   file1 = '~/research/pf_clusters/pf_fits/tables/'+obsid+'_table.dat'
   check = findfile(file1,count=count1)
   IF (count1 NE 1) THEN GOTO,ERROR
   file2 = '~/research/pf_clusters/pf_fits/s_results/'+obsid+'_results.log'
   check = findfile(file2,count=count2)
   IF (count2 NE 1) THEN GOTO,ERROR

   ;# redshift filter
   zf1++
   IF (zfilter EQ 'yes') THEN BEGIN
      IF ((z[i] LT zmin) OR (z[i] GT zmax)) THEN GOTO,ERROR
   ENDIF
   zf2++

   ;# read in data
   dataobs = read_ascii(file1, template = s_tabletemplate)
   datafit = read_ascii(file2, template = s_resultstemplate)

   ;# get central entropy value, either obs or fit
   IF k0val EQ 'obs' THEN BEGIN
      IF (ktype EQ 'flat') THEN k = dataobs.k_flat ELSE k = dataobs.k
      num = n_elements(k)       ;# remember, they're reversed
      k0 = k[num-1]
      k0err = dataobs.k_err[num-1]
   ENDIF ELSE IF k0val EQ 'fit' THEN BEGIN
      IF (ktype EQ 'itpl') THEN BEGIN
         chi1 = datafit.chisq[0]
         chi2 = datafit.chisq[1]
      ENDIF ELSE BEGIN
         chi1 = datafit.chisq[2]
         chi2 = datafit.chisq[3]
      ENDELSE
      k0 = datafit.k0[ind]
      k0err = datafit.k0err[ind]
      k100  = datafit.k100[ind]
      k100err = datafit.k100err[ind]
      alpha = datafit.plaw[ind]
      alphaerr = datafit.plawerr[ind]
   ENDIF ELSE BEGIN
      print, '## ERROR: something is wrong, I do not understand ',k0val,' as type of data to acquire.'
      exit
   ENDELSE
   
;   k12 = k0+k100*(12./100.)^(alpha)
;   k0 = k12
;   ord = where(rclusters EQ name,num)
;   IF num LE 0 THEN tx = 1.0
;   tx = rtxs[ord]
;   tx = tx[0]
;   k0 = k0/tx^0.6

   ;# filter for plaw k0
   IF chi2 LE chi1 THEN BEGIN
      print, ''
      print, '## WARNING: power-law better fit for:'
      print, format='(A-20,A20,F10.3,F10.3,F8.2,F8.2)',name, obsid, k0, k0err, chi1, chi2
   ENDIF
   IF ((k0 LE 0) OR (k0-k0err LE 0.))THEN BEGIN
      print, ''
      print, '## WARNING: K0 <= 0'
      print, format='(A-20,A20,4A10)','#Name','Obsid','z','K0','K0err','Source'
      print, format='(A-20,A20,F10.4,F10.3,F10.3,A10)',name,obsid,z[i],k0,k0err,lines[i]
      print, ''
      k0 = k0+2*k0err
      push, ax, k0
      k0err = -1d-6
      k0zero = 'T'
   ENDIF

   ;# calculate fluxes
   cosmology, z[i], result, /silent
   newdl = result[2]
   IF ((notes[i] EQ 'crawford') OR $
       (notes[i] EQ 'heckman') OR $
       (notes[i] EQ 'white') OR $
       (notes[i] EQ 'owen')) THEN BEGIN
      IF ((notes[i] EQ 'crawford') OR (notes[i] EQ 'white')) THEN BEGIN
         cosmology, z[i], result, hubcon=50, matdens=1.0, cosdens=0.0, /silent
         olddl = result[2]
      ENDIF ELSE IF (notes[i] EQ 'heckman') THEN BEGIN
         cosmology, z[i], result, hubcon=75, matdens=1.0, cosdens=0.0, /silent
         olddl = result[2]
      ENDIF
      IF (notes[i] EQ 'crawford') THEN BEGIN
         IF lines[i] EQ 'y' THEN BEGIN
            flux   = ha[i]/(4 * !PI * (olddl * cmMpc)^2)
            lumout = flux * 4 * !PI * (newdl * cmMpc)^2
            fluout = flux
            fluxerr= haerr[i]/(4 * !PI * (olddl * cmMpc)^2)
            lumerr = fluxerr * 4 * !PI * (newdl * cmMpc)^2
            fluerr = fluxerr
         ENDIF ELSE IF lines[i] EQ 'n' THEN BEGIN
            flux   = (1.99d42*z[i]^2.)/(4 * !PI * (olddl * cmMpc)^2)
            lumout = flux * 4 * !PI * (newdl * cmMpc)^2
            fluout = flux
            fluxerr= (haerr[i])/(4 * !PI * (olddl * cmMpc)^2)
            lumerr = fluxerr * 4 * !PI * (newdl * cmMpc)^2
            fluerr = fluxerr
         ENDIF
      ENDIF ELSE BEGIN
         flux   = ha[i]/(4 * !PI * (olddl * cmMpc)^2)
         lumout = flux * 4 * !PI * (newdl * cmMpc)^2
         fluout = flux
         fluxerr= haerr[i]/(4 * !PI * (olddl * cmMpc)^2)
         lumerr = fluxerr * 4 * !PI * (newdl * cmMpc)^2
         fluerr = fluxerr
      ENDELSE
   ENDIF ELSE IF (ha[i] GT 0.1) THEN BEGIN
      lumout = ha[i]
      fluout = ha[i]/(4 * !PI * (newdl * cmMpc)^2)
      lumerr = haerr[i]
      fluerr = haerr[i]/(4 * !PI * (newdl * cmMpc)^2)
   ENDIF ELSE IF (ha[i] LT 0.1) THEN BEGIN
      lumout = ha[i] * 4 * !PI * (newdl * cmMpc)^2
      fluout = ha[i]
      lumerr = haerr[i] * 4 * !PI * (newdl * cmMpc)^2
      fluerr = haerr[i]
   ENDIF ELSE BEGIN
      print, 'Something is wrong with ',ha[i]
      EXIT
   ENDELSE

   ;# either plot lum or flux
   IF space EQ 'lum' THEN BEGIN
      out = lumout/1d40
      outerr = lumerr/1d40
   ENDIF ELSE BEGIN
      out = fluout/1d-15
      outerr = fluerr/1d-15
   ENDELSE

if name EQ 'junk' then print, out

   ;# get tc values
   IF usetc EQ 'yes' THEN BEGIN
      ord = where(tcclusters EQ name,num)
      IF num LE 0. THEN BEGIN
         print, '## ERROR: Missing tcool for ',name
         t0 = 0.01
         t0err = 0.0
      ENDIF ELSE BEGIN
         t0 = t0s[ord]
         t0err = t0errs[ord]
      ENDELSE
      IF lines[i] EQ 'n' THEN BEGIN
         push, t0nf, t0
         push, t0nferr, t0err
         push, t0hanf, out
         push, t0hanferr, outerr
      ENDIF ELSE BEGIN
         push, t0f, t0
         push, t0ferr, t0err
         push, t0haf, out
         push, t0haferr, outerr
      ENDELSE
      SKIP:
   ENDIF

   ;# diff. between upper-limits and detections
   IF lines[i] EQ 'n' THEN BEGIN
      notice = 'NF'
      push, hall, out
      push, hallerr, outerr
      push, kll, k0
      push, kllerr, k0err
      push, zll, z[i]
   ENDIF ELSE BEGIN
      notice = 'F'
      push, haall, out
      push, haallerr, outerr
      push, kall, k0
      push, kallerr, k0err
      push, zall, z[i]
   ENDELSE

   ;# print out odd sources
   IF ((k0+k0err LE 30. AND lines[i] EQ 'n') OR (k0-k0err GE 30. AND lines[i] EQ 'y')) THEN BEGIN
      IF lines[i] EQ 'n' THEN BEGIN
         push, nfname, name
         push, nfobs, obsid
         push, nfz, z[i]
         push, nfk0, k0
         push, nflum, out
      ENDIF
      IF lines[i] EQ 'y' THEN BEGIN
         push, fname, name
         push, fobs, obsid
         push, fz, z[i]
         push, fk0, k0
         push, flum, out
      ENDIF
      IF k0zero NE 'T' THEN BEGIN
         push, gy, k0
         push, gx, out
      ENDIF
   ENDIF
   IF k0zero EQ 'T' THEN push, ay, out

   ;# build counters
   total++
   IF lines[i] EQ 'n' THEN nha++ ELSE yha++

   ;# print stuff to a log
   obsid = split('_',obsid)
   FOR j=0,n_elements(obsid)-1 DO BEGIN
      printf, LOGLUN, format='(A-20,A20,A10,F10.4,4F12.4)', name, obsid[j], notice, z[i], fluout/1d-15, fluerr/1d-15, lumout/1d40, lumerr/1d40
   ENDFOR

ERROR:
oldname = name
ENDFOR

;# filter info
IF zfilter EQ 'yes' THEN BEGIN
   print, format='(A-35,I10)','Number clusters before z-filter:',zf1
   print, format='(A-35,I10)','Number clusters after z-filter:',zf2
ENDIF

;# do some stats
print, format='(A-35,A4,I10)','Total number of clusters:','::',total
print, format='(A-35,A4,I10)','Number of clusters w/Ha:','::',yha
print, format='(A-35,A4,I10)','Number of clusters wo/Ha:','::',nha
IF n_elements(kall) GT 0 THEN BEGIN
   a = alog10(kall)
   kmean = 10.^(mean(a)+0.5*(stddev(a)^2.))
   kdev = sqrt(kmean^2.*10.^(stddev(a)^2.-1))
   print, format='(A-20,F10.2,A5,F10.2)','K0 w/ Ha:',kmean,'+/-',kdev
ENDIF
IF n_elements(kll) GT 0 THEN BEGIN
   a = alog10(kll)
   kmean = 10.^(mean(a)+0.5*(stddev(a)^2.))
   kdev = sqrt(kmean^2.*10.^(stddev(a)^2.-1))
   print, format='(A-20,F10.2,A5,F10.2)','K0 wo/ Ha:',kmean,'+/-',kdev
ENDIF
IF n_elements(zall) GT 0 THEN BEGIN
   kmean = mean(zall)
   kdev = stddev(zall)
   print, format='(A-20,F10.3,A5,F10.3)','z w/ Ha:',kmean,'+/-',kdev
   print, format='(A-20,F10.3,A5,F10.3)','Max z w/ Ha:',max(zall)
ENDIF
IF n_elements(zll) GT 0 THEN BEGIN
   kmean = mean(zll)
   kdev = stddev(zll)
   print, format='(A-20,F10.3,A5,F10.3)','z wo/ Ha:',kmean,'+/-',kdev
   print, format='(A-20,F10.3,A5,F10.3)','Max z wo/ Ha:',max(zll)
ENDIF

;# print oddities
IF n_elements(fname) GT 0 THEN BEGIN
   print, ''
   print, "## Halpha source found above K_thresh:"
   ord = sort(fname)
   fname = fname[ord]
   fobs = fobs[ord]
   fz = fz[ord]
   fk0 = fk0[ord]
   flum = flum[ord]
   FOR i=0,n_elements(fname)-1 DO $
      print, format='(A-20,A20,3F10.4)', fname[i], fobs[i], fz[i], fk0[i], flum[i]
ENDIF
IF n_elements(nfname) GT 0 THEN BEGIN
   print, ''
   ord = sort(nfname)
   nfname = nfname[ord]
   nfobs = nfobs[ord]
   nfz = nfz[ord]
   nfk0 = nfk0[ord]
   nflum = nflum[ord]
   print, "## No Halpha source found below K_thresh:"
   FOR i=0,n_elements(nfname)-1 DO $
      print, format='(A-20,A20,3F10.4)', nfname[i], nfobs[i], nfz[i], nfk0[i], nflum[i]
   print, ''
ENDIF

;# plot everything
loadct, 13
set_plot, 'PS'
device, $
   filename = 'ha_k0.eps', $
   /color, $
   /cmyk, $
   /encapsulated, $
   /portrait, $
   /helvetica, $
   bits=16
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 3
!Y.THICK  = 3
!Z.THICK  = 3

IF space EQ 'lum' THEN $
  ytex = textoidl('L_{H\alpha} [10^{40} ergs s^{-1}]') ELSE $
  ytex = textoidl('H\alpha Flux [10^{-15} ergs cm^{-2} s^{-1}]')
xtex = textoidl('K_{0} [keV cm^{2}]')
xmin = 0.5*min([kall,kll])
xmax = 1.3*max([kall,kll])
ymin = 0.5*min([haall,hall])
ymax = 1.25*max([haall+haallerr,hall])
plotsym, 0, psize, /fill
IF mini EQ 'yes' THEN BEGIN
   plot, kall, haall, $
         /nodata, $
         linestyle = 0, $
         xrange = [xmin,xmax], $
         yrange = [ymin,ymax],$
         xtitle = xtex, $
         ytitle = ytex, $
         /xsty, /ysty, $
         /xlog, /ylog, $
         YTickFormat='exponent', $
         XTickFormat='exponent', $
         charsize = csize, $
         POSITION = ASPECT(1.0), $
         psym = 8
ENDIF ELSE BEGIN
   plot, kall, haall, $
         /nodata, $
         linestyle = 0, $
         xrange = [xmin,xmax], $
         yrange = [ymin,ymax],$
         xtitle = xtex, $
         ytitle = ytex, $
         /xsty, /ysty, $
         /xlog, /ylog, $
         YTickFormat='exponent', $
         XTickFormat='exponent', $
         charsize = csize, $
         psym = 8
ENDELSE

;# upperlimit arrows
plotsym, 1, 2.0, thick=pthick
oplot, kll, hall, psym=8

;# unfound points
ord = where(kllerr GE 0.)
plotsym, 0, psize, /fill
oplot, kll[ord], hall[ord], psym=8
oploterror, kll[ord], hall[ord], kllerr[ord], hallerr[ord], psym=8, /nohat

;# blue boxes, red stars
IF plodd EQ 'yes' THEN BEGIN
   IF n_elements(gx) GT 1 THEN BEGIN
      ord = where(gy LE 30.)
      plotsym, 8, 1.2*psize, /fill
      oplot, gy, gx, psym=8, color=50
      plotsym, 3, 0.8*psize, /fill
      oplot, gy[ord], gx[ord], psym=8, color=250
   ENDIF
ENDIF

;# found points with errorbars
plotsym, 0, psize, /fill
oploterror, kall, haall, kallerr, haallerr, psym=8, /nohat
oplot, kall, haall, psym=8
plotsym, 0, 0.8*psize, /fill
oplot, kall, haall, psym=8, color=225

;# k0 upperlimit arrows
IF n_elements(ax) GT 0 THEN BEGIN
   plotsym, 6, 2.0, color=0, thick=pthick
   oplot, ax, ay, psym=8, thick=pthick
   plotsym, 4, 1.2*psize, /fill
   oplot, ax, ay, psym=8, color=0
   plotsym, 4, 0.8*psize, /fill
   oplot, ax, ay, psym=8, color=150
ENDIF

;# conductive line at k0 = 30
IF plokt EQ 'yes' THEN $
   oplot, [30.,30.], [1d-10,1d30], linestyle=2, psym=0, thick=pthick

;# add legend
IF zfilter EQ 'yes' THEN BEGIN
   IF zmin EQ 0. THEN lzmin='0.00' ELSE lzmin = num2str(zmin,2)
   IF zmax GT 10. THEN lzmax='\infty' ELSE lzmax = num2str(zmax,2)
   items = [textoidl(lzmin+'< z <'+lzmax)]
   linearr = replicate(-99,n_elements(items))
   psyarr = replicate(-99,n_elements(items))
   legend, items, linestyle=larr, psym=parr, box=0, charsize=0.8, /top, /right, /fill
ENDIF

device,/close

;# histograms
set_plot, 'PS'
device, filename='hahist.eps', $
        /color, $
        /encapsulated, $
        /portrait, $
        /helvetica
histoplot, [kall,kll], 0.15, $
           /log, $
           xtitle = textoidl('K_0 [keV cm^2]'), $
           ytitle = textoidl('Number of Clusters'), $
           charsize= csize
device, /close

;# plot tc if needed
IF usetc EQ 'yes' THEN BEGIN
   ytex = textoidl('L_{H\alpha} [10^{40} ergs s^{-1}]')
   xtex = textoidl('t_{0} [Gyrs]')
   x = [t0f,t0nf]
   y = [t0haf,t0hanf]
   xmin = 0.5*min(x)
   xmax = 1.3*max(x)
   ymin = 0.7*min(y)
   ymax = 1.25*max(y)
   plotsym, 0, psize, /fill
   set_plot, 'PS'
   device, $
      filename = 'ha_t0.eps', $
      /color, $
      /cmyk, $
      /encapsulated, $
      /portrait, $
      /helvetica
   !FANCY    = 4
   !LINETYPE = 0
   !P.FONT   = 0
   !X.THICK  = 3
   !Y.THICK  = 3
   !Z.THICK  = 3
   plot, x, y, $
         /nodata, $
         linestyle = 0, $
         xrange = [xmin,xmax], $
         yrange = [ymin,ymax],$
         xtitle = xtex, $
         ytitle = ytex, $
         /xsty, /ysty, $
         /xlog, /ylog, $
         YTickFormat='exponent', $
         XTickFormat='exponent', $
         charsize = csize, $
         POSITION = ASPECT(1.0), $
         psym = 8

   ;# upperlimit arrows
   plotsym, 1, 2.0, thick=pthick
   oplot, t0nf, t0hanf, psym=8

   ;# unfound points
   plotsym, 0, psize, /fill
   oplot, t0nf, t0hanf, psym=8
   oploterror, t0nf, t0hanf, t0nferr, t0hanferr, psym=8, /nohat

   ;# found points with errorbars
   plotsym, 0, psize, /fill
   oploterror, t0f, t0haf, t0ferr, t0haferr, psym=8, /nohat
   oplot, t0f, t0haf, psym=8
   plotsym, 0, 0.8*psize, /fill
   oplot, t0f, t0haf, psym=8, color=225
   device, /close
ENDIF

set_plot, 'X'

END
