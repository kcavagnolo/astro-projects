PRO k0rad, radfile

;#################################
;#################################
;# usage:                       ;#
;# IDL> k0rad, 'allradiolx.dat' ;#
                                ;#
!quiet  = 1                     ;#
myhome  = GETENV('HOME')        ;#
dotc    = 'no'                  ;# also plot tcool vs. lrad?
tcfile  = 'dat/t0_fits.dat'     ;# name of file with cooling profile fits
k0val   = 'fit'                 ;# type of data to use: 'obs' or 'fit'
space   = 'power'               ;# plot luminosity (lum) or flux or power?
ktype   = 'flat'                ;# type of K to use, 'flat' or 'itpl'
model   = 'nonzero'             ;# type of model to use, 'zero' or 'nonzero'
mini    = 'yes'                 ;# make a mini plot
zfilter = 'yes'                 ;# make a cut in redshift space
plokt   = 'no'                  ;# plot the k threshold line?
zmax    = 0.2                   ;# upper redshift to cut at
zmin    = 0.0                   ;# lower redshift to cut at
csize   = 1.0                   ;# size of characters
psize   = 0.8                   ;# size of plot points
pthick  = 1                     ;# thickness of upperlimit arrows
rbin    = 0.25                  ;# log bins for histogram
Jy      = 1d-23                 ;# 1Jy = 10**-23 ergs sec**-1 cm**-2 Hz**-1
cmMpc   = 3.08d24               ;# 1 Mpc = 3.08x10**24 cm
nu0nvss = 1.4d9                 ;# 1.4 GHz is the bandpass of the NVSS
nu0sumss= 0.843d9               ;# 843.9 MHz is the bandpass of SUMSS
                                ;#
;#################################
;#################################

set_plot, 'PS'
loadct, 13, /silent
device, filename='k0rad.eps', $
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

;# load all the templates needed to read data
restore,myhome+'/research/redux/scripts/xspectemp_rin_normerr_src.sav'
restore,myhome+'/research/redux/scripts/s_tabletemplate.sav'
restore,myhome+'/research/redux/scripts/s_resultstemplate.sav'
readcol, radfile, FORMAT='(A,A,A,F,F,F,F,F,A)', comment='#', $
         cluster, obsids, found, z, flux, ferr, lum, lumerr, org
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

;# get values specific to model
IF (ktype EQ 'flat') THEN BEGIN
    IF (model EQ 'nonzero') THEN ind = 2 ELSE ind = 3
ENDIF
IF (ktype EQ 'itpl') THEN BEGIN
    IF (model EQ 'nonzero') THEN ind = 0 ELSE ind = 1
ENDIF

;# make tex file
;; OPENW, /get_lun, texlun, 'radio.tex'
;; printf, texlun, '\begin{deluxetable}{lccc}'
;; printf, texlun, '\tabletypesize{\scriptsize}'
;; printf, texlun, '\tablecaption{Radio Luminosities for ACCEP Clusters.\label{tab:radiotab}}'
;; printf, texlun, '\tablewidth{0pt}'
;; printf, texlun, '\tablehead{'
;; printf, texlun, '\colhead{Name} & \colhead{$L_{Radio}$} & \colhead{$\sigma L_{Radio}$} & \colhead{Ref.}\\'
;; printf, texlun, '\colhead{} & \colhead{$10^{40}$ ergs s$^{-1}$} & \colhead{$10^{40}$ ergs s$^{-1}$} & \colhead{}\\'
;; printf, texlun, '\colhead{(1)} & \colhead{(2)} & \colhead{(3)} & \colhead{(4)}}'
;; printf, texlun, '\startdata'

;# begin looping
zf1 = 0
zf2 = 0
total = 0
yrad = 0
nrad = 0
oldname = 'jgsdfkjsf'
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

    ;# define filename and check for it
    IF found[i] EQ 'OA' THEN BEGIN
       count = 0
       GOTO,ERROR
    ENDIF

    ;# check for file existance
    file1 = myhome+'/research/pf_clusters/pf_fits/tables/'+obsid+'_table.dat'
    check = findfile(file1,count=count1)
    IF (count1 NE 1) THEN BEGIN
       IF oldname NE name THEN print, 'Missing ',file1,' for ',name
       GOTO,ERROR
    ENDIF
    file2 = myhome+'/research/pf_clusters/pf_fits/s_results/'+obsid+'_results.log'
    check = findfile(file2,count=count2)
    IF (count2 NE 1) THEN BEGIN
       IF oldname NE name THEN print, 'Missing ',file2,' for ',name
       GOTO,ERROR
    ENDIF

    ;# redshift filter
    zf1++
    IF (zfilter EQ 'yes') THEN BEGIN
       IF ((z[i] LT zmin) OR (z[i] GT zmax)) THEN GOTO,ERROR
    ENDIF
    zf2++

    ;# pretty up the names
    cname = strcompress(name,/remove_all)
    cname = str_replace(name,'_00',' ')
    cname = str_replace(name,'_0',' ')
    cname = str_replace(name,'_',' ')
    cname = str_replace(name,'ABELL','Abell')
    cname = str_replace(name,'CENTAURUS','Centaurus')
    cname = str_replace(name,'HERCULES','Hercules')
    cname = str_replace(name,'HYDRA','Hercules')
    cname = str_replace(name,'OPHIUCHUS','Ophiuchus')
    cname = str_replace(name,'SERSIC','Sersic')
    cname = str_replace(name,'ZWICKY','Zwicky')

    ;# read in data
    dataobs = read_ascii(file1, template = s_tabletemplate)
    datafit = read_ascii(file2, template = s_resultstemplate)

    ;# compute power
    cosmology,z[i],result,/silent
    dl = result[2]
    IF (org[i] EQ 'SUMSS') THEN nu0 = nu0sumss ELSE nu0 = nu0nvss
    power = (4 * !PI * (dl * cmMpc)^2 * flux[i] * Jy * nu0)/1d40
    powererr = (4 * !PI * (dl * cmMpc)^2 * ferr[i] * Jy * nu0)/1d40

    ;# get central entropy value, either obs or fit
    IF k0val EQ 'obs' THEN BEGIN
        IF (ktype EQ 'flat') THEN k = dataobs.k_flat ELSE k = dataobs.k
        num = n_elements(k)     ;# remember, they're reversed
        k0 = k[num-1]
        k0err = dataobs.k_err[num-1]
    ENDIF ELSE IF k0val EQ 'fit' THEN BEGIN
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

;k12 = k0+k100*(12./100.)^(alpha)
;k0 = k12

    ;# filter for negative k0
    IF ((k0 LE 0) OR (k0-k0err LE 0.))THEN BEGIN
       print, ''
       print, '## WARNING: K0 <= 0, cluster not plotted'
       print, format='(A-20,A20,4A10)','#Name','Obsid','z','K0','K0err','Source'
       print, format='(A-20,A20,F10.4,F10.3,F10.3,A10)',name,obsid,z[i],k0,k0err,found[i]
       print, ''
       k0 = k0+2*k0err
       push, ax, k0
       k0err = -1d-6
       k0zero = 'T'
    ENDIF

    ;# print out odd sources
    IF ((k0+k0err LE 30. AND found[i] EQ 'NF') OR $
        (k0-k0err GE 30. AND found[i] EQ 'F')) THEN BEGIN
       IF found[i] EQ 'NF' THEN BEGIN
          push, nfname, name
          push, nfobs, obsid
          push, nfz, z[i]
          push, nfk0, k0
          push, nflum, lum[i]
       ENDIF
       IF found[i] EQ 'F' THEN BEGIN
          push, fname, name
          push, fobs, obsid
          push, fz, z[i]
          push, fk0, k0
          push, flum, lum[i]
       ENDIF
       IF (k0zero NE 'T') THEN BEGIN
           push, gx, k0
           IF space EQ 'lum' THEN push, gy, lum[i]
           IF space EQ 'flux' THEN push, gy, flux[i]
           IF space EQ 'power' THEN push, gy, power
       ENDIF
    ENDIF

    ;# sort based on detection
    IF found[i] NE 'F' THEN BEGIN
       IF org[i] EQ 'SUMSS' THEN BEGIN
          push, sumxnf, k0
          push, sumynf, power
          push, sumxnferr, k0err
          push, sumynferr, powererr
       ENDIF
       push, k0nf, k0
       push, k0errnf, k0err
       push, rlumnf, lum[i]
       push, rlumerrnf, lumerr[i]
       push, flnf, flux[i]
       push, flnferr, ferr[i]
       push, pwnf, power
       push, pwnferr, powererr
    ENDIF ELSE BEGIN
       IF org[i] EQ 'SUMSS' THEN BEGIN
          push, sumx, k0
          push, sumy, power
          push, sumxerr, k0err
          push, sumyerr, powererr
       ENDIF
       push, k0f, k0
       push, k0errf, k0err
       push, rlumf, lum[i]
       push, rlumerrf, lumerr[i]
       push, flf, flux[i]
       push, flferr, ferr[i]
       push, pwf, power
       push, pwferr, powererr
    ENDELSE
    IF k0zero EQ 'T' THEN push, ay, power

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
      IF found[i] EQ 'NF' THEN BEGIN
         push, t0nf, t0
         push, t0nferr, t0err
         push, t0radnf, power
         push, t0radnferr, powererr
      ENDIF ELSE BEGIN
         push, t0f, t0
         push, t0ferr, t0err
         push, t0radf, power
         push, t0radferr, powererr
      ENDELSE
      SKIP:
   ENDIF

    ;# counters
    total++
    IF found[i] EQ 'F' THEN yrad++ ELSE nrad++

    ;# print to latex file
;;    IF found[i] NE 'F' THEN lls = '$<$' ELSE lls = ' '
;;    printf, texlun, format='(A-20,A3,A3,F8.2,A3,F8.2,A3,A5,A3)', $
;;            cname,' & ',lls,lum[i],' & ',lumerr[i],' & ',org[i],'\\'
ERROR:
    oldname = name
ENDFOR

;# print oddities
print, ''
print, "## Found radio source above K_thresh:"
ord = sort(fname)
fname = fname[ord]
fobs = fobs[ord]
fz = fz[ord]
fk0 = fk0[ord]
flum = flum[ord]
FOR i=0,n_elements(fname)-1 DO $
   print, format='(A-20,A20,3F10.4)', fname[i], fobs[i], fz[i], fk0[i], flum[i]
print, ''
ord = sort(nfname)
nfname = nfname[ord]
nfobs = nfobs[ord]
nfz = nfz[ord]
nfk0 = nfk0[ord]
nflum = nflum[ord]
print, "## No radio source found below K_thresh:"
FOR i=0,n_elements(nfname)-1 DO $
   print, format='(A-20,A20,3F10.4)', nfname[i], nfobs[i], nfz[i], nfk0[i], nflum[i]
print, ''

;# close-up tex file
;; printf, texlun, '\enddata'
;; printf, texlun, '\tablecomments{None '
;; printf, texlun, '}'
;; printf, texlun, '\end{deluxetable}'
;; free_lun, texlun

;# set plotting values
IF space EQ 'lum' THEN BEGIN
   yall = [rlumf,rlumnf]
   y = rlumf
   yn = rlumnf
   yerr = rlumerrf
   ynerr = rlumerrnf
   ytex = textoidl('L_{Radio} [10^{40} ergs s^{-1}]')
ENDIF ELSE IF space EQ 'flux' THEN BEGIN
   yall = [flf,flnf]
   y = flf
   yn = flnf
   yerr = flferr
   ynerr = flnferr
   ytex = textoidl('Flux [Jy]')
ENDIF ELSE IF space EQ 'power' THEN BEGIN
   yall = [pwf,pwnf]
   y = pwf
   yn = pwnf
   yerr = pwferr
   ynerr = pwnferr
   ytex = textoidl('\nuL_{\nu} [10^{40} ergs s^{-1}]')
ENDIF

xall = [k0f,k0nf]
x = k0f
xn = k0nf
xerr = k0errf
xnerr = k0errnf
xtex = textoidl('K_0 [keV cm^2]')
xmin = 0.5*min(xall)
xmax = 1.5*max(xall)
ymin = 0.5*min(yall)
ymax = 1.5*max(yall)

;# do some stats
IF zfilter EQ 'yes' THEN BEGIN
   print, format='(A-35,A4,I10)','Number clusters before z-filter','::',zf1
   print, format='(A-35,A4,I10)','Number clusters after z-filter','::',zf2
ENDIF
print, format='(A-35,A4,I10)','Total number of clusters:','::',total
print, format='(A-35,A4,I10)','Number of clusters w/radio:','::',yrad
print, format='(A-35,A4,I10)','Number of clusters wo/radio:','::',nrad
a = alog10(k0f)
kmean = 10.^(mean(a)+0.5*(stddev(a)^2.))
kdev = sqrt(kmean^2.*10.^(stddev(a)^2.-1))
print, format='(A-20,F10.2,A4,F8.2)','K0 w/ radio:',kmean,'+/-',kdev
a = alog10(k0nf)
kmean = 10.^(mean(a)+0.5*(stddev(a)^2.))
kdev = sqrt(kmean^2.*10.^(stddev(a)^2.-1))
print, format='(A-20,F10.2,A4,F8.2)','K0 wo/ radio:',kmean,'+/-',kdev
ord = where(k0f LE 30.)
a = alog10(k0f[ord])
kmean = 10.^(mean(a)+0.5*(stddev(a)^2.))
kdev = sqrt(kmean^2.*10.^(stddev(a)^2.-1))
print, format='(A-20,F10.2,A4,F8.2)','K0(<=30.) w/ radio:',kmean,'+/-',kdev

; found points
plotsym, 0, psize, /fill
IF mini EQ 'yes' THEN BEGIN
   plot, x, y, $
         /nodata, $
         xtitle = xtex, $
         ytitle = ytex, $
         xran = [xmin,xmax], $
         yran = [ymin,ymax], $
         /xsty, /ysty, $
         /YLOG, /XLOG, $
         YTickFormat='exponent', $
         XTickFormat='exponent', $
         psym = 8, $
         POSITION = ASPECT(1.0), $
         charsize = csize
ENDIF ELSE BEGIN
   plot, x, y, $
         /nodata, $
         xtitle = xtex, $
         ytitle = ytex, $
         xran = [xmin,xmax], $
         yran = [ymin,ymax], $
         /xsty, /ysty, $
         /YLOG, /XLOG, $
         YTickFormat='exponent', $
         XTickFormat='exponent', $
         psym = 8, $
         charsize = csize
ENDELSE

;# unfound points
a = where(xnerr GE 0.)
plotsym, 0, psize, /fill
oplot, xn[a], yn[a], color=0, psym=8
fudge = 0.*xnerr[a]
oploterror, xn[a], yn[a], xnerr[a], fudge, psym=8, errcolor=0, color=0, /nohat

;# sumss unfound
a = where(sumxnferr GE 0.)
plotsym, 8, psize, /fill
oplot, sumxnf[a], sumynf[a], color=0, psym=8
fudge = 0.*sumxnferr[a]
oploterror, sumxnf[a], sumynf[a], sumxnferr[a], fudge, psym=8, errcolor=0, color=0, /nohat

;# upperlimit arrows
plotsym, 1, 2.0, color=0, thick=pthick
oplot, xn, yn, psym=8, thick=pthick

;# blue boxes
IF n_elements(gx) GT 1 THEN BEGIN
   plotsym, 8, 1.2*psize, /fill
   oplot, gx, gy, psym=8, color=50
   ord = where(gx LT 30.)
   plotsym, 3, 0.8*psize, /fill
   oplot, gx[ord], gy[ord], psym=8, color=250
ENDIF

;# all found points
a = where(xerr GE 0.)
plotsym, 0, psize, /fill
oploterror, x[a], y[a], xerr[a], yerr[a], psym=8, errcolor=0, color=0, /nohat
oplot, x[a], y[a], color=0, psym=8
plotsym, 0, 0.8*psize, /fill
oplot, x[a], y[a], color=225, psym=8

;# sumss found
a = where(sumxerr GE 0.)
plotsym, 8, psize, /fill
oploterror, sumx[a], sumy[a], sumxerr[a], sumyerr[a], psym=8, errcolor=0, color=0, /nohat
oplot, sumx[a], sumy[a], color=0, psym=8
plotsym, 8, 0.8*psize, /fill
oplot, sumx[a], sumy[a], color=225, psym=8

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
   oplot, [30.,30.],[1d-10,1d30], linestyle=2, psym=0, thick=pthick

;# add legend
IF zfilter EQ 'yes' THEN BEGIN
   IF zmin EQ 0. THEN lzmin='' ELSE lzmin = num2str(zmin,2)+' <'
   IF zmax GT 10. THEN lzmax='\infty' ELSE lzmax = num2str(zmax,2)
   items = [textoidl(lzmin+' z <'+lzmax)]
   linearr = replicate(-99,n_elements(items))
   psyarr = replicate(-99,n_elements(items))
   legend, items, linestyle=linearr, psym=psyarr, box=0, charsize=0.8, /top, /right, /fill
ENDIF

;# close device
device,/close

;# histogram of rad dist.
set_plot, 'PS'
device, filename='radhist.eps', $
        /color, $
        /cmyk, $
        /encapsulated, $
        /portrait, $
;        /times
        /helvetica
IF space EQ 'lum' THEN ytex = textoidl('Log L_{Radio} [10^{40} ergs s^{-1}]')
IF space EQ 'flux' THEN ytex = textoidl('Log Flux_{Radio} [Jy]')
IF space EQ 'power' THEN ytex = textoidl('Log \nuL_{\nu} [10^{40} ergs s^{-1}]')
histoplot, alog10(x), rbin, $
           /drawlines, /colorfill, fillcolor = hcolor, $
           ytitle = ytex, $
           xtitle = textoidl('Number of Clusters'), $
           position= ASPECT(1.0), $
           charsize= csize
device, /close

;# plot tc if needed
IF usetc EQ 'yes' THEN BEGIN
   ytex = textoidl('\nuL_{\nu} [10^{40} ergs s^{-1}]')
   xtex = textoidl('t_{0} [Gyrs]')
   x = [t0f,t0nf]
   y = [t0radf,t0radnf]
   xmin = 0.5*min(x)
   xmax = 1.3*max(x)
   ymin = 0.7*min(y)
   ymax = 1.25*max(y)
   plotsym, 0, psize, /fill
   loadct, 13
   set_plot, 'PS'
   device, $
      filename = 't0rad.eps', $
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
   oplot, t0nf, t0radnf, psym=8

   ;# unfound points
   plotsym, 0, psize, /fill
   oplot, t0nf, t0radnf, psym=8
   oploterror, t0nf, t0radnf, t0nferr, t0radnferr, psym=8, /nohat

   ;# found points with errorbars
   plotsym, 0, psize, /fill
   oploterror, t0f, t0radf, t0ferr, t0radferr, psym=8, /nohat
   oplot, t0f, t0radf, psym=8
   plotsym, 0, 0.8*psize, /fill
   oplot, t0f, t0radf, psym=8, color=225
   device, /close
ENDIF

!quiet  = 0
END
