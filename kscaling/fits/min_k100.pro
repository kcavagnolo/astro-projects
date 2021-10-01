PRO min_k100, dat1

!QUIET = 1

;#################################
;#################################
;# usage:                       ;#
;# IDL> min_tscal, 'done.list'  ;#
                                ;#
tfid     = 5.0                  ;# pivot point in Tx plane
nbins    = 5                    ;# number of bins desired for k100-tx plot
ftxerr   = 0.025                ;# fiducial err on Tx
myhome   = GETENV('HOME')       ;# environment variable for HOME
ktype    = 'flat'               ;# type of K to use: 'flat' or 'itpl'
model    = 'nonzero'            ;# type of model to use: 'zero' or 'nonzero'
zfilter  = 'no'                 ;# make a cut in redshift space
zmin     = 0.40                 ;# redshift to cut at
zmax     = 10.40                ;# redshift to cut at
txfilter = 'yes'                ;# make a cut in tx space
txmin    = 3.0                  ;# tx to cut at
txmax    = 200.0                ;# tx to cut at
k0filter = 'no'                 ;# make a cut in k0 space
k0min    = 30.0                 ;# k0 to cut at
k0max    = 3000000.0            ;# k0 to cut at
adfilter = 'no'                 ;# should the K0 values be filtered for histogram plotting?
filter0  = 150.                 ;# first filter: for K100 > filter0, err must be...
filter1  = 20.                  ;# second filter: ...less than filter1% else...
filter2  = 20.                  ;# third filter: ...for all others, K100 err must be < filter2%                     
csize    = 1.0                  ;# size of characters in plots
psize    = 0.5                  ;# psym size
;; txfile   = 'tx.dat'
                                ;#
;#################################
;#################################

;# setup the fancy plotting options
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 3
!Y.THICK  = 3
!Z.THICK  = 3

;# conv to log space err
lnerr = 1.0/alog(10.)

;# load all the templates needed to read data
restore,myhome+'/research/redux/scripts/s_resultstemplate.sav'

;# read in list of clusters to work with
readcol, dat1, FORMAT='A,A,F,F,F,F,F,F,F,F,F,A,F,A,F,A', comment='#', $
         cluster,obsids,x,y,rmax,mincts,zs,nhs,txs,fes,lbols,chips,eobss,diffs,robs,loc
;; readcol, txfile, FORMAT='(A,A,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F)', comment='#', $
;;          tname,tobs,trin,trout,tnh,tnhlo,tnhhi,ttx,ttxlo,ttxhi,tfe,tfelo,tfehi,tn,tnlo,tnhi,ttx2,ttx2lo,ttx2hi,tn2,tn2lo,tn2hi,tz,tzlo,tzhi,tcr,tsrc,tchisq,tdof

;# get values specific to model
IF (ktype EQ 'flat') THEN BEGIN
   IF (model EQ 'nonzero') THEN ind = 2 ELSE ind = 3
ENDIF
IF (ktype EQ 'itpl') THEN BEGIN
   IF (model EQ 'nonzero') THEN ind = 0 ELSE ind = 1
ENDIF

;# open a log file for BCES
OPENW, K100BCES, "bces_k100.dat", /GET_LUN
OPENW, ALPBCES, "bces_alpha.dat", /GET_LUN

;# begin loop through each cluster
zf1 = 0
zf2 = 0
FOR i = 0,n_elements(obsids)-1 DO BEGIN
   obsid = strcompress(obsids[i],/remove_all)
   name = strcompress(cluster[i],/remove_all)
   ord = where(cluster EQ name)
   multi  = 'no'
   IF n_elements(ord) EQ 1 THEN obsid = obsid ELSE BEGIN
      multi = 'yes'
      temp = obsids[ord]
      FOR j=1,n_elements(temp)-1 DO BEGIN
         obsid = obsid+'_'+strcompress(temp[j],/remove_all)
      ENDFOR
   ENDELSE

   ;# check for file existance
   datfile = myhome+'/research/pf_clusters/pf_fits/s_results/'+obsid+'_results.log'
   check = findfile(datfile,count=count)
   IF (count NE 1) THEN GOTO,ERROR

;;    ;# query txfile
;;    ord = where(tname EQ name, num)
;;    IF num LE 0 THEN BEGIN
;;       print, '## ERROR: Missing ',name,' in ',txfile
;;       GOTO,ERROR
;;    ENDIF
;;    ord = ord[0]
;;    tx = ttx[ord]
;;    tlo = tx-ttxlo[ord]
;;    thi = ttxhi[ord]-tx
;;    IF tlo GT thi THEN terr = tlo ELSE terr = thi
;;    IF tx LE 0 THEN BEGIN
;;       print, '## ERROR: No TX for ',name,' in ',txfile
;;       GOTO,ERROR
;;    ENDIF

   ;# read in data
   datafit  = read_ascii(datfile, template = s_resultstemplate)

   ;# get central entropy value, either obs or fit
   IF (ktype EQ 'itpl') THEN BEGIN
      chi1 = datafit.chisq[0]
      chi2 = datafit.chisq[1]
   ENDIF ELSE BEGIN
      chi1 = datafit.chisq[2]
      chi2 = datafit.chisq[3]
   ENDELSE

   ;# define value arrays
   tx = txs[i]
   z = zs[i]
   k0 = datafit.k0[ind]
   k0err = datafit.k0err[ind]
   k100 = datafit.k100[ind]
   k100err = datafit.k100err[ind]
   alpha = datafit.plaw[ind]
   alphaerr = datafit.plawerr[ind]

   ;# filters
   zf1++
   IF (zfilter EQ 'yes') THEN BEGIN
      IF ((z LT zmin) OR (z GT zmax)) THEN GOTO,ERROR
   ENDIF
   IF (txfilter EQ 'yes') THEN BEGIN
      IF ((tx LT txmin) OR (tx GT txmax)) THEN GOTO,ERROR
   ENDIF
   IF (k0filter EQ 'yes') THEN BEGIN
      IF ((k0 LT k0min) OR (k0 GT k0max)) THEN GOTO,ERROR
   ENDIF
   IF (adfilter EQ 'yes') THEN BEGIN
      IF ((k100 GT filter0) AND (100.*(k100err/k100) GT filter1)) THEN GOTO,ERROR
      IF ((k100 LE filter0) AND (100.*(k100err/k100) GT filter2)) THEN GOTO,ERROR
   ENDIF
   zf2++

;;    ;# throw warnings
;;    IF ((k100 LE 30) OR (k100 GT 1000)) THEN BEGIN
;;       print,''
;;       print, '## WARNING: K100 > 1000 or <= 10 for:'
;;       print, format='(A-20,A20,8A10)','name','obsid','k0','k0err','k100','k100err','alpha','alphaerr','chi1','chi2'
;;       print, format='(A-20,A20,4F10.3,4F10.4)',name, obsid, k0, k0err, k100, k100err, alpha, alphaerr, chi1, chi2
;;    ENDIF
;;    IF (alpha GT 2.0) THEN BEGIN
;;       print,''
;;       print, '## WARNING: alpha greater than 2 for:'
;;       print, format='(A-20,A20,8A10)','name','obsid','k0','k0err','k100','k100err','alpha','alphaerr','chi1','chi2'
;;       print, format='(A-20,A20,4F10.3,4F10.4)',name, obsid, k0, k0err, k100, k100err, alpha, alphaerr, chi1, chi2
;;    ENDIF

   ;# build final arrays
   push, zall, z
   push, tall, tx/tfid
;;    push, terrall, terr
   push, k0all, k0
   push, k100all, k100
   push, k100errall, k100err
   push, kr, k0+k100
   push, aall, alpha
   push, aerrall, alphaerr
;;    printf, K100BCES, FORMAT='(5F10.4)', alog10(tx), lnerr*(terr/tx), alog10(k100), lnerr*(k100err/k100), 0.0
;;    printf, ALPBCES, FORMAT='(5F10.4)', tx, terr, alpha, alphaerr, 0.0
   printf, K100BCES, FORMAT='(5F10.4)', alog10(tx), ftxerr*alog10(tx), alog10(k100), lnerr*(k100err/k100), 0.0
   printf, ALPBCES, FORMAT='(5F10.4)', tx, ftxerr*tx, alpha, alphaerr, 0.0

ERROR:
ENDFOR

;# close log
FREE_LUN, K100BCES
FREE_LUN, ALPBCES

;# print out some information
print, ''
print, format='(A-35,I10)','Number clusters before filter(s):',zf1
print, format='(A-35,I10)','Number clusters after filter(s):',zf2
print, ''

;# perform the scaling calculation
gamma = -2.0
gammax = 2.0
gamstep = 0.001
WHILE gamma LE gammax DO BEGIN
   knew  = alog10(k100all*(tall^gamma))
   kmean = 10.^(mean(knew))+(0.5*(stddev(knew)^2.))
   kdev = sqrt(kmean^2.*10.^((stddev(knew))^2.-1))
   push, gamall, gamma
   push, kdevall, kdev/kmean
   gamma = gamma+gamstep
ENDWHILE

;# bin on tx
x = alog10(tall)
y = alog10(k100all)
xerr = x*ftxerr
;; xerr = lnerr*(terrall/tall)
yerr = lnerr*(k100errall/k100all)
ord = sort(x)
x = x[ord]
y = y[ord]
xerr = xerr[ord]
yerr = yerr[ord]
count = 0
total = 0
goal = round(n_elements(x)/nbins)
WHILE (total NE n_elements(x)) DO BEGIN
   IF (count LT goal) THEN BEGIN
      push, xtemp, x[total]
      push, ytemp, y[total]
      push, xweight, 1.0/(xerr[total])^2.
      push, yweight, 1.0/(yerr[total])^2.
      count++
   ENDIF
   IF ((count EQ goal) OR (total EQ n_elements(x)-1)) THEN BEGIN
      IF (total EQ n_elements(x)-1) THEN BEGIN
         print, FORMAT='(A-35,I5)','## Points in last bin:',count
         print, FORMAT='(A-35,I5)','## Goal was:',goal
         IF count/goal LT 0.5 THEN BEGIN
            print, FORMAT='(A-35)','## Percent full is < 50%, excluding bin'
            GOTO,SKIP
         ENDIF
      ENDIF
      push, ywav, (total(yweight*ytemp))/total(yweight)
      push, ysigwav, (1./sqrt(total(yweight)))
      push, xwav, mean(xtemp)
      push, xwavlo, mean(xtemp)-min(xtemp)
      push, xwavhi, max(xtemp)-mean(xtemp)
      SKIP:
      void, xtemp
      void, ytemp
      void, xweight
      void, yweight
      count = 0
   ENDIF
   total++
ENDWHILE

;# fit the log(k100)-log(tx) relation
x = alog10(tall)
y = alog10(k100all)
xerr = x*ftxerr
;; xerr = lnerr*(terrall/tall)
yerr = lnerr*(k100errall/k100all)
p = mpfit('LINFITEX', [1d, 1d], $
          FUNCTARGS={X: x, Y: y, SIGMA_X: xerr, SIGMA_Y: yerr}, $
          perror=dp, bestnorm=chi2, /quiet)
print, ''
print, FORMAT='(A-20)', 'LINFITEX Tx-K100:'
print, FORMAT='(A-20,F10.3,A6,F10.3)','Slope: ', p[1], ' +/- ', dp[1]
print, FORMAT='(A-20,F10.3,A6,F10.3)','Y-intercept: ', p[0], ' +/- ', dp[0]
print, FORMAT='(A-20,F15.3)','Bestnorm: ', chi2
px = maken(-100,100,1000)
py = p[1]*px+p[0]
ssy = (1.0-(1.2/2))*px+p[0]
bp = [0.148E+00, 0.782E-01, 0.223E+01, 0.615E-01]
bpy = bp[0]*px+bp[2]

;# fit the binned log(k100)-log(tx) relation
x = xwav
y = ywav
xerr = xwav*ftxerr
yerr = ysigwav
p = mpfit('LINFITEX', [1d, 1d], $
          FUNCTARGS={X: x, Y: y, SIGMA_X: xerr, SIGMA_Y: yerr}, $
          perror=dp, bestnorm=chi2, /quiet)
print, ''
print, FORMAT='(A-20)', 'LINFITEX (bin) Tx-K100:'
print, FORMAT='(A-20,F10.3,A6,F10.3)','Slope: ', p[1], ' +/- ', dp[1]
print, FORMAT='(A-20,F10.3,A6,F10.3)','Y-intercept: ', p[0], ' +/- ', dp[0]
print, FORMAT='(A-20,F15.3)','Bestnorm: ', chi2
wpx = maken(-100,100,1000)
wpy = p[1]*px+p[0]

;# fit the alpha-tx relation
x = tall
y = aall
xerr = x*ftxerr
;; xerr = terrall
yerr = aerrall
p = mpfit('LINFITEX', [1d, 1d], $
          FUNCTARGS={X: x, Y: y, SIGMA_X: xerr, SIGMA_Y: yerr}, $
          perror=dp, bestnorm=chi2, /quiet)
print, ''
print, FORMAT='(A-20)', 'LINFITEX Tx-alpha:'
print, FORMAT='(A-20,F10.3,A6,F10.3)','Slope: ', p[1], ' +/- ', dp[1]
print, FORMAT='(A-20,F10.3,A6,F10.3)','Y-intercept: ', p[0], ' +/- ', dp[0]
print, FORMAT='(A-20,F15.3)','Bestnorm: ', chi2
apx = maken(-100,100,1000)
apy = p[1]*px+p[0]

;# make plots of data
loadct, 13, /silent
set_plot, 'PS'
x = tall
;; xerr = terrall
xerr = x*ftxerr
y = aall
yerr = aerrall
xtex = textoidl('T_{cluster} [keV]')
ytex = textoidl('Power law index, \alpha')
IF min(x-xerr) LE 0 THEN factor=1.2 ELSE factor=0.8
xmin = factor*min(x-xerr)
xmax = 1.2*max(x+xerr)
IF min(y-yerr) LE 0 THEN factor=1.2 ELSE factor=0.8
ymin = factor*min(y-yerr)
ymax = 1.2*max(y+yerr)
device, filename='alphatx.eps', $
        /color, $
        /encapsulated, $
        /portrait, $
        set_font='Times-Roman'
plot, x, y, $
      /nodata, $
      linestyle = 0, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax],$
      xtitle = xtex, $
      ytitle = ytex, $
      /xsty, /ysty, $
      position= ASPECT(1.0), $
      charsize = csize
plotsym, 0, psize, /fill
oploterror, x, y, xerr, yerr, psym=8, /nohat
oplot, x, y, color=0, psym=8
plotsym, 0, 0.8*psize, /fill
oplot, x, y, color=225, psym=8
oplot, apx, apy, linestyle=2, thick=2, psym=0
device, /close

;# k100 vs. tcluster
set_plot, 'PS'
x = alog10(tall)
;; xerr = lnerr*(terrall/tall)
xerr = x*ftxerr
y = alog10(k100all)
yerr = lnerr*(k100errall/k100all)
xtex = textoidl('log (T_{cluster}/T_{5} [keV])')
ytex = textoidl('log (K_{100} [keV cm^2])')
IF min(x-xerr) LE 0 THEN factor=1.2 ELSE factor=0.8
xmin = factor*min(x-xerr)
xmax = 1.05*max(x+xerr)
IF min(y-yerr) LE 0 THEN factor=1.2 ELSE factor=0.8
ymin = factor*min(y-yerr)
ymax = 1.05*max(y+yerr)
device, filename='k100tx.eps', $
        /color, $
        /encapsulated, $
        /portrait, $
        set_font='Times-Roman'
plot, x, y, $
      /nodata, $
      linestyle = 0, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax],$
      xtitle = xtex, $
      ytitle = ytex, $
      /xsty, /ysty, $
      position= ASPECT(1.0), $
      charsize = csize
plotsym, 0, psize, /fill
oploterror, x, y, xerr, yerr, psym=8, /nohat
oplot, x, y, color=0, psym=8
plotsym, 0, 0.8*psize, /fill
oplot, x, y, color=225, psym=8
oplot, px, py, linestyle=2, thick=2, psym=0  ;fitexy
oplot, px, bpy, linestyle=3, thick=2, psym=0  ;bces
oplot, px, ssy, linestyle=1, thick=2, psym=0 ;self-similar
device, /close

;# plot fe0 vs. l14
set_plot, 'PS'
x = xwav
y = ywav
xlo = xwavlo
xhi = xwavhi
yerr = ysigwav
xtex = textoidl('log (T_{cluster}/T_{5} [keV])')
ytex = textoidl('log (K_{100} [keV cm^2])')
xmin = 1.05*min(x-xlo)
xmax = 1.05*max(x+xhi)
ymin = 0.95*min(y-yerr)
ymax = 1.05*max(y+yerr)
device, filename='bink100tx.eps', $
        /color, $
        /encapsulated, $
        /portrait, $
        set_font='Times-Roman'
plot, x, y, $
      /nodata, $
      linestyle = 0, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax],$
      xtitle = xtex, $
      ytitle = ytex, $
      /xsty, /ysty, $
      position= ASPECT(1.0), $
      charsize = csize
plotsym, 0, psize, /fill
oploterror, x, y, xlo, yerr, psym=8, /lobar
oploterror, x, y, xhi, yerr, psym=8, /hibar
oplot, x, y, color=0, psym=8
plotsym, 0, 0.8*psize, /fill
oplot, x, y, color=225, psym=8
oplot, wpx, wpy, linestyle=2, thick=2, psym=0  ;fitexy
oplot, px, ssy, linestyle=1, thick=2, psym=0 ;self-similar
device, /close

;# plot min gamma
set_plot, 'PS'
device, filename='mintscal.eps', $
        /color, $
        /encapsulated, $
        /portrait, $
        set_font='Times-Roman'
xmin = min(gamall)
xmax = max(gamall)
ymin = 0.98*min(kdevall)
ymax = 1.02*max(kdevall)
plot,gamall,kdevall, $
     /xsty, /ysty, $
     xtitle = textoidl('\gamma'), $
     ytitle = textoidl('\sigma_{K^*_{\gamma}}/K^*_{\gamma}'), $
     xrange = [xmin,xmax], $
     yrange = [ymin,ymax], $
     charsize= csize
get = where(kdevall EQ min(kdevall))
n = n_elements(get)
n = indgen(n)+1
n = median(n)
IF (n EQ 2 OR n EQ 1) THEN n = 0
get = get[n]
print, format='(A-30,F10.3)','Minimum gamma:',gamall[get]
print, format='(A-30,F10.3)','Minimum K100 dev:',kdevall[get]
items = [textoidl('Min \sigma_{K^*_{\gamma}}/K^*_{\gamma}: '+num2str(kdevall[get],3)), $
         textoidl('Min \gamma: '+num2str(gamall[get],3))]
linearr = replicate(-99,n_elements(items))
psyarr = replicate(-99,n_elements(items))
legend, items, linestyle=linearr,psym=psyarr,/top,box=0,/right_legend,charsize=csize
plotsym, 3, /fill
oplot, [gamall[get],-100], [kdevall[get],-100], psym=8
oplot, [gamall[get],gamall[get]], [-100,kdevall[get]], linestyle=2
oplot, [-100,gamall[get]], [kdevall[get],kdevall[get]], linestyle=2
device,/close
set_plot,'X'

END
