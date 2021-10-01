PRO betamods, reffile

;# general options
myhome = GETENV('HOME')
redir  = 'reprocessed'
bin    = '5pix'
numpp  = '1'

;# read-in reference file
readcol, reffile, FORMAT='A,A,F,F,F,F,F,F,F,F,F,A,F,A,F,A', comment='#', $
         names, obsids, xs, ys, rmaxs, minctss, zs, nhs,$
         txs, fes, lbols, chips, eobss, diffs, robss, locs

;# open logs and start LaTeX file
GET_LUN, LOGLUN
OPENW, LOGLUN, 'err_betamods.log'
GET_LUN, LISTLUN
OPENW, LISTLUN, 'list'
GET_LUN, OUTLUN
OPENW, OUTLUN, 'temp'
printf, OUTLUN, format='(A-20,A20,A8,A12,A12,A12,A12,A12,A12,A12,A12,A12,A12,A12,A12,A12,A10,A5,A12)', $
        '#Name','Obsid','z','SB0','S01','+/-','rc1','+/-','beta1','+/-','S02','+/-','rc2','+/-','beta2','+/-','chisq','dof','prob'
GET_LUN, TEXLUN
OPENW, TEXLUN, 'betamods.tex'
printf, TEXLUN, '\documentclass{article}'
printf, TEXLUN, '\usepackage{longtable,lscape}'
printf, TEXLUN, '\begin{document}'
printf, TEXLUN, '\begin{landscape}'
printf, TEXLUN, '\scriptsize'
printf, TEXLUN, '\begin{center}'
printf, TEXLUN, '\begin{longtable}{lccccccccccc}'
printf, TEXLUN, '\hline \hline\\'
printf, TEXLUN, 'Name & z & S$_{X0}$ & S$_{01}$ & r$_{c1}$ & $\beta_1$ & S$_{02}$ & r$_{c2}$ & $\beta_2$ & $\chi^2$ & D.O.F. & Prob.\\'
printf, TEXLUN, '\hline'

;# set-up plotting device
loadct, 13, /silent
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 3
!Y.THICK  = 3
!Z.THICK  = 3

;# start looping for each cluster
FOR i = 0,n_elements(obsids)-1 DO BEGIN
   multi = 'no'
   obsid = strcompress(obsids[i],/remove_all)
   name = strcompress(names[i],/remove_all)
   fname = name
   strreplace, fname, '_', ' '
   z = zs[i]
   tx = txs[i]
   datadir = strcompress(locs[i],/remove_all)
   cosmology, z, result, /silent
   as2kpc = result[4]
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

   ;# make color arrays
   IF z LE 0.05 THEN color = 50
   IF ((z LE 0.3) AND (z GT 0.05)) THEN color = 150
   IF z GT 0.3 THEN color = 250

   ;## read data
   fits   = mrdfits(sbr,1)
   rmid   = fits.rmid*0.492*as2kpc
   exp    = fits.exposure
   surbri = fits.sur_bri/exp/0.492^2.
   sbrerr = fits.sur_bri_err/exp/0.492^2.

   ;# adhoc filter
   ord    = where((rmid LT 1000.) AND (surbri GT 0.))
   rmid   = rmid[ord]
   surbri = surbri[ord]
   sbrerr = sbrerr[ord]
   sb0    = surbri[0]
   sb0err = sbrerr[0]

   ;# a = [s01, s02, rc1, rc2, beta1, beta2]
   ;# build the input parameter array
   parnames = ['S0','rc','beta']
   parinfo = replicate({value:0.D, $
                        fixed:0, $
                        limited:[1,1], $
                        limits:[0.D,0.D], $
                        step:[0.D], $
                        tied:['']}, n_elements(parnames))
   ;# initial guesses
   a = [surbri[0], $
        rmid[0]/2., $
        0.5D]
   ;# limits
   parinfo(0).limits  = [0.1*surbri[0],1.1*surbri[0]]           ;# S0
   parinfo(1).limits  = [0.D,max(rmid)]                         ;# rc
   parinfo(2).limits  = [0.D,5.D]                               ;# beta
   parinfo(*).value = a

   ;# run the fitter
   weights = 1./sbrerr^2.
   parinfo(*).value = a
   print, ''
   print, "## STATUS: Running mpcurvefit..."
   result = mpcurvefit(rmid, surbri, weights, a, sigma, FUNCTION_NAME='single_beta', $
                       ITMAX=5000, CHISQ=chisq, PARINFO=parinfo, STATUS=status, $
                       YERROR=residrms, ERRMSG=errmsg, DOF=dof, /DOUBLE, /QUIET, /NODERIVATIVE)
   IF status LT 0 THEN BEGIN
      message, errmsg
      GOTO, ERR
   ENDIF
   print, ''
   print, "## Fitting complete with no errors."
   print, '## Fit parameters:'

   ;# *If* you can assume that the true reduced chi-squared
   ;# value is unity -- meaning that the fit is implicitly
   ;# assumed to be of good quality -- then the estimated
   ;# parameter uncertainties can be computed by scaling SIGMA
   ;# by the measured chi-squared value.
   ord = where(sigma LE 0, num)
   IF num GT 0 THEN sigma[ord] = 0.1*a[ord]
   sigma = sigma*sqrt(chisq/dof)
   resid = surbri-result
   prob_est = 1.0-IGAMMA((0.5*n_elements(rmid)-3), 0.5*chisq)
   FOR jj=0,n_elements(sigma)-1 DO BEGIN
      print, format='(A-10,E10.3,A7,E10.3)',parnames[jj]+': ',a[jj],' +/- ',sigma[jj]
   ENDFOR
   print, format='(A-10,E10.3,A7,E10.3)','Residuals:',mean(resid),' +/- ',residrms
   print, format='(A-10,I10)','DOF:',dof
   print, format='(A-10,F10.3)','ChiSq:',chisq
   print, format='(A-10,F10.3)','RChiSq:',chisq/dof
   print, format='(A-10,F10.3)','Prob:',prob_est

   ;# plot the results
   xmin = 0.9*min(rmid)
   xmax = 1.1*max(rmid)
   ymin = 0.9*min(surbri)
   ymax = 1.1*max(surbri)
   set_plot, 'PS'
   printf, LISTLUN, obsid+'_betamod.ps'
   device, filename = obsid+'_betamod.ps', $
           /color, $
           /portrait, $
           /helvetica
   xran = [xmin,xmax]
   yran = [ymin,ymax]
   plot, xran, yran, $
         /nodata, $
         xran = xran, $
         yran = yran, $
         /xlog, /ylog, $
         /xsty, /ysty, $
         title  = fname+'     '+obsid, $
         xtitle = textoidl('R [kpc]'), $
         ytitle = textoidl('Surface Brightness [cts/sec/arcsec^2]'), $
         charsize = 0.8
   plotsym, 0, 0.6, /fill
   oplot, rmid, surbri, psym=8, color=color, thick=3
   oplot, rmid, surbri, psym=0, color=color, thick=3
   oploterror, rmid, surbri, sbrerr, psym=0, errcolor=color, color=color
   a = [a[0],0,a[1],0,a[2],0]
   sigma = [sigma[0],0,sigma[1],0,sigma[2],0]
   y = (a[0]*(1.+(rmid/a[2])^2.)^(0.5-3*a[4]))
   oplot, rmid, y, psym=0, color=250, thick=3, linestyle=2

   ;# draw the legend
   ;# a = [s01, s02, rc1, rc2, beta1, beta2]
   rchi = chisq/dof
   items = [textoidl('S_{0,1}: '+num2str(a[0])), $
            textoidl('r_{c,1}: '+num2str(a[2])), $
            textoidl('\beta_1: '+num2str(a[4])), $
            textoidl('S_{0,2}: '+num2str(a[1])), $
            textoidl('r_{c,2}: '+num2str(a[3])), $
            textoidl('\beta_2: '+num2str(a[5])), $
            textoidl('\chi_{red}^2: '+num2str(rchi)), $
            textoidl('Prob_{\PDF}: '+num2str(prob_est))]
;   items = [textoidl('S_{0}: '+num2str(a[0])), $
;            textoidl('r_{c}: '+num2str(a[2])), $
;            textoidl('\beta: '+num2str(a[4])), $
;            textoidl('\chi_{red}^2: '+num2str(chisq/dof,3)), $
;            textoidl('Prob_{\PDF}: '+num2str(prob_est))]
   linearr = replicate(-99,n_elements(items))
   psyarr = replicate(-99,n_elements(items))
   legend, items, linestyle=larr, psym=parr, box=0, charsize=0.8, /top, /right, /fill
   device, /close

   ;# print results to LaTeX file
   printf, OUTLUN, format='(A-20,A20,F8.4,E12.2,E12.2,E12.2,F12.3,F12.3,F12.3,F12.3,E12.2,E12.2,F12.3,F12.3,F12.3,F12.3,F10.2,I5,E12.2)', $
           name, obsid, z, surbri[0], a[0], sigma[0], a[2], sigma[2], a[4], sigma[4], a[1], sigma[1], a[3], sigma[3], a[5], sigma[5], chisq, dof, prob_est
   printf, TEXLUN, format='(A-20,A-10,F8.4,A7,E12.2,A7,E12.2,A7,E12.2,A7,E12.2,A7,F12.3,A7,F12.3,A7,F12.3,A7,F12.3,A7,E12.2,A7,E12.2,A7,F12.3,A7,F12.3,A7,F12.3,A7,F12.3,A7,F8.2,A3,I5,A3,E12.2,A2)', $
           fname, '\dotfill & ', z, ' & ', $
           sb0,  ' $\pm$', sb0err,   ' & ', $
           a[0], ' $\pm$', sigma[0], ' & ', $
           a[2], ' $\pm$', sigma[2], ' & ', $
           a[4], ' $\pm$', sigma[4], ' & ', $
           a[1], ' $\pm$', sigma[1], ' & ', $
           a[3], ' $\pm$', sigma[3], ' & ', $
           a[5], ' $\pm$', sigma[5], ' & ', $
           chisq, ' & ', dof, ' & ', prob_est, '\\'

err:
   IF count NE 1 THEN printf, LOGLUN, 'No ',sbr
ENDFOR

;# close out the LaTeX file
printf, TEXLUN, '\hline'
printf, TEXLUN, '\end{longtable}'
printf, TEXLUN, '\end{center}'
printf, TEXLUN, '\end{landscape}'
printf, TEXLUN, '\end{document}'
close, OUTLUN
close, TEXLUN
close, LISTLUN
close, LOGLUN
SPAWN, 'cat list | pscat '+numpp+' betamods.ps'
SPAWN, 'rm -f list'
SPAWN, 'rm -f *betamod.ps'
SPAWN, 'perl ~/research/redux/scripts/sorter.pl temp betamods.dat'
SPAWN, 'rm -f temp'
set_plot, 'X'
!quiet = 0

END
