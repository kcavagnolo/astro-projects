PRO t0, dat1

;# setup the fancy plotting options
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 3
!Y.THICK  = 3
!Z.THICK  = 3

;# read in list of clusters to work with
myhome = GETENV('HOME')
restore, myhome+'/research/redux/scripts/s_resultstemplate.sav'
restore, myhome+'/research/redux/scripts/tcooltemplate.sav'
readcol, dat1, FORMAT='A,A,F,F,F,F,F,F,F,F,F,A,F,A,F,A', comment='#', $
         cluster,obsids,x,y,rmax,mincts,zs,nhs,txs,fes,lbols,chips,eobss,diffs,robs,loc
OPENW, T0, 'kmm_t0.in', /get_lun
OPENW, RES, 't0_fits.dat', /get_lun
printf, RES, format='(A-20,A35,A8,A8,A8,A6,8A15)',$
        '#Cluster','Obsid','Tmode','Rmin','Rmax','Ann','t0',$
        't0_err','t100','t100_err','Power-law','Plaw_err','Chisq','Prob'

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
   file1 = myhome+'/research/pf_clusters/pf_fits/s_results/'+obsid+'_results.log'
   check = findfile(file1,count=count)
   IF (count NE 1) THEN GOTO,ERROR
   file2 = myhome+'/research/pf_clusters/pf_fits/tables/'+obsid+'_tcool.dat'
   check = findfile(file2,count=count3)
   IF (count3 NE 1) THEN GOTO,ERROR

   ;# read in data
   data = read_ascii(file1, template = s_resultstemplate)
   rmin = data.rmin[0]
   rmax = data.rmax[0]
   datacool = read_ascii(file2, template = tcooltemplate)
   rin = datacool.rin
   rout = datacool.rout
   rmean = 0.5*(rin+rout)
   obscool = datacool.tc32
   obscoolerr = datacool.tc32err
   wfit = where((rmean GE rmin) * (rmean LE rmax))

;;    ;# plot some stuff
;;    xmin = 0.9*min(rmean)
;;    xmax = 1.1*max(rmean)
;;    ymax = 1.1*max(obscool)
;;    ymin = 0.9*min(obscool)
;;    plot, rmean, obscool, $
;;          title  = title, $
;;          xtitle = textoidl('R_{mean} [Mpc]'), $
;;          ytitle = textoidl('Cooling Time [Gyr]'), $
;;          /xlog, /ylog, $
;;          /xsty, /ysty, $
;;          xran = [xmin,xmax], $
;;          yran = [ymin,ymax], $
;;          psym = 0
;;    plotsym, 0, 0.8, /fill
;;    oplot, rmean, obscool, psym=8
;;    oploterror, rmean, obscool, obscoolerr

   ;# fit profile using Markwardt's Levenburg-Marquardt non-linear
   ;# least squares algorithm
   a = [10., 150.0, 1.5]
   weights = 1./obscoolerr[wfit]^2.
   parinfo = replicate({value:0.D, $
                        fixed:0, $
                        limited:[0,0], $
                        limits:[0.D,0.D], $
                        step:[0.D], $
                        tied:['']}, n_elements(a))
   parinfo(0).limited(0) = 1
   parinfo(1).limited(0) = 1
   parinfo(*).value = a
   result = mpcurvefit(rmean[wfit], obscool[wfit], weights, a, sigma, $
                       FUNCTION_NAME='fit_form_100', $
                       ITMAX = 100, $
                       CHISQ = chisq, $
                       PARINFO = parinfo, $
                       STATUS = status, $
                       ERRMSG = errmsg, $
                       DOF = dof, $
                       /AUTODERIVATIVE, $
                       /QUIET)
   prob_est = 1.0-IGAMMA((0.5*n_elements(rmean[wfit])-3), 0.5*chisq)

   ;# run some checks
   nain = FINITE(a[0])
   IF ((a[0] LE 0.0) OR (a[0]-sigma[0] LE 0.) OR (a[0]*1000. GT 1d5) OR (nain NE 1)) THEN BEGIN
      print, name,' ',obsid
      plot, rmean, obscool
      oplot, rmean[wfit], result, linestyle=2
      print,a
      print, sigma
      IF a[0] LE 0. THEN BEGIN
         a[0] = result[0]
         sigma[0] = 0.1*a[0]
      ENDIF
   ENDIF
   push, tcall, a[0]
   printf, T0, alog10(a[0])
   wann = n_elements(result)
   printf, RES, format='(A-20,A35,A8,F8.2,F8.2,I6,7F15.5,E15.5)',$
           name,obsid,'flat',rmin,rmax,wann,a[0],sigma[0],a[1],$
           sigma[1],a[2],sigma[2],chisq/dof,prob_est
ERROR:
ENDFOR
FREE_LUN, T0
FREE_LUN, RES

;# make bins for obscool cum prof
tcall = tcall * 1000.
obsmin = min(tcall)
obsmax = max(tcall)
obsiter = obsmin/2.
total = float(n_elements(tcall))
print, '## STATUS: Working on Tcall cumulative profile...'
WHILE obsiter LE obsmax DO BEGIN
   num = float(n_elements(where(tcall LE obsiter)))
   push, obscumpr, num/total
   push, obsx, obsiter
   obsiter = obsiter + 1
ENDWHILE
print, 'Done.'
push, obsx, obsmax
push, obscumpr, 1.0
obsxmin = 0.8*min(obsx)
obsxmax = 1.2*max(obsx)
obsymin = 0.8*min(obscumpr)
obsymax = 1.1
push, obsx, obsxmax
push, obscumpr, 1.0

;# histogram of tcall dist.
set_plot, 'PS'
device, filename='t0.eps', $
        /color, $
        /encapsulated, $
        /portrait, $
        /helvetica
multiplot,[1,2], $
          mxtitle=textoidl('t_{c0} [Myr]')
obsxmin=6.
obsxmax=60000.
histoplot, tcall, 0.20, $
           /log, $
           /xsty, $
           ;/fraction, $
           xrange = [obsxmin,obsxmax], $
           ytitle = textoidl('Number of clusters'), $
           charsize= 0.8
ytex = textoidl('Fractional number of clusters')
multiplot
plot, obsx, obscumpr, $
      linestyle = 0, $
      xrange = [obsxmin,obsxmax], $
      yrange = [obsymin,obsymax],$
      ytitle = ytex, $
      /xsty, /ysty, $
      /xlog, $
      charsize = 0.8, $
      psym = 10
device, /close
multiplot,/reset

END
