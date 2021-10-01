PRO mckr, infile, nmc, silent=silent

myhome  = GETENV('HOME')
restore, myhome+"/research/redux/scripts/s_tabletemplate.sav"
restore, myhome+"/research/redux/scripts/s_resultstemplate.sav"
readcol, infile, FORMAT='A,A,F,F,F,F,F,F,F,F,F,A,F,A,F,A', comment='#', $
         cluster, obsids, xs, ys, rmaxs, minctss, zs, nhs,$
         txs, fes, lbols, chips, eobss, diffs, robss, locs

OPENW, RESLUN, 'mckr.log', /GET_LUN
printf, RESLUN, format='(A-20,A20,A6,A8,A8,A8,A6,8A15,A10)',$
        '#Cluster','Obsid','NMC','Tmode','Rmin','Rmax','Ann','K0',$
        'K0_err','K100','K100_err','Power-law','Plaw_err','Chisq','Prob','DOF'

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

   ;# read in data
   obs   = read_ascii(file1, template = s_tabletemplate)
   fit   = read_ascii(file2, template = s_resultstemplate)
   kr    = obs.k_flat
   kerr  = obs.k_err
   rmin  = fit.rmin[2]
   rmax  = fit.rmax[2]
   rin   = obs.rin_mpc
   rout  = obs.rout_mpc
   rmean = 0.5*(rin+rout)
   wfit  = where((rmean GE rmin) * (rmean LE rmax))
   kr    = kr[wfit]
   kerr  = kerr[wfit]
   rmean = rmean[wfit]
   ocdof  = fit.dof[2]
   occhi  = fit.chisq[2]*ocdof
   ozdof  = fit.dof[3]
   ozchi  = fit.chisq[3]*ozdof
   ok0   = fit.k0[2]
   maxindex = n_elements(rmean)-1

   ;# monte carlo the kr and fit
   print, ''
   print, '## STATUS: Performing '+num2str(nmc)+' Monte Carlo simulations...'
   k_array = fltarr(nmc,maxindex+1)
   k_mean  = fltarr(maxindex+1)
   k_error = fltarr(maxindex+1)
   wnam  = strcompress(name,/remove_all)
   wobs  = strcompress(obsid,/remove_all)
   wrmin = strcompress(sigfig(rmin,2),/remove_all)
   wrmax = strcompress(sigfig(rmax,2),/remove_all)
   wann  = strcompress(n_elements(wfit),/remove_all)
   tmode = 'flat'
   void, ck0
   void, ckerr
   void, ck100
   void, ck100err
   void, ca
   void, caerr
   void, cchi
   void, cprob
   void, cdof
   void, zk0
   void, zkerr
   void, zk100
   void, zk100err
   void, za
   void, zaerr
   void, zchi
   void, zprob
   void, zdof
   FOR imc = 0,nmc-1 DO BEGIN
      ranarr = randomn(k,maxindex+1,/double)   
      mckr = kr + (kerr*ranarr)
      k_array[imc,*] = mckr

      ;# k0!=0
      a = [10., 150.0, 1.5]
      weights = 1./kerr^2.
      parinfo = replicate({value:0.D, $
                           fixed:0, $
                           limited:[0,0], $
                           limits:[0.D,0.D], $
                           step:[0.D], $
                           tied:['']}, n_elements(a))
      parinfo(0).limited(0) = 1
      parinfo(1).limited(0) = 1
      parinfo(*).value = a
      result = mpcurvefit(rmean, mckr, weights, a, sigma_entropy, $
                          FUNCTION_NAME='fit_form_100', $
                          ITMAX = 100, $
                          CHISQ = chisq_entropy, $
                          PARINFO = parinfo, $
                          STATUS = status, $
                          ERRMSG = errmsg, $
                          DOF = dof, $
                          /AUTODERIVATIVE, $
                          /QUIET)
      prob_est = 1.0-IGAMMA((0.5*n_elements(rmean)-3), 0.5*chisq_entropy)
      dof1 = dof
      IF NOT keyword_set(silent) THEN BEGIN
         print, '## K0 != 0'
         print, format='(A10,A10,A10,A10,A10,A10,A10)', 'K0','K0err','K100','Plaw','Chisq', 'DOF', 'RedChisq'
         print, format='(F10.3,F10.3,F10.3,F10.3,F10.3,I10,F10.3)', a[0],sigma_entropy[0],a[1],a[2],chisq_entropy, dof, chisq_entropy/dof
      ENDIF
      push, ck0, a[0]
      push, ckerr, sigma_entropy[0]
      push, ck100, a[1]
      push, ck100err, sigma_entropy[1]
      push, ca, a[2]
      push, caerr, sigma_entropy[2]
      push, cchi, chisq_entropy
      push, cprob, prob_est
      push, cdof, dof

      ;# k0=0
      a = [0.0, a[1], a[2]]
      weights = 1./kerr^2.
      parinfo = replicate({value:0.D, $
                           fixed:0, $
                           limited:[0,0], $
                           limits:[0.D,0.D], $
                           step:[0.D], $
                           tied:['']}, n_elements(a))
      parinfo(0).fixed = 1
      parinfo(*).value = a
      parinfo(1).limited(0) = 1
      result_s0 = mpcurvefit(rmean, mckr, weights, a, sigma_entropy, $
                             FUNCTION_NAME='fit_form_100', $
                             ITMAX = 100, $
                             CHISQ = chisq_entropy, $
                             PARINFO = parinfo, $
                             STATUS = status, $
                             ERRMSG = errmsg, $
                             DOF = dof, $
                             /AUTODERIVATIVE, $
                             /QUIET)
      prob_est = 1.0-IGAMMA((0.5*n_elements(rmean)-3), 0.5*chisq_entropy)
      dof2 = dof
      IF NOT keyword_set(silent) THEN BEGIN
         print, '## K0 = 0'
         print, format='(A10,A10,A10,A10,A10,A10,A10)', 'K0','K0err','K100','Plaw','Chisq', 'DOF', 'RedChisq'
         print, format='(F10.3,F10.3,F10.3,F10.3,F10.3,I10,F10.3)', a[0],sigma_entropy[0],a[1],a[2],chisq_entropy, dof, chisq_entropy/dof
      ENDIF
      push, zk0, a[0]
      push, zkerr, sigma_entropy[0]
      push, zk100, a[1]
      push, zk100err, sigma_entropy[1]
      push, za, a[2]
      push, zaerr, sigma_entropy[2]
      push, zchi, chisq_entropy
      push, zprob, prob_est
      push, zdof, dof
   ENDFOR
   n = float(n_elements(where(cchi GE occhi)))
   t = float(n_elements(cchi))
   print, 'K0=c p-value: ',n/t
   n = float(n_elements(where(zchi GE ozchi)))
   t = float(n_elements(zchi))
   print, 'K0=0 p-value: ',n/t
   printf, RESLUN, format='(A-20,A20,I6,A8,A8,A8,A6,7F15.5,E15.5,I10)',$
           wnam,wobs,nmc,tmode,wrmin,wrmax,wann,mean(ck0),stddev(ck0),mean(ck100),$
           stddev(ck100),mean(ca),stddev(ca),mean(cchi),mean(cprob),mean(cdof)
   printf, RESLUN, format='(A-20,A20,I6,A8,A8,A8,A6,7F15.5,E15.5,I10)',$
           wnam,wobs,nmc,tmode,wrmin,wrmax,wann,mean(zk0),stddev(zk0),mean(zk100),$
           stddev(zk100),mean(za),stddev(za),mean(zchi),mean(zprob),mean(zdof)
   ERROR:
ENDFOR
FREE_LUN, RESLUN

;FOR jmc = 0,maxindex DO BEGIN
;   k_mean[jmc]  = mean(k_array[*,jmc])
;   k_error[jmc] = stddev(k_array[*,jmc])
;ENDFOR
;kmcerr = 0.5*ck0/k_mean*k_error

END
