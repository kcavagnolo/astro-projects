PRO get_dof

myhome = GETENV('HOME')
restore,myhome+'/research/redux/scripts/s_resultstemplate.sav'
restore,myhome+'/research/redux/scripts/s_tabletemplate.sav'
readcol,'junk',FORMAT='A,A,F,F,F,F,F,F,F,F,F,A,F,A,F,A', comment='#', $
        cluster,obsids,x,y,rmax,mincts,z,nh,tx,fe,lbol,chip,eobs,diff,robs,loc

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
   dataobs = read_ascii(file1, template = s_tabletemplate)
   datafit = read_ascii(file2, template = s_resultstemplate)
   kr = dataobs.k_flat
   kerr = dataobs.k_err
   rmin = datafit.rmin[2]
   rmax = datafit.rmax[2]
   rin = dataobs.rin_mpc
   rout = dataobs.rout_mpc
   rmean = 0.5*(rin+rout)
   wfit = where((rmean GE rmin) * (rmean LE rmax))

   ;# k0!=0
   a = [10., 150.0, 1.5]
   weights = 1./kerr[wfit]^2.
   parinfo = replicate({value:0.D, $
                        fixed:0, $
                        limited:[0,0], $
                        limits:[0.D,0.D], $
                        step:[0.D], $
                        tied:['']}, n_elements(a))
   parinfo(0).limited(0) = 1
   parinfo(1).limited(0) = 1
   parinfo(*).value = a
   result = mpcurvefit(rmean[wfit], kr[wfit], weights, a, sigma_entropy, $
                       FUNCTION_NAME='fit_form_100', $
                       ITMAX = 100, $
                       CHISQ = chisq_entropy, $
                       PARINFO = parinfo, $
                       STATUS = status, $
                       ERRMSG = errmsg, $
                       DOF = dof, $
                       /AUTODERIVATIVE, $
                       /QUIET)
   prob_est = 1.0-IGAMMA((0.5*n_elements(rmean[wfit])-3), 0.5*chisq_entropy)
   dof1 = dof

   ;# k0=0
   a = [0.0, a[1], a[2]]
   weights = 1./kerr[wfit]^2.
   parinfo = replicate({value:0.D, $
                        fixed:0, $
                        limited:[0,0], $
                        limits:[0.D,0.D], $
                        step:[0.D], $
                        tied:['']}, n_elements(a))
   parinfo(0).fixed = 1
   parinfo(*).value = a
   parinfo(1).limited(0) = 1
   result_s0 = mpcurvefit(rmean[wfit], kr[wfit], weights, a, sigma_entropy, $
                          FUNCTION_NAME='fit_form_100', $
                          ITMAX = 100, $
                          CHISQ = chisq_entropy, $
                          PARINFO = parinfo, $
                          STATUS = status, $
                          ERRMSG = errmsg, $
                          DOF = dof, $
                          /AUTODERIVATIVE, $
                          /QUIET)
   prob_est = 1.0-IGAMMA((0.5*n_elements(rmean[wfit])-3), 0.5*chisq_entropy)
   dof2 = dof

   GET_LUN, OUT
   OPENW, OUT, obsid+'_dof.dat'
   printf, OUT, format='(A5)', 'DOF'
   printf, OUT, format='(I5)', dof1
   printf, OUT, format='(I5)', dof2
   printf, OUT, format='(I5)', dof1
   printf, OUT, format='(I5)', dof2
   FREE_LUN, OUT

ERROR:
ENDFOR
END
