PRO mckmm, in

myhome = GETENV('HOME')
ktype  = 'flat'
model  = 'nonzero'

restore,myhome+'/research/redux/scripts/s_resultstemplate.sav'
readcol, in, FORMAT='A,A,F,F,F,F,F,F,F,F,F,A,F,A,F,A', comment='#', $
         cluster,obsids,x,y,rmax,mincts,zs,nhs,txs,fes,lbols,chips,eobss,diffs,robs,loc
IF (ktype EQ 'flat') THEN BEGIN
   IF (model EQ 'nonzero') THEN ind = 2 ELSE ind = 3
ENDIF
IF (ktype EQ 'itpl') THEN BEGIN
   IF (model EQ 'nonzero') THEN ind = 0 ELSE ind = 1
ENDIF

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
   file = myhome+'/research/pf_clusters/pf_fits/s_results/'+obsid+'_results.log'
   check = findfile(file,count=count)
   IF (count NE 1) THEN GOTO,ERROR
   datafit = read_ascii(file, template = s_resultstemplate)
   IF (ktype EQ 'itpl') THEN BEGIN
      chi1 = datafit.chisq[0]
      chi2 = datafit.chisq[1]
   ENDIF ELSE BEGIN
      chi1 = datafit.chisq[2]
      chi2 = datafit.chisq[3]
   ENDELSE
   push, k0, datafit.k0[ind]
ERROR:
ENDFOR

i=0
WHILE i LT 1000 DO BEGIN
   OPENW, LOG, '/tmp/junk'+num2str(i)+'.in', /GET_LUN
   num = n_elements(k0)
   a = randomind(num,20)
   temp = k0[a]
   FOR j=0, n_elements(temp)-1 DO BEGIN
      printf, LOG, format='(F10.3)', temp[j]
   ENDFOR
   FREE_LUN, LOG
   i++
ENDWHILE
END
