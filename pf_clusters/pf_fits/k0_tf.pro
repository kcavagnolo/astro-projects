PRO k0_tf

k0val = 'fit'
ktype = 'flat'
model = 'nonzero'
myhome = GETENV('HOME')

restore,myhome+'/research/redux/scripts/xspectemp_rin_normerr_src.sav'
restore,myhome+'/research/redux/scripts/s_tabletemplate.sav'
restore,myhome+'/research/redux/scripts/s_resultstemplate.sav'

r25_77 = myhome+'/research/me_temp_proj/me_fits/dat/culled_r2500-50_7-7.dat'
r25_27 = myhome+'/research/me_temp_proj/me_fits/dat/culled_r2500-50_2-7.dat'
r50_77 = myhome+'/research/me_temp_proj/me_fits/dat/culled_r5000-50_7-7.dat'
r50_27 = myhome+'/research/me_temp_proj/me_fits/dat/culled_r5000-50_2-7.dat'
r25f = read_ascii(r25_77, template = xspectemp_rin_normerr_src)
r25h = read_ascii(r25_27, template = xspectemp_rin_normerr_src)
check = r25h.obsid/r25f.obsid
uhoh = where(check NE 1)
IF uhoh NE -1 THEN BEGIN
   print,'Uh oh... out of order file'
   exit
ENDIF
r50f = read_ascii(r50_77, template = xspectemp_rin_normerr_src)
r50h = read_ascii(r50_27, template = xspectemp_rin_normerr_src)
check = r50h.obsid/r50f.obsid
uhoh = where(check NE 1)
IF uhoh NE -1 THEN BEGIN
   print,'Uh oh... out of order file'
   exit
ENDIF
t77 = r25f.tx
t27 = r25h.tx
flo = r25f.tx - r25f.tlo
fhi = r25f.thi - r25f.tx
hlo = r25h.tx - r25h.tlo
hhi = r25h.thi - r25h.tx
r25tf   = t27/t77
r25tfhi = r25tf*(sqrt((hhi/t27)^2.+(fhi/t77)^2.))
r25tflo = r25tf*(sqrt((hlo/t27)^2.+(flo/t77)^2.))
r25names = r25f.cluster

t77 = r50f.tx
t27 = r50h.tx
flo = r50f.tx - r50f.tlo
fhi = r50f.thi - r50f.tx
hlo = r50h.tx - r50h.tlo
hhi = r50h.thi - r50h.tx
r50tf   = t27/t77
r50tfhi = r50tf*(sqrt((hhi/t27)^2.+(fhi/t77)^2.))
r50tflo = r50tf*(sqrt((hlo/t27)^2.+(flo/t77)^2.))
r50names = r50f.cluster

readcol, 'done.list', FORMAT='A,A,F,F,F,F,F,F,F,F,F,A,F,A,F,A', comment='#', $
         cluster, obsids, x, y, rmax, mincts, z, nh, tx, fe, lbol, chip, eobs, diff, robs, loc
IF (ktype EQ 'flat') THEN BEGIN
   IF (model EQ 'nonzero') THEN ind = 2 ELSE ind = 3
ENDIF
IF (ktype EQ 'itpl') THEN BEGIN
   IF (model EQ 'nonzero') THEN ind = 0 ELSE ind = 1
ENDIF

FOR i = 0,n_elements(obsids)-1 DO BEGIN
   name = strcompress(cluster[i],/remove_all)
   obs = strcompress(obsids[i],/remove_all)
   ord = where(cluster EQ name)
   IF n_elements(ord) NE 1 THEN BEGIN
      temp = obsids[ord]
      FOR j=1,n_elements(temp)-1 DO BEGIN
         obs = obs+'_'+strcompress(temp[j],/remove_all)
      ENDFOR
   ENDIF
   file = '~/research/pf_clusters/pf_fits/s_results/'+obs+'_results.log'
   check = findfile(file,count=count)
   IF (count NE 1) THEN GOTO,SKIP
   fitdata = read_ascii(file, template = s_resultstemplate)
   k = fitdata.k0
   kerr = fitdata.k0err
   ord1 = where(r25names EQ name)
   ord2 = where(r50names EQ name)
   IF ord1[0] NE -1 THEN BEGIN
      push, alltf1, r25tf[ord1]
      push, alltf1hi, r25tfhi[ord1]
      push, alltf1lo, r25tflo[ord1]
;      print, FORMAT='(A-20,F10.2,A20,F10.2)', name, k[ind], r25names[ord1], r25tf[ord1]
      push, allk1, k[ind]
      push, allk1lo, kerr[ind]
      push, allk1hi, kerr[ind]
   ENDIF ELSE IF ord2[0] NE -1 THEN BEGIN
      push, alltf2, r50tf[ord2]
      push, alltf2hi, r50tfhi[ord2]
      push, alltf2lo, r50tflo[ord2]
;      print, FORMAT='(A-20,F10.2,A20,F10.2)', name, k[ind], r50names[ord2], r50tf[ord2]
      push, allk2, k[ind]
      push, allk2lo, kerr[ind]
      push, allk2hi, kerr[ind]
   ENDIF ELSE GOTO, SKIP
SKIP:
ENDFOR

;# set up empty plot
x = [allk1,allk2]
xerr = [allk1lo,allk2lo]
y = [alltf1,alltf2]
ylo = [alltf1lo,alltf2lo]
yhi = [alltf1hi,alltf2hi]
ytex = textoidl('T_{HBR}')
xtex = textoidl('K_0 [keV cm^{2}]')
xmin = 0.8*min(x)
xmax = 1.1*max(x)
ymin = 0.9*min(y)
ymax = 1.1*max(y)
set_plot, 'PS'
device, filename = 'tfk0.eps', $
        /encapsulated, $
        /helvetica
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 3
!Y.THICK  = 3
!Z.THICK  = 3
plot, x, y, $
      /nodata, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      xtitle = xtex, $
      ytitle = ytex, $
      /xsty, /ysty, $
      /xlog, $
      charsize = 0.8
plotsym, 0, 0.7, /fill
oplot, allk1, alltf1, psym=8
oploterror, allk1, alltf1, allk1lo, alltf1lo, psym=8, /nohat, /lobar
oploterror, allk1, alltf1, allk1hi, alltf1hi, psym=8, /nohat, /hibar
plotsym, 3, 0.8
oplot, allk2, alltf2, psym=8
oploterror, allk2, alltf2, allk2lo, alltf2lo, psym=8, /nohat, /lobar
oploterror, allk2, alltf2, allk2hi, alltf2hi, psym=8, /nohat, /hibar
device,/close

;# quick stats for k0 < 50
FOR i=0,n_elements(x)-1 DO $
   IF ylo[i] GT yhi[i] THEN push, sig, ylo[i] ELSE push, sig, yhi[i] 
xw = 1./(xerr^2.)
yw = 1./(sig^2.)
ord = where(x LE 55.)
xmean = wtd_mean(x[ord],xw[ord])
ymean = wtd_mean(y[ord],yw[ord])
xsd = sqrt((total(xw[ord]*x[ord]^2.)*total(xw[ord])-(total(xw[ord]*x[ord]))^2.)/((total(xw[ord]))^2.-total(xw[ord]^2.)))
ysd = sqrt((total(yw[ord]*y[ord]^2.)*total(yw[ord])-(total(yw[ord]*y[ord]))^2.)/((total(yw[ord]))^2.-total(yw[ord]^2.)))
;xsd = sqrt((total(xw[ord]*(x[ord]-xmean)^2.))/((n_elements(xw[ord])-1)*total(xw[ord])/n_elements(xw[ord])))
;ysd = sqrt((total(yw[ord]*(y[ord]-ymean)^2.))/((n_elements(yw[ord])-1)*total(yw[ord])/n_elements(yw[ord])))
xsdom = 1./sqrt(total(xw[ord]))
ysdom = 1./sqrt(total(yw[ord]))
print, '## K0 < 55'
print, format='(A-10,F10.3,A5,F8.3,F8.3)','K0:',xmean,'+/-',xsd,xsdom
print, format='(A-10,F10.3,A5,F8.3,F8.3)','THBR:',ymean,'+/-',ysd,ysdom
print, ''
ord = where(x GT 55.)
xmean = wtd_mean(x[ord],xw[ord])
ymean = wtd_mean(y[ord],yw[ord])
xsd = sqrt((total(xw[ord]*x[ord]^2.)*total(xw[ord])-(total(xw[ord]*x[ord]))^2.)/((total(xw[ord]))^2.-total(xw[ord]^2.)))
ysd = sqrt((total(yw[ord]*y[ord]^2.)*total(yw[ord])-(total(yw[ord]*y[ord]))^2.)/((total(yw[ord]))^2.-total(yw[ord]^2.)))
;xsd = sqrt((total(xw[ord]*(x[ord]-xmean)^2.))/((n_elements(xw[ord])-1)*total(xw[ord])/n_elements(xw[ord])))
;ysd = sqrt((total(yw[ord]*(y[ord]-ymean)^2.))/((n_elements(yw[ord])-1)*total(yw[ord])/n_elements(yw[ord])))
xsdom = 1./sqrt(total(xw[ord]))
ysdom = 1./sqrt(total(yw[ord]))
print, '## K0 > 55'
print, format='(A-10,F10.3,A5,F8.3,F8.3)','K0:',xmean,'+/-',xsd,xsdom
print, format='(A-10,F10.3,A5,F8.3,F8.3)','THBR:',ymean,'+/-',ysd,ysdom

END
