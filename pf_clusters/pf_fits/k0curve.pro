PRO k0curve, dat1

ktype = 'flat'
model = 'nonzero'
rk = 100.

;# get values specific to model
IF (ktype EQ 'flat') THEN BEGIN
   IF (model EQ 'nonzero') THEN ind = 2 ELSE ind = 3
ENDIF
IF (ktype EQ 'itpl') THEN BEGIN
   IF (model EQ 'nonzero') THEN ind = 0 ELSE ind = 1
ENDIF

;# read in list of clusters to work with
restore,'~/research/redux/scripts/s_resultstemplate.sav'
readcol, dat1, FORMAT='A,A,F,F,F,F,F,F,F,F,F,A,F,A,F,A', comment='#', $
         clusters,obsids,xs,ys,rmaxs,minctss,zs,nhs,txs,fes,lbols,chips,eobss,diffs,robs,locs
prevname = 'kjsdflsjdk'
FOR i = 0,n_elements(obsids)-1 DO BEGIN
   obsid = strcompress(obsids[i],/remove_all)
   name = strcompress(clusters[i],/remove_all)
   IF name EQ prevname THEN GOTO,ERROR
   ord = where(clusters EQ name)
   IF n_elements(ord) EQ 1 THEN obsid = obsid ELSE BEGIN
      temp = strcompress(obsids[ord],/remove_all)
      obsid = strjoin(temp,'_',/single)
   ENDELSE

   ;# check for file existance
   file = '~/research/pf_clusters/pf_fits/s_results/'+obsid+'_results.log'
   check = findfile(file,count=count)
   IF (count NE 1) THEN GOTO,ERROR
   datafit = read_ascii(file, template = s_resultstemplate)

   ;# get central entropy value, either obs or fit
   r = maken(1d-6,1d6,1d4)
   k0 = datafit.k0[ind]
   k0err = datafit.k0err[ind]
   k100 = datafit.k100[ind]
   k100err = datafit.k100err[ind]
   a = datafit.plaw[ind]
   aerr = datafit.plawerr[ind]
   chi1 = datafit.chisq[2]
   chi2 = datafit.chisq[3]
   curv = (abs(100^(-a)*(a-1)*a*k100*rk^(a-1)))/(1.0+(100^(-a)*a*k100*rk^(a-1))^2.)^(3./2.)
   tcurv = (abs(100^(-a)*(a-1)*a*k100*r^(a-1)))/(1.0+(100^(-a)*a*k100*r^(a-1))^2.)^(3./2.)
   kt = total(tcurv)/total(r)
   h = k100
;   kr = k0+k100*(r/100.)^a
;   plot,r,kr/max(kr),/xlog,/ylog
;   oplot,r,tcurv/max(tcurv),linestyle=2
;   yesno = ''
;   read, '## INPUT: Press enter to continue', yesno

   ;# derivative of d(kappa)/d(alpha)
   numer = 100.^(-a)*h*r^a*(a^2.*h^2.*(2.^(4.*a+1.)*625.^a*alog10(100.)*a^2.-(1.d4^a+2.^(4.*a+1.)*625.^a*alog10(100.))*a+2.^(4.*a+1.)*625.^a)*r^(2.*a)+(-1.d8^a*alog10(100.)*a^2.+2.^(8*a+1.)*625.^(2.*a)*a+1.d8^a*alog10(100.)*a-1.d8^a)*r^2.+(a-1.)*a*(1.d8^a*r^2.-2.^(4.*a+1.)*625.^a*a^2.*h^2.*r^(2.*a))*alog10(r))
   denom = (a^2.*h^2.*r^(2.*a)+1.d4^a*r^2.)^2.*sqrt(100.^(-2.*a)*a^2.*h^2.*r^(2.*a-2.)+1.)
   dkda = total(numer/denom)/(n_elements(numer))

   ;# derivative of d(kappa)/d(K100)
   numer = 1.d4^a*(a-1.)*a*r^(a+1.)*(1.d4^a*r^2.-2.*a^2.*h^2.*r^(2.*a))
   denom = (a^2.*h^2.*r^(2.*a)+1.d4^a*r^2.)^(5./2.)
   dkdh = total(numer/denom)/(n_elements(numer))

   ;# calc total error
   kterr = (abs(dkda)*aerr^2.+abs(dkdh)*k100err^2.)/total(r)
   ulkt = 'n'
   ulk0 = 'n'
   IF kt-kterr LE 0. THEN BEGIN
      kt = kt+2.*kterr
      kterr = 0.
      ulkt = 'y'
   ENDIF
   IF ((k0-k0err LE 0.) OR (chi2 LE chi1)) THEN BEGIN
      k0 = k0+2.*k0err
      k0err = 0.
      ulk0 = 'y'
   ENDIF
   IF ulkt EQ 'y' THEN BEGIN
      push, y2ul, kt
      push, xul, k0
   ENDIF
   IF ulk0 EQ 'y' THEN BEGIN
      push, ux, k0
      push, uy, kt
   ENDIF
   push, x, k0
   push, xerr, k0err
   push, y1, curv
   push, y2, kt
   push, y2err, kterr
   push, allnames, name+' '+obsid+' '+num2str(k0)
   ord = WHERE(FINITE(tcurv, /NAN),count)
   if count GT 0 then begin
      print,name
      print,tcurv[ord]
   endif      

ERROR:
prevname = name
ENDFOR

;# interesting items
ord = where(y2 EQ max(y2))
print, 'The highest curvature is... ',allnames[ord]
ord = where(y2 EQ min(y2))
print, 'The lowest curvature is... ',allnames[ord]
print, '# K0 < 40'
ord = where(x LT 40.)
a = alog10(y2[ord])
amean = 10.^(mean(a)+0.5*(stddev(a)^2.))
dev = sqrt(amean^2.*10.^(stddev(a)^2.-1))
print, format='(A-30,E10.2,A4,E10.2)','Mean kappa_A:',amean,'+/-',dev
print, '# K0 < 40'
ord = where(x GE 40.)
a = alog10(y2[ord])
amean = 10.^(mean(a)+0.5*(stddev(a)^2.))
dev = sqrt(amean^2.*10.^(stddev(a)^2.-1))
print, format='(A-30,E10.2,A4,E10.2)','Mean kappa_A:',amean,'+/-',dev

;# plot everything
set_plot, 'PS'
device, $
   filename = 'curvk0.eps', $
   /encapsulated, $
   /portrait, $
   set_font='Times-Roman'
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 3
!Y.THICK  = 3
!Z.THICK  = 3
plotsym, 0, 0.5, /fill
xmin = 0.70*min(x)
xmax = 1.25*max(x)
ymin = 0.50*min(y2-y2err)
ymax = 1.50*max(y2+y2err)
ytex = textoidl('Average Curvature, \kappa_A')
xtex = textoidl('K_{0} [keV cm^{2}]')
plot, x, y2, $
      linestyle = 0, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax],$
      xtitle = xtex, $
      ytitle = ytex, $
      /xsty, /ysty, $
      /xlog, /ylog, $
      ytickformat='exponent',$
      psym = 8, $
      POSITION = ASPECT(1.0), $
      charsize = 1.0
oploterror, x, y2, xerr, y2err, psym=8, /nohat
oplot, ux, uy, psym=8
;plotsym, 6, 2.0, /fill
;oplot, ux, uy, psym=8
;plotsym, 1, 2.0, /fill
;oplot, xul, y2ul, psym=8
device,/close
set_plot, "X"

END
